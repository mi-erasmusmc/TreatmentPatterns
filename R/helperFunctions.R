#' loadRenderTranslateSql
#'
#' Load and translate SQL file to desired dialect.
#'
#' @param sql
#'     SQL statement
#' @param oracleTempSchema
#'     Temporary Oracle schema
#' @param dbms
#'     DBMS
#' @param warnOnMissingParameters
#'     Warning message for missing parameters
#' @param output
#'     Output
#' @param outputFile
#'     Output file
#' @param ...
#'     Other parameters
#'
#' @return renderedSql
#'     renderSql object
loadRenderTranslateSql <- function(
    sql,
    oracleTempSchema = NULL,
    dbms = "postgresql",
    warnOnMissingParameters = TRUE,
    output = FALSE,
    outputFile,
    ...) {
  if (grepl('.sql', sql)) {
    pathToSql <- sql
    parameterizedSql <- readChar(pathToSql, file.info(pathToSql)$size)[1]
  } else {
    parameterizedSql <- sql
  }

  renderedSql <- SqlRender::render(
    sql = parameterizedSql,
    warnOnMissingParameters = warnOnMissingParameters,
    ...)

  renderedSql <- SqlRender::translate(
    sql = renderedSql,
    targetDialect = dbms,
    oracleTempSchema = oracleTempSchema)

  if (output == TRUE) {
    SqlRender::writeSql(renderedSql, outputFile)
    writeLines(paste("Created file '", outputFile, "'", sep = ""))
  }
  return(renderedSql)
}


#' extractFile
#' 
#' Extract table with specific name from database server.
#'
#' @param connection
#'     Connection object
#' @param tableName
#'     Name of table
#' @param resultsSchema
#'     Schema of results
#' @param dbms
#'     DBMS
#'
#' @return data
#'     data.frame 
#' @export
extractFile <- function(connection, tableName, resultsSchema, dbms) {
  parameterizedSql <- "SELECT * FROM @resultsSchema.@tableName"
  renderedSql <- SqlRender::render(
    parameterizedSql,
    resultsSchema = resultsSchema,
    tableName = tableName)

  translatedSql <- SqlRender::translate(
    renderedSql,
    targetDialect = dbms)
  data <- DatabaseConnector::querySql(connection, translatedSql)
}


#' stripname
#' 
#' Recursive function to remove name from all levels of list.
#' 
#' @param x input list
#' @param name the name of the list item from which the names will be removed
#'
#' @return list with removed names
#' 
#' @examples
#' \dontrun{
#' TreatmentPatterns:::stripname("base")
#' }
stripname <- function(x, name) {
  # Assertions
  checkmate::assertTRUE(!is.null(x))
  checkmate::assertCharacter(x = name, len = 1, null.ok = FALSE)
  
  thisdepth <- depth(x)
  if (thisdepth == 0) {
    return(x)
  } else if (length(nameIndex <- which(names(x) == name)) > 0) {
    element <- names(x)[nameIndex]
    x[[element]] <- unname(x[[element]])
  }
  return(lapply(x, stripname, name))
}


#' depth
#'
#' Function to find depth of a list element.
#'
#' @param x input list (element)
#' @param thisdepth current list depth
#'
#' @return the depth of the list element
#' 
#' @examples
#' \dontrun{
#' TreatmentPatterns:::depth(list("a"))
#' }
depth <- function(x, thisdepth = 0) {
  # Assertions
  checkmate::assertTRUE(!is.null(x))
  checkmate::assertNumeric(x = thisdepth, len = 1, lower = 0, null.ok = FALSE)
  
  if (!is.list(x)) {
    return(thisdepth)
  } else {
    return(max(unlist(
      lapply(x, depth, thisdepth = thisdepth + 1)
    )))
  }
}


#' getCohortCounts
#'
#' Count the cohorts specified by cohortIds (empty = all).
#'
#' @param connection
#'     Connection object
#' @param cohortDatabaseSchema
#'     Cohort database schema
#' @param cohortTable
#'     Cohort table
#' @param cohortIds
#'     Cohort IDs
#'
#' @return counts
#'     Number of cohort IDs
getCohortCounts <- function(connection = NULL,
                            cohortDatabaseSchema,
                            cohortTable,
                            cohortIds = c()) {
  sql <- loadRenderTranslateSql(
      sql = paste0(
        system.file(package = "TreatmentPatterns"),
        "/SQL/CohortCounts.sql"
      ),
      dbms = connection@dbms,
      resultsSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      cohortIds = cohortIds
    )
  counts <- DatabaseConnector::querySql(
    connection,
    sql,
    snakeCaseToCamelCase = TRUE)
  return(counts)
}


#' getCohortCharacteristics
#'
#' Borrowed function from CohortDiagnostics.
#'
#' @param connectionDetails
#'     Connection details object
#' @param connection
#'     Connection object
#' @param cdmDatabaseSchema
#'     CDM database schema
#' @param cohortDatabaseSchema
#'     Cohort database schema
#' @param cohortTable
#'     Cohort table
#' @param cohortIds
#'     Cohort IDs 
#' @param cdmVersion
#'     CDM version
#' @param covariateSettings
#'     Covariate settings
#' @param batchSize 
#'     Batch size
#'
#' @return results object
getCohortCharacteristics <- function(
    connectionDetails = NULL,
    connection = NULL,
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    cohortTable = "cohort",
    cohortIds,
    cdmVersion = 5,
    covariateSettings,
    batchSize = 100) {
  startTime <- Sys.time()

  results <- Andromeda::andromeda()
  for (start in seq(1, length(cohortIds), by = batchSize)) {
    end <- min(start + batchSize - 1, length(cohortIds))
    if (length(cohortIds) > batchSize) {
      ParallelLogger::logInfo(sprintf(
        "Batch characterization. Processing cohorts %s through %s",
        start,
        end
      ))
    }
    featureExtractionOutput <- FeatureExtraction::getDbCovariateData(
        connection = connection,
        oracleTempSchema = NULL,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cdmVersion = cdmVersion,
        cohortTable = cohortTable,
        cohortId = cohortIds[start:end],
        covariateSettings = covariateSettings,
        aggregated = TRUE
      )
    
    populationSize <- attr(
      x = featureExtractionOutput, 
      which = "metaData")$populationSize

    populationSize <- dplyr::tibble(
      cohortId = names(populationSize),
      populationSize = populationSize)
    
    if (!"analysisRef" %in% names(results)) {
      results$analysisRef <- featureExtractionOutput$analysisRef
    }
    if (!"covariateRef" %in% names(results)) {
      results$covariateRef <- featureExtractionOutput$covariateRef
    } else {
      covariateIds <- results$covariateRef %>%
        dplyr::select(.data$covariateId)
      Andromeda::appendToTable(
        results$covariateRef,
        featureExtractionOutput$covariateRef %>%
          dplyr::anti_join(covariateIds, by = "covariateId", copy = TRUE)
      )
    }
    if ("timeRef" %in% names(featureExtractionOutput) &&
        !"timeRef" %in% names(results)) {
      results$timeRef <- featureExtractionOutput$timeRef
    }
    
    if ("covariates" %in% names(featureExtractionOutput) &&
        dplyr::pull(dplyr::count(featureExtractionOutput$covariates)) > 0) {
      covariates <- featureExtractionOutput$covariates %>%
        dplyr::rename(cohortId = .data$cohortDefinitionId) %>%
        dplyr::left_join(populationSize, by = "cohortId", copy = TRUE) %>%
        dplyr::mutate(
          sd = sqrt(((populationSize * .data$sumValue) + .data$sumValue) / 
                      (populationSize ^ 2))) %>%
        # TODO: check where averageValue computed in standard settings
        # (now computed here)
        dplyr::mutate(mean = .data$sumValue / (populationSize)) %>%
        dplyr::select(-.data$sumValue,-.data$populationSize)
      
      if (FeatureExtraction::isTemporalCovariateData(featureExtractionOutput)) {
        covariates <- covariates %>%
          dplyr::select(.data$cohortId,
                        .data$timeId,
                        .data$covariateId,
                        .data$mean,
                        .data$sd)
      } else {
        covariates <- covariates %>%
          dplyr::select(.data$cohortId,
                        .data$covariateId,
                        .data$mean,
                        .data$sd)
      }
      if ("covariates" %in% names(results)) {
        Andromeda::appendToTable(results$covariates, covariates)
      } else {
        results$covariates <- covariates
      }
    }
    
    if ("covariatesContinuous" %in% names(featureExtractionOutput) &&
        dplyr::pull(dplyr::count(featureExtractionOutput$covariatesContinuous)) > 0) {
      covariates <- featureExtractionOutput$covariatesContinuous %>%
        dplyr::rename(
          mean = .data$averageValue,
          sd = .data$standardDeviation,
          cohortId = .data$cohortDefinitionId
        )
      if (FeatureExtraction::isTemporalCovariateData(featureExtractionOutput)) {
        covariates <- covariates %>%
          dplyr::select(.data$cohortId,
                        .data$timeId,
                        .data$covariateId,
                        .data$mean,
                        .data$sd)
      } else {
        covariates <- covariates %>%
          dplyr::select(.data$cohortId,
                        .data$covariateId,
                        .data$mean,
                        .data$sd)
      }
      if ("covariates" %in% names(results)) {
        Andromeda::appendToTable(results$covariates, covariates)
      } else {
        results$covariates <- covariates
      }
    }
  }
  
  delta <- Sys.time() - startTime
  ParallelLogger::logInfo("Cohort characterization took ",
                          signif(delta, 3),
                          " ",
                          attr(delta, "units"))
  return(results)
}


#' createCustomCovariateSettings
#'
#' @param list_covariates
#'     List of covariates
#'
#' @return covariateSettings
createCustomCovariateSettings <- function(list_covariates) {
  covariateSettings <- list(list_covariates = list_covariates)
  attr(covariateSettings, "fun") <-
    "TreatmentPatterns:::getCustomCovariates"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}


#' getCustomCovariates
#'
#' @param connection 
#' @param oracleTempSchema 
#' @param cdmDatabaseSchema 
#' @param cdmVersion 
#' @param cohortTable 
#' @param cohortId 
#' @param covariateSettings 
#' @param aggregated 
#' @param rowIdField 
#'
#' @return all_covariates
#' @export
#'
#' @examples
#' \dontrun{
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' connection <- DatabaseConnector::connect(connectionDetails)
#' 
#' getCustomCovariates(connection = connection, cdmDatabaseSchema = "main")}
getCustomCovariates <- function(connection,
                                oracleTempSchema = NULL,
                                cdmDatabaseSchema,
                                cdmVersion = "5",
                                cohortTable = "#cohort_person",
                                cohortId = -1,
                                covariateSettings,
                                aggregated = TRUE,
                                rowIdField = "subject_id") {
  # start with empty covariate set
  all_covariates <- NULL

  for (c in 1:length(covariateSettings$list_covariates)) {
    pathToSql <-
      paste0("inst/SQL/", covariateSettings$list_covariates[c], ".sql")
    parameterizedSql <-
      readChar(pathToSql, file.info(pathToSql)$size)[1]

    renderedSql <- SqlRender::render(
      sql = parameterizedSql,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortTable = cohortTable,
      cohortId = cohortId,
      aggregated = aggregated,
      rowIdField = rowIdField,
      covariateId = (999000 + c) * 1000 +
        999
    )

    renderedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = attr(connection, "dbms"),
        oracleTempSchema = oracleTempSchema
      )

    covariates <-
      DatabaseConnector::querySql(
        connection,
        renderedSql,
        snakeCaseToCamelCase = TRUE)

    covariateRef <- data.frame(
      covariateId = (999000 + c) * 1000 + 999,
      covariateName = covariateSettings$list_covariates[c],
      analysisId = 999,
      conceptId = 999000 + c
    )

    analysisRef <- data.frame(
      analysisId = 999,
      analysisName = "Custom",
      domainId = "Custom"
      ,
      startDay = 0,
      endDay = 0,
      isBinary = "Y",
      missingmeansZero = "Y"
    )

    metaData <- list(sql = sql, call = match.call())
    result <- Andromeda::andromeda(
      covariates = covariates,
      covariateRef = covariateRef,
      analysisRef = analysisRef
    )

    attr(result, "metaData") <- "metaData"
    class(result) <- "CovariateData"
    
    all_covariates <- appendCovariateData(result, all_covariates)

  }
  return(all_covariates)
}



#' exportCharacterization
#'
#' Borrowed function from CohortDiagnostics.
#'
#' @param characteristics
#'     Characteristics
#' @param databaseId
#'     Database ID
#' @param incremental
#'     Incremental
#' @param covariateValueFileName
#'     Covariate value file name
#' @param covariateRefFileName
#'     Covariate reference file name
#' @param analysisRefFileName
#'     Analysis reference file name
#' @param timeRefFileName
#'     Time reference file name
#' @param counts
#'     Counts
#' @param minCellCount
#'     Minimum cell count
exportCharacterization <- function(characteristics,
                                   databaseId,
                                   incremental,
                                   covariateValueFileName,
                                   covariateRefFileName,
                                   analysisRefFileName,
                                   timeRefFileName = NULL,
                                   counts,
                                   minCellCount) {
  if (!"covariates" %in% names(characteristics)) {
    warning("No characterization output for submitted cohorts")
  } else if (dplyr::pull(dplyr::count(characteristics$covariateRef)) > 0) {
    characteristics$filteredCovariates <- characteristics$covariates %>%
      dplyr::filter(mean >= 0.0001) %>%
      dplyr::mutate(databaseId = !!databaseId) %>%
      dplyr::left_join(
        counts,
        by = c("cohortId", "databaseId"),
        copy = TRUE) %>%
      dplyr::mutate(
        mean = dplyr::case_when(
          .data$mean != 0 &
            .data$mean < minCellCount / .data$cohortEntries ~ 
            -minCellCount / .data$cohortEntries,
          TRUE ~ .data$mean
        )
      ) %>%
      dplyr::mutate(sd = dplyr::case_when(.data$mean >= 0 ~ sd)) %>%
      dplyr::mutate(
        mean = round(.data$mean, digits = 4),
        sd = round(.data$sd, digits = 4)) %>%
      dplyr::select(-.data$cohortEntries,-.data$cohortSubjects)
    
    if (dplyr::pull(dplyr::count(characteristics$filteredCovariates)) > 0) {
      covariateRef <- dplyr::collect(characteristics$covariateRef)
      writeToCsv(
        data = covariateRef,
        fileName = covariateRefFileName,
        incremental = incremental,
        covariateId = covariateRef$covariateId
      )
      analysisRef <- dplyr::collect(characteristics$analysisRef)
      writeToCsv(
        data = analysisRef,
        fileName = analysisRefFileName,
        incremental = incremental,
        analysisId = analysisRef$analysisId
      )
      if (!is.null(timeRefFileName)) {
        timeRef <- dplyr::collect(characteristics$timeRef)
        writeToCsv(
          data = timeRef,
          fileName = timeRefFileName,
          incremental = incremental,
          analysisId = timeRef$timeId
        )
      }
      writeCovariateDataAndromedaToCsv(
        data = characteristics$filteredCovariates,
        fileName = covariateValueFileName,
        incremental = incremental
      )
    }
  }
}


#' writeCovariateDataAndromedaToCsv
#'
#' Borrowed function from CohortDiagnostics.
#'
#' @param data
#'     Data
#' @param fileName
#'     Filename
#' @param incremental
#'     Incremental
writeCovariateDataAndromedaToCsv <- function(
    data, 
    fileName, 
    incremental = FALSE) {
    if (incremental && file.exists(fileName)) {
      ParallelLogger::logDebug("Appending records to ", fileName)
      batchSize <- 1e5
      
      cohortIds <- data %>%
        distinct(.data$cohortId) %>%
        pull()
      
      tempName <- paste0(fileName, "2")
      
      processChunk <- function(chunk, pos) {
        chunk <- chunk %>%
          filter(!.data$cohort_id %in% cohortIds)
        readr::write_csv(chunk, tempName, append = (pos != 1))
      }
      
      readr::read_csv_chunked(
        file = fileName,
        callback = processChunk,
        chunk_size = batchSize,
        col_types = readr::cols(),
        guess_max = batchSize
      )
      
      addChunk <- function(chunk) {
        colnames(chunk) <- SqlRender::camelCaseToSnakeCase(colnames(chunk))
        readr::write_csv(chunk, tempName, append = TRUE)
      }
      Andromeda::batchApply(data, addChunk)
      unlink(fileName)
      file.rename(tempName, fileName)
    } else {
      if (file.exists(fileName)) {
        ParallelLogger::logDebug(
          "Overwriting and replacing previous ",
          fileName,
          " with new.")
        unlink(fileName)
      } else {
        ParallelLogger::logDebug("Creating ", fileName)
      }
      writeToFile <- function(batch) {
        first <- !file.exists(fileName)
        if (first) {
          colnames(batch) <- SqlRender::camelCaseToSnakeCase(colnames(batch))
        }
        readr::write_csv(batch, fileName, append = !first)
      }
      Andromeda::batchApply(data, writeToFile)
    }
  }


#' writeToCsv
#'
#' Borrowed function from CohortDiagnostics.
#'
#' @param data
#'     Data
#' @param fileName
#'     File name
#' @param incremental
#'     Incremental
#' @param ...
#'     Other parameters
writeToCsv <- function(data, fileName, incremental = FALSE, ...) {
  colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  if (incremental) {
    params <- list(...)
    names(params) <- SqlRender::camelCaseToSnakeCase(names(params))
    params$data = data
    params$fileName = fileName
    do.call(saveIncremental, params)
    ParallelLogger::logDebug("appending records to ", fileName)
  } else {
    if (file.exists(fileName)) {
      ParallelLogger::logDebug("Overwriting and replacing previous ",
                               fileName,
                               " with new.")
    } else {
      ParallelLogger::logDebug("creating ", fileName)
    }
    readr::write_excel_csv(
      x = data,
      file = fileName,
      na = "",
      append = FALSE,
      delim = ","
    )
  }
}


#' appendCovariateData
#'
#' Borrowed function from Triton.
#'
#' @param tempCovariateData
#'     Temporary covariate data
#' @param covariateData
#'     Covariate data
#'
#' @return covariateData
#'     Covariate data
appendCovariateData <- function(tempCovariateData, covariateData) {
  ##==## appends covariate objects ##==##
  if (is.null(covariateData)) {
    covariateData <- tempCovariateData
  } else {
    if (hasData(covariateData$covariates)) {
      if (hasData(tempCovariateData$covariates)) {
        Andromeda::appendToTable(covariateData$covariates,
                                 tempCovariateData$covariates)
      }
    } else if (hasData(tempCovariateData$covariates)) {
      covariateData$covariates <- tempCovariateData$covariates
    }
    if (hasData(covariateData$covariatesContinuous)) {
      if (hasData(tempCovariateData$covariatesContinuous)) {
        Andromeda::appendToTable(
          covariateData$covariatesContinuous,
          tempCovariateData$covariatesContinuous
        )
      } else if (hasData(tempCovariateData$covariatesContinuous)) {
        covariateData$covariatesContinuous <-
          tempCovariateData$covariatesContinuous
      }
    }
    Andromeda::appendToTable(
      covariateData$covariateRef,
      tempCovariateData$covariateRef)
    
    Andromeda::appendToTable(
      covariateData$analysisRef,
      tempCovariateData$analysisRef)
    
    for (name in names(attr(tempCovariateData, "metaData"))) {
      if (is.null(attr(covariateData, "metaData")[name])) {
        attr(covariateData, "metaData")[[name]] <- attr(
          tempCovariateData, 
          "metaData")[[name]]
      } else {
        attr(covariateData, "metaData")[[name]] <- list(
          attr(covariateData, "metaData")[[name]],
          attr(tempCovariateData, "metaData")[[name]])
      }
    }
  }
  return(covariateData)
}


#' hasData
#'
#' Borrowed function from Triton.
#'
#' @param data
#'     Data
hasData <- function(data) {
  ##==## checks if data has data ##==##
  return(!is.null(data) && (data %>% count() %>% pull()) > 0)
}


#' Title
#'
#' Borrowed from devtools:
#' https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
#'
#' @param pkg
#'     Package
#' @param version
#'     Version
is_installed <- function (pkg, version = 0) {
  installed_version <- tryCatch({
    utils::packageVersion(pkg)
    }, error = function(e) {
      NA
      }
  )
  !is.na(installed_version) && installed_version >= version
}


#' ensure_installed
#'
#' Borrowed and adapted function from devtools.
#' https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L74
#' 
#' @param pkg
#'     Package
ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <- paste0(
      sQuote(pkg),
      " must be installed for this functionality.")
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}
