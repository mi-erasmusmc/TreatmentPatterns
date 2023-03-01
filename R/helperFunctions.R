#' extractFile
#' 
#' Extract table with specific name from database server.
#'
#' @param connection DatabaseConnector connection object
#' @param tableName Name of table
#' @param resultsSchema Schema of results
#' @param dbms Name of dbms to use
#'
#' @import checkmate
#' @importFrom DatabaseConnector querySql
#' @import SqlRender
#' 
#' @return data the extracted table as a data.frame
#' @export
#' 
#' @examples
#' \dontrun{
#'   con <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
#'   extractFile(con, "person", "main", "sqlite")
#'}
extractFile <- function(connection, tableName, resultsSchema, dbms) {
  # Assertions
  checkmate::checkClass(connection, "DatabaseConnectorDbiConnection")
  checkmate::checkCharacter(tableName, len = 1)
  checkmate::checkCharacter(resultsSchema, len = 1)
  checkmate::checkCharacter(dbms, len = 1)
  
  parameterizedSql <- "SELECT * FROM @resultsSchema.@tableName"
  renderedSql <- SqlRender::render(
    parameterizedSql,
    resultsSchema = resultsSchema,
    tableName = tableName)

  translatedSql <- SqlRender::translate(
    renderedSql,
    targetDialect = dbms)
  DatabaseConnector::querySql(connection, translatedSql)
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

#' is_installed
#'
#' Borrowed from devtools:
#' https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
#'
#' @param pkg package name
#' @param version the minimum version of the package, by default 0
#' 
#' @importFrom utils packageVersion
#' 
#' @return whether the given package with the version is installed
#' 
#' @examples
#' \dontrun{
#' is_installed("base")
#' }
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
#' Makes sure the given package is installed. Borrowed and adapted function from devtools.
#' https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L74
#' 
#' @param pkg package name
#' 
#' @importFrom utils install.packages
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#' TreatmentPatterns:::ensure_installed("base")
#' }
ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <- paste0(
      sQuote(pkg),
      " must be installed for this functionality.")
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        utils::install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}
