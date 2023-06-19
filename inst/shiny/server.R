# Help functions
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# Shiny server function
server <- function(input, output, session) {
  
  # Functionality for help messages
  showInfoBox <- function(title, htmlFileName) {
    showModal(modalDialog(
      title = title,
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
    ))
  }
  
  observeEvent(input$treatmentPathwaysInfo, {
    showInfoBox("Sunburst plots", "html/treatmentpathways.html")
  })
  observeEvent(input$sankeyDiagramInfo, {
    showInfoBox("Sankey diagram", "html/sankeydiagram.html")
  })
  observeEvent(input$customInfo, {
    showInfoBox("Custom", "html/custom.html")
  })
  
  # Dynamic input parameters
  output$dynamic_input1 = renderUI({
    if (input$viewer1 == "Compare databases") {
      one <- checkboxGroupInput("dataset1", label = "Database", choices = included_databases, selected = included_databases[[1]])
      two <- selectInput("setting1", label = "Study population", choices = all_targetcohorts, selected = all_targetcohorts[[1]])
      return(tagList(one, two))
      
    } else if (input$viewer1 == "Compare study populations") {
      one <- selectInput("dataset1", label = "Database", choices = included_databases, selected = included_databases[[1]])
      return(tagList(one))
      
    }
  })
  
  output$dynamic_input2 = renderUI({
    if (input$viewer2 == "Compare databases") {
      one <- checkboxGroupInput("dataset2", label = "Database", choices = included_databases, selected = included_databases[[1]])
      two <- selectInput("population2", label = "Study setting", choices = all_studynames, selected = all_studynames[[1]])
      three <- selectInput("year2", label = "Year", choices = all_years, selected = "all")
      return(tagList(one, two, three))
      
    } else if (input$viewer2 == "Compare study settings") {
      one <- checkboxGroupInput("population2", label = "Study setting", choices = all_studynames, selected =  all_studynames[[1]])
      two <- selectInput("dataset2", label = "Database", choices = included_databases, selected = included_databases[[1]])
      three <- selectInput("year2", label = "Year", choices = all_years, selected = "all")
      return(tagList(one, two, three))
      
    } else if (input$viewer2 == "Compare over time") {
      one <- checkboxGroupInput("year2", label = "Year", choices = all_years, selected = "all")
      two <- selectInput("dataset2", label = "Database", choices = included_databases, selected = included_databases[[1]])
      three <- selectInput("population2", label = "Study setting", choices = all_studynames, selected = all_studynames[[1]], multiple = FALSE)
      return(tagList(one, two, three))
      
    }
  })
  # Study settings tab
  output$tableStudySettingsTitle <- renderText({"Table with study settings specified."})
  
  output$outputStudySettings <- downloadHandler('study_settings.csv', content = function(file) {
    table <- getStudySettings()
    write.csv(table, file, row.names = FALSE, na = "")
  })
  
  getStudySettings <- reactive({
    cols <- sapply(pathway_settings[pathway_settings$param == "studyName",], function(c) ifelse(c %in% input$population0, 1, 0))
    cols[1] <- 1 # add param list
    table <- pathway_settings[,cols > 0]
    return(table)
  })
  
  output$tableStudySettings <- renderDataTable({
    table <- getStudySettings()
    return(table)
  }, options = list(pageLength = 20))
  
  # Characterization tab
  output$tableCharacterizationTitle <- renderText({"Table with selected demographics and patient characteristics (in percentages)."})
  
  output$outputCharacterization <- downloadHandler('characterization.csv', content = function(file) {
    table <- getCharacterization()
    write.csv(table, file, row.names = FALSE, na = "")
  })
  
  getCharacterization <- reactive({
    if (input$viewer1 == "Compare databases") {
      # Get the data
      data <- data.frame()
      
      for (d in input$dataset1) {
        data <- rbind(data, characterization[[d]])
      }
      
      # Rename to study populations
      data$cohortId <- sapply(data$cohortId, function(c) names(which(all_targetcohorts == c)))
      data <- data[!is.na(data$cohortId),]
      data <- data[data$cohortId == names(which(all_targetcohorts == input$setting1)),]
      
      data$sd <- NULL
      data$cohortId <- NULL
      # data$covariateId <- NULL
      
      data$databaseId <- sapply(data$databaseId, function(d) names(which(included_databases == d)))
      
      # Multiply all numbers between 0-1 by 100 to get percentages (not age in years, number of persons, charlson comorbidity index score)
      data$covariateName <- iconv(data$covariateName,"WINDOWS-1252","UTF-8")
      data$mean[data$mean <= 1 & data$mean > 0 & !grepl("age in years|number of persons|charlson", tolower(data$covariateName))] <-  data$mean[data$mean <= 1 & data$mean > 0 & !grepl("age in years|number of persons|charlson", tolower(data$covariateName))] * 100
      
      # Remove duplicate rows
      data <- unique(data)
      
      # Columns different databases (rows different characteristics)
      table <- reshape2::dcast(data, covariateName + covariateId ~ databaseId, value.var = "mean")
      
    } else if  (input$viewer1 == "Compare study populations") {
      # Get the data
      data <- characterization[[input$dataset1]]
      
      data$sd <- NULL
      data$databaseId <- NULL
      # data$covariateId <- NULL
      
      # Multiply all numbers between 0-1 by 100 to get percentages (not age in years, number of persons, charlson comorbidity index score)
      data$mean[data$mean <= 1 & data$mean > 0 & !grepl("age in years|number of persons|charlson", tolower(data$covariateName))] <-  data$mean[data$mean <= 1 & data$mean > 0 & !grepl("age in years|number of persons|charlson", tolower(data$covariateName))] * 100
      
      # Remove duplicate rows
      data <- unique(data)
      
      # Rename to study populations
      data$cohortId <- sapply(data$cohortId, function(c) names(which(all_targetcohorts == c)))
      data <- data[!is.na(data$cohortId),]
      
      # Columns different study populations (rows different characteristics)
      table <- reshape2::dcast(data, covariateName + covariateId ~ cohortId, value.var = "mean")
    }
    
    # Sort
    table  <- table[order(match(table$covariateName,orderRows)),]
    row.names(table) <- NULL
    
    colnames(table)[1:2] <- c("Covariate name", "Covariate ID")
    table <- round_df(table, 1)
    table[table < 0] <- 0
    
    return(table)
  })
  
  output$tableCharacterization <- renderDataTable({
    table <- getCharacterization()
    return(table)
  }, options = list(pageLength = 20))
  
  
  # Sunburst plot tab
  result_sunburstplot <- reactive({
    
    n_cols <- 2
    
    result <- list()
    
    if (input$viewer2 == "Compare databases") {
      
      for(i in 1:ceiling(length(input$dataset2)/n_cols)) { 
        cols_ <- list();
        
        for(j in (1+n_cols*(i-1)):min(i*n_cols, length(input$dataset2))) {
          
          info <- summary_counts[[input$dataset2[[j]]]][[input$population2]]
          title_plot <- paste0(names(which(included_databases == input$dataset2[[j]])), " (N = ", info$number_target[info$year == input$year2], " , Treated % = ", info$perc[info$year == input$year2], ")")
          plot_location <- paste0("workingdirectory/", input$dataset2[[j]], "/", input$population2,"/", input$dataset2[[j]], "_",input$population2, "_" ,input$year2,"_sunburstplot.html")
          
          cols_ <- append(cols_,list(column(width = floor(8/n_cols), offset = 0, tagList(tags$h4(title_plot), tags$iframe(seamless="seamless", src=plot_location, width=400, height=400, scrolling = "no", frameborder = "no")))));
        }
        result <- append(result, list(fluidRow(cols_, style = "width:1200px" )));
      }
      do.call(tagList, result)
      
    } else if  (input$viewer2 == "Compare study settings") {
      
      for(i in 1:ceiling(length(input$population2)/n_cols)) { 
        cols_ <- list();
        for(j in (1+n_cols*(i-1)):min(i*n_cols, length(input$population2))) {
          
          info <- summary_counts[[input$dataset2]][[input$population2[[j]]]]
          title_plot <- paste0(names(which(all_studynames == input$population2[[j]])), " (N = ", info$number_target[info$year == input$year2], " , Treated % = ", info$perc[info$year == input$year2], ")")
          plot_location <- paste0("workingdirectory/",input$dataset2 ,"/",input$population2[[j]], "/", input$dataset2, "_",input$population2[[j]], "_" ,input$year2,"_sunburstplot.html")
          
          cols_ <- append(cols_,list(column(width = floor(8/n_cols), offset = 0, tagList(tags$h4(title_plot), tags$iframe(seamless="seamless", src=plot_location, width=400, height=400, scrolling = "no", frameborder = "no")))));
        }
        result <- append(result, list(fluidRow(cols_, style = "width:1200px" )));
      }
      do.call(tagList, result)
      
    } else if (input$viewer2 == "Compare over time") {
      
      for(i in 1:ceiling(length(input$year2)/n_cols)) { 
        cols_ <- list();
        for(j in (1+n_cols*(i-1)):min(i*n_cols, length(input$year2))) {
          
          info <- summary_counts[[input$dataset2]][[input$population2]]
          title_plot <- paste0(names(which(all_years == input$year2[[j]])), " (N = ", info$number_target[info$year == input$year2[[j]]], " , Treated % = ", info$perc[info$year == input$year2[[j]]], ")")
          plot_location <- paste0("workingdirectory/",input$dataset2, "/", input$population2, "/", input$dataset2, "_",input$population2, "_" ,input$year2[[j]],"_sunburstplot.html")
          
          cols_ <- append(cols_,list(column(width = floor(8/n_cols), offset = 0, tagList(tags$h4(title_plot), tags$iframe(seamless="seamless", src=plot_location, width=400, height=400, scrolling = "no", frameborder = "no")))));
        }
        result <- append(result, list(fluidRow(cols_, style = "width:1200px" )));
      }
      do.call(tagList, result)
    }
    
    return(result)
  }) 
  
  output$sunburstplots <- renderUI({
    result <- result_sunburstplot()
    return(result)
  })
  
  output$sunburstlegend <- renderUI({
    legend <- tags$iframe(seamless="seamless", src=paste0("workingdirectory/", included_databases[[1]], "/", input$population2[[1]], "/legend.html"), height=600, scrolling = "no", frameborder = "no")
    return(legend)
  })
  
  # Sankey diagram tab
  output$sankeydiagram <- renderUI({
    
    info <- summary_counts[[input$dataset34]][[input$population345]]
    title_plot <- paste0(names(which(included_databases == input$dataset34)), " (N = ", info$number_target[info$year == "all"], " , Treated % = ", info$perc[info$year == "all"], ")")
    plot_location <- paste0("workingdirectory/", input$dataset34, "/",input$population345, "/", input$dataset34, "_",input$population345, "_all_sankeydiagram.html")
    plot <- tagList(tags$h4(title_plot), tags$iframe(seamless="seamless", src=plot_location, width=800, height=800, scrolling = "no", frameborder = "no"))
    
    return(plot)
  })
  
  
  # Treated patients tab
  output$tableTreatedPatientsTitle <- renderText({paste0("Table with the % of patients treated of target population in each treatment layer in the entire study period.")})
  
  
  output$outputTreatedPatients <- downloadHandler('treated_patients.csv', content = function(file) {
    table <- getTreatedPatients()
    write.csv(table, file, row.names = FALSE, na = "")
  })
  
  getTreatedPatients <- reactive({
    table <- summary_treated[[input$dataset34]][[input$population345]]
    
    # Change row labels
    table$index_year <- sapply(stringr::str_extract(table$index_year, "\\d+"), function(l) names(layers[as.integer(l)]))
    
    # Change column names
    colnames(table) <- c("Treatment layer", "% Treated")
    
    # Round numbers
    table <- round_df(table, 1)
    row.names(table) <- NULL
    
    return(table)
  })
  
  output$tableTreatedPatients <- renderDataTable({
    table <- getTreatedPatients()
    return(table)
  }, options = list(pageLength = 5))
  
  
  output$tableSummaryPathwayTitle <- renderText({paste0("Table with a) the % of treated patients with each treatment group somewhere in the full pathway and b) the % of patients with each treatment group as '", tolower(names(which(layers == input$layer3))), "' of the patients receiving a '", tolower(names(which(layers == input$layer3))), "'. Both include patients in '", tolower(names(which(all_years == input$year3))), "'.") })
  
  output$tableSummaryPathwayTitle <- renderText({paste0("Table with a) the % of treated patients with each treatment group somewhere in the full pathway and b) the % of patients with each treatment group as '", tolower(names(which(layers == input$layer3))), "' of the patients receiving a '", tolower(names(which(layers == input$layer3))), "'. Both include patients in 'Entire study period'.") })
  
  output$outputSummaryPathway <- downloadHandler('summary_pathway.csv', content = function(file) {
    table <- getSummaryPathway()
    write.csv(table, file, row.names = FALSE, na = "")
  })
  
  getSummaryPathway <- reactive({
    # Get the data
    if (input$year3 == "all") {
      data <- summary_eventcohorts[[input$dataset34]][[input$population345]]
    } else {
      data <- summary_eventcohorts_year[[input$dataset34]][[input$population345]]
      
      data <- data[data$y == input$year3,]
      data$y <- NULL
    }
    
    # Select and rename column
    col_name <- paste0("event_cohort_name", input$layer3)
    table <- data[,c("outcomes",  "ALL_LAYERS", col_name)]
    colnames(table) <- c("Treatment group", "Full pathway", names(which(layers == input$layer3)))
    
    # Round numbers
    table <- round_df(table, 1)
    
    # Sort
    table  <- table[order(match(table$`Treatment group`,orderEventCohorts)),]
    row.names(table) <- NULL
    
    return(table)
  })
  
  output$tableSummaryPathway <- renderDataTable({
    table <- getSummaryPathway()
    return(table)
  }, options = list(pageLength = 20))
  
  output$figureSummaryPathwayTitleYears <- renderText({
    paste0("Figure with the % of patients with each treatment group as '", tolower(names(which(layers == input$layer3))), "' of the patients receiving a '", tolower(names(which(layers == input$layer3))), "' over the different years (= second column of table over time).")
  })
  
  output$figureSummaryPathwayYears <- renderPlot({
    data <- summary_eventcohorts_year[[input$dataset34]][[input$population345]]
    
    data$ALL_LAYERS <- NULL
    
    col_name <- paste0("event_cohort_name", input$layer3)
    
    plot.data <- data[,c("y", "outcomes", col_name)]
    colnames(plot.data) <- c("Year", "Treatment group", "Percentage")
    
    # Sort
    if (is.null(orderEventCohorts)) {
      plot.data$`Treatment group` <- as.factor(plot.data$`Treatment group`)
    } else {
      plot.data$`Treatment group` <- factor(plot.data$`Treatment group` , levels = orderEventCohorts)
    }
    
    # Plot
    if (length(unique(plot.data$Year))==1) {
      ggplot(plot.data) +
        geom_point(mapping = aes(x = Year, y = Percentage, group = `Treatment group`, colour = `Treatment group`))  +
        labs (x = "Years", y = "Percentage (%)", title = "")
    } else {
      ggplot(plot.data) +
        geom_line(mapping = aes(x = Year, y = Percentage, group = `Treatment group`, colour = `Treatment group`))  +
        labs (x = "Years", y = "Percentage (%)", title = "")
    }
  })
  
  
  output$figureSummaryPathwayTitleLayers <- renderText({
    paste0("Figure with the % of patients with each treatment group over the different layers. This includes patients in '", tolower(names(which(all_years == input$year3))) , "' (= second column of table over treatment layers).")
  })
  
  output$figureSummaryPathwayLayers <- renderPlot({
    
    # Get the data
    if (input$year3 == "all") {
      data <- summary_eventcohorts[[input$dataset34]][[input$population345]]
    } else {
      data <- summary_eventcohorts_year[[input$dataset34]][[input$population345]]
      
      data <- data[data$y == input$year3,]
      data$y <- NULL
    }
    
    # Transform
    data$ALL_LAYERS <- NULL
    
    plot.data <- reshape2::melt(data, id.vars = 'outcomes')
    plot.data$variable <- stringr::str_replace(plot.data$variable, "event_cohort_name", "")
    
    colnames(plot.data) <- c("Treatment group", "Layer", "Percentage")
    
    # Rename
    plot.data$Layer <- sapply(plot.data$Layer, function(l) names(layers[as.integer(l)]))
    
    # Sort
    if (is.null(orderEventCohorts)) {
      plot.data$`Treatment group` <- as.factor(plot.data$`Treatment group`)
    } else {
      plot.data$`Treatment group` <- factor(plot.data$`Treatment group` , levels = orderEventCohorts)
    }
    
    plot.data$Layer <- factor(plot.data$Layer, levels = as.character(names(layers)))
    
    # Plot
    ggplot(plot.data) +
      geom_line(mapping = aes(x = Layer, y = Percentage, group = `Treatment group`, colour = `Treatment group`))  +
      labs (x = "Treatment layers", y = "Percentage (%)", title = "") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  })
  
  
  # Duration tab
  output$tableDurationTitle <- renderText({"Table with average druation of treatments in each treatment layer per treatment group (in days)." })
  
  output$outputDuration <- downloadHandler('duration.csv', content = function(file) {
    table <- getDuration()
    write.csv(table, file, row.names = FALSE, na = "")
  })
  
  getDuration <- reactive({
    # Get the data
    data <- duration[[input$dataset34]][[input$population345]]
    
    # Rename
    data <- reshape2::dcast(data, event_cohort_name ~ event_seq, value.var = "AVG_DURATION")
    colnames(data) <- c("Treatment group", sapply(colnames(data)[2:(ncol(data)-1)], function(l) names(layers[as.integer(l)])), "Overall")
    
    # Sort
    data  <- data[order(match(data$`Treatment group`,orderEventCohorts)),]
    row.names(data) <- NULL
    
    data <- round_df(data, 1)
    data[is.na(data)] <- "NA"
    
    return(data)
  })
  
  output$tableDuration <- renderDataTable({
    data <- getDuration()
    return(data)
  }, options = list(pageLength = 20))
  
  
  output$heatmapDurationTitle <- renderText({"Figure with average duration of treatments in each treatment layer per treatment group (in days)." })
  
  output$heatmapDuration <- renderPlot({
    
    # Get the data
    data <- duration[[input$dataset34]][[input$population345]]
    
    # Rename
    data <- reshape2::dcast(data, event_cohort_name ~ event_seq, value.var = "AVG_DURATION")
    colnames(data) <- c("Treatment group", sapply(colnames(data)[2:(ncol(data)-1)], function(l) names(layers[as.integer(l)])), "Overall")
    
    # Sort
    data <- data[order(-match(data$`Treatment group`,orderEventCohorts)),]
    
    # Create plot
    data.plot <- reshape2::melt(data, id.var = 'Treatment group')
    data.plot$value_category <- cut(as.numeric(data.plot$value), breaks = c(0,1,30,90,120,180,1000), labels = c("0-1", "1-30", "30-90", "90-120", "120-180", "180+"), right = FALSE)
    
    colors <- c("#FFFFFF", "#FFFFbF", "#FFFF00", "#FFAA00","#FF0000","#a11212", "#631616")
    names(colors) <- c(NA, "0-1", "1-30", "30-90", "90-120", "120-180", "180+")
    
    ggplot(data.plot) + geom_tile(aes(x=variable, y= `Treatment group`, fill = value_category), colour = "white") + scale_fill_manual(name = "Duration", values= colors, na.value = "#FFFFFF") +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
}

