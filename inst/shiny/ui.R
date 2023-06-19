# Help functions
addInfo <- function(item, infoId) {
  infoTag <- tags$small(
    class = "badge pull-right action-button",
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
    type = "button",
    id = infoId,
    "i"
  )
  item$children[[1]]$children <-
    append(item$children[[1]]$children, list(infoTag))
  return(item)
}

# Shiny ui function
ui <- dashboardPage(
  dashboardHeader(title = "TreatmentPatterns",
                  tags$li(div(img(src = 'logo.png',
                                  title = "OHDSI PLP", height = "40px", width = "40px"),
                              style = "padding-top:0px; padding-bottom:0px;"),
                          class = "dropdown")
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      
      # Tabs (some with additional information)
      menuItem("About", tabName = "about"),
      menuItem("Databases", tabName = "databases"),
      menuItem("Study settings", tabName = "studysettings"),
      menuItem("Characterization", tabName = "characterization"),
      addInfo(menuItem("Sunburst plots", tabName = "pathways"), "treatmentPathwaysInfo"),
      addInfo(menuItem("Sankey diagram", tabName = "sankeydiagram"), "sankeyDiagramInfo"),
      menuItem("Treated patients", tabName = "summarypathway"),
      menuItem("Duration eras", tabName = "duration"),
      addInfo(menuItem("Custom", tabName = "custom"), "customInfo"),
      
      # Input parameters
      conditionalPanel(
        condition = "input.tabs=='studysettings'",
        checkboxGroupInput("population0", label = "Study setting", choices = all_studynames, selected = all_studynames[[1]])
      ),
      conditionalPanel(
        condition = "input.tabs=='characterization'",
        radioButtons("viewer1", label = "Viewer", choices = c("Compare databases", "Compare study populations"), selected = "Compare databases")
      ),
      conditionalPanel(
        condition = "input.tabs=='characterization'",
        htmlOutput("dynamic_input1")
      ),
      conditionalPanel(
        condition = "input.tabs=='pathways'",
        radioButtons("viewer2", label = "Viewer", choices = c("Compare databases", "Compare study settings", "Compare over time"), selected = "Compare databases")
      ),
      conditionalPanel(
        condition = "input.tabs=='pathways'",
        htmlOutput("dynamic_input2")),
      
      conditionalPanel(
        condition = "input.tabs=='sankeydiagram' || input.tabs=='summarypathway' || input.tabs=='duration'",
        selectInput("dataset34", label = "Database", choices = included_databases, selected = included_databases[[1]])
      ),
      conditionalPanel(
        condition = "input.tabs=='sankeydiagram' ||input.tabs=='summarypathway' || input.tabs=='duration' || input.tabs=='custom'",
        selectInput("population345", label = "Study setting", choices = all_studynames, selected = all_studynames[[1]])
      ),
      conditionalPanel(
        condition = "input.tabs=='summarypathway'",
        selectInput("year3", label = "Year", choices = all_years, selected = "all")),
      conditionalPanel(
        condition = "input.tabs=='summarypathway'",
        radioButtons("layer3", label = "Treatment layer", choices = layers, selected = 1)),
      conditionalPanel(
        condition = "input.tabs=='custom'",
        checkboxGroupInput("dataset5", label = "Database", choices = included_databases, selected = included_databases[[1]])
      )
    )
  ),
  dashboardBody(
    
    tags$body(tags$div(id="ppitest", style="width:1in;visible:hidden;padding:0px")),
    tags$script('$(document).on("shiny:connected", function(e) {
                                    var w = window.innerWidth;
                                    var h = window.innerHeight;
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("pltChange", obj);
                                });
                                $(window).resize(function(e) {
                                    var w = $(this).width();
                                    var h = $(this).height();
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("pltChange", obj);
                                });
                            '),
    
    tabItems(
      tabItem(
        tabName = "about",
        br(),
        p(
          "This web-based application provides an interactive platform to explore the results of the TreatmentPatterns R Package. 
          This R package contains the resources for performing a treatment pathway analysis of a study population of interest."
        ),
        HTML("<li>R package: <a href=\"https://github.com/mi-erasmusmc/TreatmentPatterns\">GitHub</a></li>"),
        h3("Background"),
        p("There is a lack of real-world evidence on how patients with specific medical conditions are treated in the real-world. Understanding current treatment practices is important to efficiently spend resources to improve clinical practice. This package gives insight in treatment patterns to help understand and address current research gaps in clinical care by utilizing the powerful analytical tools developed by the Observational Health Data Sciences and Informatics (OHDSI) community."),
        h3("Methods"),
        p("This study will describe the treatment pathway consisting of specified events of interest (e.g. prescriptions of drugs, therapies, other treatments) for specified target cohorts (study populations of interest). For each of the target cohorts, a sunburst diagram (and more) is produced to describe the treatment sequence observed in the target population."),
        h3("Development Status"),
        p("The results presented in this application are not final yet and should be treated as such (no definite conclusions can be drawn based upon this and the results should not be distributed further).")
      ),
      
      tabItem(
        tabName = "databases",
        includeHTML("./html/databasesInfo.html")
      ),
      
      tabItem(tabName = "studysettings",
              box(width = 12,
                  textOutput("tableStudySettingsTitle"),
                  div(style = 'overflow-x: scroll',dataTableOutput("tableStudySettings", width = "100%")),
                  downloadButton(outputId = "outputStudySettings", label = "Download")
              )
      ),
      
      tabItem(tabName = "characterization",
              box(width = 12,
                  textOutput("tableCharacterizationTitle"),
                  div(style = 'overflow-x: scroll',dataTableOutput("tableCharacterization", width = "100%")),
                  downloadButton(outputId = "outputCharacterization", label = "Download")
              )
      ),
      
      tabItem(tabName = "pathways",
              column(width = 8, 
                     box(
                       title = "Treatment Pathways", width = 30, status = "primary",
                       htmlOutput("sunburstplots"))),
              column(width = 3, htmlOutput("sunburstlegend"))
      ),
      
      tabItem(tabName = "sankeydiagram",
              column(width = 12, 
                     box(
                       title = "Treatment Pathways", width = 30, status = "primary",
                       htmlOutput("sankeydiagram")))
      ),
      
      tabItem(tabName = "summarypathway",
              box(width = 6,
                  textOutput("tableTreatedPatientsTitle"),
                  dataTableOutput("tableTreatedPatients"),
                  downloadButton(outputId = "outputTreatedPatients", label = "Download"),
                  textOutput("tableSummaryPathwayTitle"),
                  dataTableOutput("tableSummaryPathway"),
                  downloadButton(outputId = "outputSummaryPathway", label = "Download")
              ),
              box(width = 6,
                  textOutput("figureSummaryPathwayTitleYears"),
                  plotOutput("figureSummaryPathwayYears", height = "450px"),
                  textOutput("figureSummaryPathwayTitleLayers"),
                  plotOutput("figureSummaryPathwayLayers", height = "450px")
              )
      ),
      
      tabItem(tabName = "duration",
              tabsetPanel(
                id = "resultDurationPanel",
                tabPanel(
                  "Tables",
                  br(),
                  textOutput("tableDurationTitle"),
                  br(),
                  dataTableOutput("tableDuration"),
                  downloadButton(outputId = "outputDuration", label = "Download")
                ),
                tabPanel(
                  "Figures",
                  br(),
                  textOutput("heatmapDurationTitle"),
                  br(),
                  plotOutput("heatmapDuration", height = "500px")
                )
              )
      ),
      
      tabItem(tabName = "custom",
              box(width = 12,
                  uiOutput("...")
              )
      )
    )
  )
)
