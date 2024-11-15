#PeakMeister2.0_UI was written by Erick Helmeczi and Liam Surry 
#Initialized 2024-11-12 @McMaster University
#Department of Chemistry and Chemical Biology 

if (!require("pacman", quietly = TRUE)) install.packages("pacman")
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")

pacman::p_load("shiny","shinyBS", "tools","waiter","shinyFiles","RColorBrewer","shinycssloaders", "shinydashboard", "DT", "shinyalert","ggplot2","plotly","hash","pracma", "tidyverse", "stats", "DescTools", "xcms", "rlang","markdown", "openxlsx","readxl","writexl",
               install = TRUE)

title <- "PeakMeister v2.0"
#title = title 

#define UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = title
  ),
  #Set up sidebar and tabs
  dashboardSidebar(
    sidebarMenu(
      #About tab -> for README and instructions
      menuItem("About",
        tabName = "about",
        icon = icon("info-circle")),
      #Paramaters tab -> user supplied mass list and instrument settings
      menuItem("User Supplied Paramaters",
        tabName = "userparameters",
        icon = icon("cogs")),
      #Visualization tab for checking electropherograms
      menuItem("Visualization",
        tabName = "processing",
        icon = icon("area-chart")),
      #Manual adjustment tab for processing data
      menuItem("Quality Control",
        tabName = "processing",
        icon = icon("searchengin")),
      #Reporting tab for exporting results/outputting true detections
      menuItem("Reporting",
        tabName = "reporting",
        icon = icon("file-export"))
    )
  ),
  
  dashboardBody(
   
    use_waiter(),
    tabItems(
      # About tab content
      tabItem(
        tabName = "about",
        fluidPage(
          column(width = 12, 
                 h3("Documentation"),
                 box(
                   title = "Documentation Overview", 
                   status = "primary", 
                   solidHeader = TRUE,
                   width = NULL,
                   #Create collapsible tabs that are populated by the content of the .md files in the Documentation folder
                   bsCollapse(id = "collapseExample", open = "Disclaimer",
                              bsCollapsePanel("Disclaimer", uiOutput("disclaimerContent")),
                              bsCollapsePanel("README", uiOutput("readmeContent")),
                              bsCollapsePanel("Updates", uiOutput("updatesContent")),
                              bsCollapsePanel("License", uiOutput("licenseContent"))
                    )
                 )
              )
            )
        ),
      #User supplied inputs tab content 
      tabItem(
        tabName = "userparameters",
        tabsetPanel(
          id = "userParameters",
          #make subtabs for Mass List, Refererence List, & Paramaters. 
          #Option to either upload and excel file or for you to manually create your own through the app 
          
          # 1. Mass List 
          tabPanel("Mass List",
                   fileInput("massList", "Upload Mass List and Parameters (Excel)", accept = c(".xlsx")),
                   div(style = "overflow-x: auto;",
                   DT::dataTableOutput("massListData")),
                   fluidRow(
                     column(3, textInput("name", "Name")),
                     column(3, numericInput("mz", "mz", value = 0)),
                     column(3, numericInput("extraction.window.ppm", "Extraction Window (ppm)", value = 0)),
                     column(3, textInput("interference", "Interference")),
                     column(3, numericInput("interference.comigration.threshold.seconds", "Interference Comigration Threshold (seconds)", value = 0)),
                     column(3, numericInput("minimim.peak.width.seconds", "Minimum Peak Width (seconds)", value = 0)),
                     column(3, numericInput("migration.window.seconds", "Migration Window (seconds)", value = 0)),
                     column(3, numericInput("peak.space.tolerance.percent", "Peak Space Tolerance (%)", value = 0)),
                     column(3, numericInput("snr.threshold", "SNR Threshold", value = 0)),
                     column(3, textInput("smoothing.kernel", "Smoothing Kernel")),
                     column(3, numericInput("smoothing.strength", "Smoothing Strength", value = 0))
                   ),
                   tags$strong(h4("Peak Migration Time Inputs")),
                   fluidRow(
                     lapply(1:13, function(i) {
                       column(3, numericInput(paste0("peak.", i, ".mt"), paste("Peak", i, "MT"), value = 0))
                     })
                   ),
                   actionButton("addMassRow", "Add Mass")
          ),
          
          # 2. Reference Mass List
          tabPanel("Reference Mass List",
                   fileInput("refMassList", "Upload Reference Mass List (Excel)", accept = c(".xlsx")),
                   div(style = "overflow-x: auto;",
                   DT::dataTableOutput("refMassListData")),
                   fluidRow(
                     column(3, textInput("ref.class", "Class")),
                     column(3, numericInput("ref.min.mt.min", "Min MT Min", value = 0)),
                     column(3, numericInput("ref.max.mt.min", "Max MT Min", value = 0)),
                     column(3, numericInput("ref.peak.fwhm.tolerance.multiplier", "Peak FWHM Tolerance Multiplier", value = 0)),
                     column(3, numericInput("ref.peak.space.tolerance.percent", "Peak Space Tolerance (%)", value = 0)),
                     column(3, textInput("ref.smoothing.kernel", "Smoothing Kernel")),
                     column(3, numericInput("ref.smoothing.strength", "Smoothing Strength", value = 0))
                   ),
                   tags$strong(h4("Peak Migration Time Inputs")),
                   fluidRow(
                     lapply(1:13, function(i) {
                       column(3, numericInput(paste0("ref.peak.", i, ".mt"), paste("Peak", i, "MT"), value = 0))
                     })
                   ),
                   actionButton("addRefMassRow", "Add Reference Mass")
          ),
                   
          #Parameters list/tab
          tabPanel("Parameters",
                   fileInput("parameterList", "Upload paramaters (Excel)", accept = c(".xlsx")),
                   div(style = "overflow-x: auto;",
                   DT::dataTableOutput("parametersData")),
                   fluidRow(
                     #Create columns for parameters datatable
                     column(3, numericInput("number.of.injections", "number.of.injections", value = 1)),
                     column(3, numericInput("ref.mass.one", "ref.mass.one", value = 0)),
                     column(3, numericInput("ref.mass.two", "ref.mass.two", value = 0)),
                     column(3, numericInput("ref.mass.window.ppm", "ref.mass.window.ppm", value = 0)),
                     column(3, numericInput("ref.mass.minimum.counts", "ref.mass.minimum.counts", value = 0)),
                     column(3, selectInput("apply.mass.calibration", "apply.mass.calibration", choices = c("Yes", "No"))),
                     column(3, selectInput("apply.smoothing", "apply.smoothing", choices = c("Yes", "No"))),
                     column(3, textInput("plot.format", "plot.format"))
                   ),
                   #Action button for adding rows
                   actionButton("addParamRow", "Add Parameter")),
          #Action button for saving manually input data
          downloadButton("save", "Save Mass List and Parameters"))
          )
        )
      )
    )
