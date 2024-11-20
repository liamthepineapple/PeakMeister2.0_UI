#PeakMeister2.0_UI was written by Erick Helmeczi and Liam Surry 
#Initialized 2024-11-12 @McMaster University
#Department of Chemistry and Chemical Biology 

#### 1. Setup ####
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")

pacman::p_load("shiny","shinyBS", "tools","waiter","shinyFiles","RColorBrewer","shinycssloaders", "shinydashboard", "DT", "shinyalert","ggplot2","plotly","hash","pracma", "tidyverse", "stats", "DescTools", "xcms", "rlang","markdown", "openxlsx","readxl","writexl","fontawesome",
               install = TRUE)

#Title of app
title <- "PeakMeister v2.0"

#define UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = title
  ),
  #### 2. Sidebar Main Tabs ####
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
        icon = icon("screwdriver-wrench")),
      #Engine tab -> where the analysis actually occurs and where users input their datafiles 
      menuItem("Engine",
               tabName = "engine",
               icon = icon("heartbeat")),
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
  #### 3. About Main Tab Content####
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
      
      #### 4. User Supplied Inputs Main Tab Content####
      tabItem(
        tabName = "userparameters",
        tabsetPanel(
          id = "userParameters",
          #make subtabs for Mass List, Refererence List, & Paramaters. 
          #Option to either upload and excel file or for you to manually create your own through the app 
          
          #####4.1. Mass List Subtab##### 
          tabPanel("Mass List",
                   fileInput("massList", "Upload Mass List and Parameters (Excel)", accept = c(".xlsx")),
                   div(style = "overflow-x: auto;",
                   DT::dataTableOutput("massListData")),
                   fluidRow(
                     #Create input columns for required inputs from excel file
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
                   #For organizational purposes, separate peak migration time inputs from other paramaters
                   tags$strong(h4("Peak Migration Time Inputs")),
                   #Function for creating a column for each peak. 
                   #May include option to change the number of peaks in the future. For now, its hardcoded at 13
                   fluidRow(
                     lapply(1:13, function(i) {
                       column(3, numericInput(paste0("peak.", i, ".mt"), paste("Peak", i, "MT"), value = 0))
                     })
                   ),
                   #Button for adding a row
                   actionButton("addMassRow", "Add Mass"),
                   #Button for Deleting a selected row
                   actionButton("deletemassrow", "Delete Selected Row"), 
                   #Button for clearing entire table 
                   actionButton("clearMassList", "Clear All")),
          
          #####4.2 Reference Mass List Subtab#####
          tabPanel("Reference Mass List",
                   fileInput("refMassListData", "Upload Reference Mass List (Excel)", accept = c(".xlsx")),
                   div(style = "overflow-x: auto;",
                   DT::dataTableOutput("refMassListData")),
                   fluidRow(
                     #generate input columns for reference mass list 
                     column(3, textInput("ref.class", "Class")),
                     column(3, numericInput("ref.min.mt.min", "Min MT Min", value = 0)),
                     column(3, numericInput("ref.max.mt.min", "Max MT Min", value = 0)),
                     column(3, numericInput("ref.peak.fwhm.tolerance.multiplier", "Peak FWHM Tolerance Multiplier", value = 0)),
                     column(3, numericInput("ref.peak.space.tolerance.percent", "Peak Space Tolerance (%)", value = 0)),
                     column(3, textInput("ref.smoothing.kernel", "Smoothing Kernel")),
                     column(3, numericInput("ref.smoothing.strength", "Smoothing Strength", value = 0))
                   ),
                   #For organizational purposes, separate peak migration time inputs from other paramaters
                   tags$strong(h4("Peak Migration Time Inputs")),
                   #Function for creating a column for each peak. 
                   #May include option to change the number of peaks in the future. For now, its hardcoded at 13
                   fluidRow(
                     lapply(1:13, function(i) {
                       column(3, numericInput(paste0("ref.peak.", i, ".mt"), paste("Peak", i, "MT"), value = 0))
                     })
                   ),
                   #Button for adding a row 
                   actionButton("addRefMassRow", "Add Reference Mass"),
                   #Button for deleting reference mass row 
                   actionButton("deleterefmassrow", "Delete Selected Row"),
                   #Button for clearing whole table 
                   actionButton("clearRefMassList", "Clear All")),
                   
          #####4.3 Parameters list subtab#####
          tabPanel("Parameters",
                   fileInput("parameterList", "Upload paramaters (Excel)", accept = c(".xlsx")),
                   div(style = "overflow-x: auto;",
                   DT::dataTableOutput("parametersData")),
                   fluidRow(
                     #Generate input columns for parameters datatable
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
                   actionButton("addParamRow", "Add Parameter"),
                   #Button for deleting row
                   actionButton("deleteparamrow", "Delete Selected Row"),
                   #Button for clearing table 
                   actionButton("clearparam", "Clear All")),
          
          #####4.4 Project Information Subtab#####
          tabPanel("Project Information",
                   fluidPage(
                     #Set HTML formating fonts and line spacing and allow for text wrapping
                     tags$style(HTML("pre {
               font-family: Arial, sans-serif;
               line-height: 2.0;
               white-space: pre-wrap; /* Allows wrapping */}")),
                     #Generate input columns 
                     column(width = 12,
                            h3("Project Information"),
                            fluidRow(
                              column(3, textInput("projectName", "Project Name (no spaces -> REQUIRED)")),
                              column(3, textInput("projectDescription", "Project Description")),
                              column(3, textInput("projectSupervisor", "Project supervisor (no spaces)")),
                              column(3, textInput("projectContact", "Project Contact (email)"))
                                    ),
                            #Button for saving project info to reactive value -> will be used to generate a metadatafile later on
                            actionButton("saveProjectInfo", "Save Project Info"),
                            #Button for clearing project information
                            actionButton("clearproject", "Clear All"),
                            #text output for project info 
                            tags$hr(),
                            h4("Current Project Info"),
                            verbatimTextOutput("currentProjectInfo")
                          )
                        )
                      )
                    ),
        #Action button for saving manually input data
        downloadButton("downloadData", "Save Table (.xlsx)")
        )
      )
    )
)
