#PeakMeister2.0_UI was written by Erick Helmeczi and Liam Surry 
#Initialized 2024-11-12 @McMaster University
#Department of Chemistry and Chemical Biology 

if (!require("pacman", quietly = TRUE)) install.packages("pacman")
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")

pacman::p_load("shiny","shinyBS", "tools","waiter","shinyFiles","RColorBrewer","shinycssloaders", "shinydashboard", "DT", "shinyalert","ggplot2","plotly","hash","pracma", "tidyverse", "stats", "DescTools", "xcms", "rlang","markdown", "openxlsx","readxl",
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
          id = "subtabs",
          tabPanel("Mass List",
                   fileInput("massList",
                             "Upload Mass List and Parameters (Excel)", accept = c(".xlsx")),
                   tableOutput("massListData")),
          
          
          
          
          tabPanel("Reference Mass List",
                   tableOutput("refMassListData")),
          
          
          
          tabPanel("Parameters",
                   fluidRow(
                     column(6, textInput("paramName", "Parameter Name")),
                     column(6, numericInput("paramValue", "Parameter Value", value = 0))
                   ),
                   actionButton("addParamRow", "Add Parameter"),
                   DT::dataTableOutput("parametersTable"))
          
          
      )
        
        
        
        
        
      )
    )
  )
)
