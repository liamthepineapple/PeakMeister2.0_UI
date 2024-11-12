#PeakMeister2.0_UI was written by Erick Helmeczi and Liam Surry 
#Initialized 2024-11-12 @McMaster University
#Department of Chemistry and Chemical Biology 

if (!require("pacman", quietly = TRUE)) install.packages("pacman")
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")

pacman::p_load("shiny","tools","waiter","shinyDirectoryInput","RColorBrewer","shinycssloaders", "shinydashboard","MSnbase", "DT", "shinyalert","ggplot2","plotly","hash","pracma", "tidyverse", "stats", "DescTools", "xcms", "rlang", "ggpubr",
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
        tabName = "parameters",
        icon = ("user-cog")),
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
    #About tab content
    tabItem(
      tabName = "about",
      fluidPage(
        
      )
  )
  
  
  
  
  
  
)