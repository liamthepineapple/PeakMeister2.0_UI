#PeakMeister2.0_UI was written by Erick Helmeczi and Liam Surry 
#Initialized 2024-11-12 @McMaster University
#Department of Chemistry and Chemical Biology 

# Run the application 
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
