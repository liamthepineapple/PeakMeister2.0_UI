#PeakMeister2.0_UI was written by Erick Helmeczi and Liam Surry 
#Initialized 2024-11-12 @McMaster University
#Department of Chemistry and Chemical Biology 


#Initialize server
server <- function(input, output) {
  output$disclaimerContent <- renderUI({
    includeMarkdown("Documentation/DISCLAIMER.md")
  })
  
  output$readmeContent <- renderUI({
    includeMarkdown("Documentation/README.md")
  })
  
  output$updatesContent <- renderUI({
    includeMarkdown("Documentation/UPDATES.md")
  })
  
  output$licenseContent <- renderUI({
    includeMarkdown("Documentation/LICENSE.md")
  })
}


