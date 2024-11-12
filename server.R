#PeakMeister2.0_UI was written by Erick Helmeczi and Liam Surry 
#Initialized 2024-11-12 @McMaster University
#Department of Chemistry and Chemical Biology 


#Initialize server
server <- function(input, output, session) {
  
  
  #About tab information
  output$readmeContent <- renderUI({
    content <- readLines("README.md") %>%
      paste(collapse = "\n")
    HTML(markdown::markdownToHTML(text = content))
  })
  
  output$licenseContent <- renderUI({
    content <- readLines("LICENSE.md") %>%
      paste(collapse = "\n")
    HTML(markdown::markdownToHTML(text = content))
  })
  
  output$disclaimerContent <- renderUI({
    content <- readLines("DISCLAIMER.md") %>%
      paste(collapse = "\n")
    HTML(markdown::markdownToHTML(text = content))
  })
  
  output$updatesContent <- renderUI({
    content <- readLines("UPDATES.md") %>%
      paste(collapse = "\n")
    HTML(markdown::markdownToHTML(text = content))
  })
  
  
}
  

