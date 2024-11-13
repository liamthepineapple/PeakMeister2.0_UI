#PeakMeister2.0_UI was written by Erick Helmeczi and Liam Surry 
#Initialized 2024-11-12 @McMaster University
#Department of Chemistry and Chemical Biology 


#Initialize server
server <- function(input, output, session) {
  
  #Read .md files to populate "about" Tab
  #Make function to read markdown files
  read_md_file <- function(file){
    markdown::markdownToHTML(file) #paste(readLines(file), collapse = "<br>")
  }
  
  output$disclaimerContent <- renderUI({
    HTML(read_md_file("Documentation/DISCLAIMER.md"))
  })
  output$readmeContent <- renderUI({
    HTML(read_md_file("Documentation/README.md"))
  })
  output$updatesContent <- renderUI({
    HTML(read_md_file("Documentation/UPDATES.md"))
  })
  output$licenseContent <- renderUI({
    HTML(read_md_file("Documentation/LICENSE.md"))
  })
}
   

