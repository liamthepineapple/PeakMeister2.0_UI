#PeakMeister2.0_UI was written by Erick Helmeczi and Liam Surry 
#Initialized 2024-11-12 @McMaster University
#Department of Chemistry and Chemical Biology 


#Initialize server
server <- function(input, output, session) {
  ####1. About tab####
  #Functions for "About" tab page -> reading information from markdown files
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
####2. User supplied inputs tab####
 #Load data from user supplied Excel file using sheet names
  
  # Initialize reactive values
  massData <- reactiveVal(data.frame(
    name = character(),
    mz = numeric(),
    extraction.window.ppm = numeric(),
    interference = character(),
    interference.comigration.threshold.seconds = numeric(),
    minimim.peak.width.seconds = numeric(),
    migration.window.seconds = numeric(),
    peak.space.tolerance.percent = numeric(),
    snr.threshold = numeric(),
    smoothing.kernel = character(),
    smoothing.strength = numeric(),
    stringsAsFactors = FALSE
  ))
  
  refMassListData <- reactiveVal(data.frame(
    ref.class = character(),
    ref.min.mt.min = numeric(),
    ref.max.mt.min = numeric(),
    peak.fwhm.tolerance.multiplier = numeric(),
    peak.space.tolerance.percent = numeric(),
    ref.smoothing.kernel = character(),
    ref.smoothing.strength = numeric(),
    stringsAsFactors = FALSE
  ))
  
  parametersData <- reactiveVal(data.frame(
    number.of.injections = numeric(),
    ref.mass.one = numeric(),
    ref.mass.two = numeric(),
    ref.mass.window.ppm = numeric(),
    ref.mass.minimum.counts = numeric(),
    apply.mass.calibration = character(),
    apply.smoothing = character(),
    plot.format = character(),
    stringsAsFactors = FALSE
  ))
  
  # Render editable tables
  output$massListData <- DT::renderDataTable({ massData() }, editable = TRUE)
  output$refMassListData <- DT::renderDataTable({ refMassListData() }, editable = TRUE)
  output$parametersData <- DT::renderDataTable({ parametersData() }, editable = TRUE)
  
  #Look for an excel file/read parameters file.
  observeEvent(input$massList,
               {
                 req(input$massList)
                 # Load data from the uploaded file
                 mass_df <- readxl::read_excel(input$massList$datapath, sheet = "Mass List") %>%
                   as.data.frame()
                 
                 is_df <- readxl::read_excel(input$massList$datapath, sheet = "Reference Mass List") %>%
                   as.data.frame()
                 
                 parameters_df <- readxl::read_excel(input$massList$datapath, sheet = "Parameters") %>%
                   as.data.frame()
                 
                 # Store the data in reactive values
                 massData(mass_df)
                 refMassListData(is_df)
                 parametersData(parameters_df)
                 
              
                 })
  
  # Handle manual input when no file is uploaded
  observeEvent(input$massList, {
    if (is.null(input$massList)) {
      massData(data.frame())  
      refMassData(data.frame())  
      parametersData(data.frame())
      
      showNotification("No file uploaded. You can enter data manually.", type = "info")
    }
  })
  
  # Manual Mass List Input
  observeEvent(input$addMassRow, {
    newRow <- data.frame(
      name = input$name,
      mz = input$mz,
      extraction.window.ppm = input$extraction.window.ppm,
      interference = input$interference,
      interference.comigration.threshold.seconds = input$interference.comigration.threshold.seconds,
      minimim.peak.width.seconds = input$minimim.peak.width.seconds,
      migration.window.seconds = input$migration.window.seconds,
      peak.space.tolerance.percent = input$peak.space.tolerance.percent,
      snr.threshold = input$snr.threshold,
      smoothing.kernel = input$smoothing.kernel,
      smoothing.strength = input$smoothing.strength,
      peak.1.mt = input$peak.1.mt,
      peak.2.mt = input$peak.2.mt,
      peak.3.mt = input$peak.3.mt,
      peak.4.mt = input$peak.4.mt,
      peak.5.mt = input$peak.5.mt,
      peak.6.mt = input$peak.6.mt,
      peak.7.mt = input$peak.7.mt,
      peak.8.mt = input$peak.8.mt,
      peak.9.mt = input$peak.9.mt,
      peak.10.mt = input$peak.10.mt,
      peak.11.mt = input$peak.11.mt,
      peak.12.mt = input$peak.12.mt,
      peak.13.mt = input$peak.13.mt,
      stringsAsFactors = FALSE
    )
    
    updatedData <- rbind(massData(), newRow)
    massData(updatedData)
    
    # Clear inputs after adding
    updateTextInput(session, "name", value = "")
    updateNumericInput(session, "mz", value = 0)
    updateNumericInput(session, "extraction.window.ppm", value = 0)
    updateTextInput(session, "interference", value = "")
    updateNumericInput(session, "interference.comigration.threshold.seconds", value = 0)
    updateNumericInput(session, "minimim.peak.width.seconds", value = 0)
    updateNumericInput(session, "migration.window.seconds", value = 0)
    updateNumericInput(session, "peak.space.tolerance.percent", value = 0)
    updateNumericInput(session, "snr.threshold", value = 0)
    updateTextInput(session, "smoothing.kernel", value = "")
    updateNumericInput(session, "smoothing.strength", value = 0)
    
    # Clear peaks inputs
    lapply(1:13, function(i) {
      updateNumericInput(session, paste0("peak.", i, ".mt"), value = 0)
    })
  })
  
  # Manual Reference Mass Input
  observeEvent(input$addRefMassRow, {
    newRow <- data.frame(
      ref.class = input$ref.class,
      ref.min.mt.min = input$ref.min.mt.min,
      ref.max.mt.min = input$ref.max.mt.min,
      peak.fwhm.tolerance.multiplier = input$ref.peak.fwhm.tolerance.multiplier,
      peak.space.tolerance.percent = input$ref.peak.space.tolerance.percent,
      ref.smoothing.kernel = input$ref.smoothing.kernel,
      ref.smoothing.strength = input$ref.smoothing.strength,
      peak.1.mt = input$peak.1.mt,
      peak.2.mt = input$peak.2.mt,
      peak.3.mt = input$peak.3.mt,
      peak.4.mt = input$peak.4.mt,
      peak.5.mt = input$peak.5.mt,
      peak.6.mt = input$peak.6.mt,
      peak.7.mt = input$peak.7.mt,
      peak.8.mt = input$peak.8.mt,
      peak.9.mt = input$peak.9.mt,
      peak.10.mt = input$peak.10.mt,
      peak.11.mt = input$peak.11.mt,
      peak.12.mt = input$peak.12.mt,
      peak.13.mt = input$peak.13.mt,
      stringsAsFactors = FALSE
    )
    
    updatedData <- rbind(refMassListData(), newRow)
    refMassListData(updatedData)
    
    #Clear inputs after adding
    updateTextInput(session, "ref.class", value = "")
    updateNumericInput(session, "ref.min.mt.min", value = 0)
    updateNumericInput(session, "ref.max.mt.min", value = 0)
    updateNumericInput(session, "ref.peak.fwhm.tolerance.multiplier", value = 0)
    updateNumericInput(session, "ref.peak.space.tolerance.percent", value = 0)
    updateTextInput(session, "ref.smoothing.kernel", value = "")
    updateNumericInput(session, "ref.smoothing.strength", value = 0)
    
    # Clear peaks inputs
    lapply(1:13, function(i) {
      updateNumericInput(session, paste0("ref.peak.", i, ".mt"), value = 0)
    })
  })
  
  # Manual Parameters Input
  observeEvent(input$addParamRow, {
    newRow <- data.frame(
      number.of.injections = input$number.of.injections,
      ref.mass.one = input$ref.mass.one,
      ref.mass.two = input$ref.mass.two,
      ref.mass.window.ppm = input$ref.mass.window.ppm,
      ref.mass.minimum.counts = input$ref.mass.minimum.counts,
      apply.mass.calibration = input$apply.mass.calibration,
      apply.smoothing = input$apply.smoothing,
      plot.format = input$plot.format,
      stringsAsFactors = FALSE
    )
    
    updatedData <- rbind(parametersData(), newRow)
    parametersData(updatedData)
    #Clear previous values 
    updateNumericInput(session, "number.of.injections", value = 0)
    updateNumericInput(session, "ref.mass.one", value = 0)
    updateNumericInput(session, "ref.mass.two", value = 0)
    updateNumericInput(session, "ref.mass.window.ppm", value = 0)
    updateNumericInput(session, "ref.mass.minimum.counts", value = 0)
    updateCheckboxInput(session, "apply.mass.calibration", value = FALSE)
    updateCheckboxInput(session, "apply.smoothing", value = FALSE)
    updateTextInput(session, "plot.format", value = "")
  })
  #Project information input
  #Generate reactive values for information
  #The idea behind this tab is to automatically generate a metadata file on the project information that is included with the analysis. Makes environment more reproducible and traceable. Additionally, project infomration will be saved into the generated results folder so results are easier to identify/organize on your computer. 
  projectName <- reactiveVal("")
  projectDescription <- reactiveVal("")
  projectSupervisor <- reactiveVal("")
  projectContact <- reactiveVal("")
  #Update reactive values when someone clicks Save project info)
  observeEvent(input$saveProjectInfo, {
    projectName(input$projectName)  # Update project name
    projectDescription(input$projectDescription)  # Update project description
    projectSupervisor(input$projectSupervisor)  # Update project supervisor
    projectContact(input$projectContact)  # Update project contact
  })
  #Display output information from reactive variables
  output$currentProjectInfo <- renderText({
    paste("Project Name:", projectName(), 
          "\nProject Description:", projectDescription(), 
          "\nProject Supervisor:", projectSupervisor(), 
          "\nProject Contact:", projectContact())
  })
  
  
  
  #Function for saving datatable once user has uploaded their values
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Mass_List_and_Parameters_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Create a list of data frames for each sheet
      write_xlsx(list(
        Parameters = parametersData(),
        Mass_List = massData(),
        Reference_Mass_List = refMassListData()
      ), path = file)
    }
  )
  ####3. Engine tab####
  #Where users upload their data
  
  #Closing bracket
}
  



