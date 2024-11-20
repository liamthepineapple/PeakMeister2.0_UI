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
  
  #####2.1 Initialize environment#####
  #Initialize reactive values as empty
  #Reactive values for mass list
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
  
  #Reactive values for reference mass list
  refMassListData <- reactiveVal(data.frame(
    name = character(),
    mz = numeric(),
    extraction.window.ppm = numeric(),
    class = character(),
    min.mt.min = numeric(),
    max.mt.min = numeric(),
    peak.fwhm.tolerance.multiplier = numeric(),
    peak.space.tolerance.percent = numeric(),
    smoothing.kernel = character(),
    smoothing.strength = numeric(),
    stringsAsFactors = FALSE
  ))
  
  #reactive values for paramaters
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
  
  #####2.2 Data Table Input#####
  #Look for an excel file/read parameters file.
  observeEvent(input$massList,
               {
                 req(input$massList)
                 # Load data from the uploaded file -> excel sheet must have sheet names "Mass List, Reference Mass List, and Paramaters"
                 #Mass List
                 mass_df <- readxl::read_excel(input$massList$datapath, sheet = "Mass List") %>%
                   as.data.frame()
                 #Reference Mass List
                 is_df <- readxl::read_excel(input$massList$datapath, sheet = "Reference Mass List") %>%
                   as.data.frame()
                 #Paramaters
                 parameters_df <- readxl::read_excel(input$massList$datapath, sheet = "Parameters") %>%
                   as.data.frame()
                 # Store the data in reactive values
                 massData(mass_df)
                 refMassListData(is_df)
                 parametersData(parameters_df)
                 })
  
  # Handle manual input when no file is uploaded -> allows users to input their own data manually 
  observeEvent(input$massList, {
    if (is.null(input$massList)) {
      massData(data.frame())  
      refMassListData(data.frame())  
      parametersData(data.frame())
      
      showNotification("No file uploaded. You can enter data manually.", type = "info")
    }
  })
  
  #####2.3 Manual Mass List Input#####
  #Add a row in the table 
  observeEvent(input$addMassRow, {
    newRow <- data.frame(
      #Input values for table 
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
    #Update dataset 
    updatedData <- rbind(massData(), newRow)
    massData(updatedData)
    
    # Clear inputs after adding allowing users to enter new ones
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
  
  #Delete selected row in Mass List server function 
  observeEvent(input$deletemassrow, {
    selected_row <- input$massListData_rows_selected
    if (length(selected_row) > 0) {
      updatedData <- massData()[-selected_row, ]
      massData(updatedData)} else {
      showNotification("Please select a row to delete.", type = "error")}})
  #Server function for clearing data table
  observeEvent(input$clearMassList, {
    massData(data.frame())
    showNotification("Table has been reset.", type = "message")})
  #Data table output
  output$massListData <- DT::renderDataTable({
    DT::datatable(massData(), selection = 'single', editable = TRUE)})
  
  #####2.4 Manual Reference Mass Input#####
  observeEvent(input$addRefMassRow, {
    newRow <- data.frame(
      #Input values for table
      name = input$name,
      mz = input$mz,
      extraction.window.ppm = input$extraction.window.ppm,
      class = input$class,
      min.mt.min = input$min.mt.min,
      max.mt.min = input$max.mt.min,
      peak.fwhm.tolerance.multiplier = input$peak.fwhm.tolerance.multiplier,
      peak.space.tolerance.percent = input$peak.space.tolerance.percent,
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
    #Update dataset
    updatedData <- rbind(refMassListData(), newRow)
    refMassListData(updatedData)
    
    #Clear inputs after adding
    updateTextInput(session, "name", value = "")
    updateNumericInput(session, "mz", value = 0)
    updateNumericInput(session, "extraction.window.ppm", value = 0)
    updateTextInput(session, "class", value = "")
    updateNumericInput(session, "min.mt.min", value = 0)
    updateNumericInput(session, "max.mt.min", value = 0)
    updateNumericInput(session, "peak.fwhm.tolerance.multiplier", value = 0)
    updateNumericInput(session, "peak.space.tolerance.percent", value = 0)
    updateTextInput(session, "smoothing.kernel", value = "")
    updateNumericInput(session, "smoothing.strength", value = 0)
    
    # Clear peaks inputs
    lapply(1:13, function(i) {
      updateNumericInput(session, paste0("ref.peak.", i, ".mt"), value = 0)
    })
  })
  
  #Deleting a selected row in reference mass list
  observeEvent(input$deleterefmassrow, {
    selected_row <- input$refMassListData_rows_selected
    if (length(selected_row) > 0) {
      updatedData <- refMassListData()[-selected_row, ]
      refMassListData(updatedData)
    } else {
      showNotification("Please select a row to delete.", type = "error")}})
  # Clearing the reference mass list
  observeEvent(input$clearRefMassList, {
    refMassListData(data.frame())
    showNotification("Reference Mass List has been cleared.", type = "message")})
  # Data table output
  output$refMassListData <- DT::renderDataTable({
    DT::datatable(refMassListData(), selection = 'single', editable = TRUE)})
  
  #####2.5 Manual Parameters Input#####
  observeEvent(input$addParamRow, {
    newRow <- data.frame(
      #Input values for table
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
    #Update dataset
    updatedData <- rbind(parametersData(), newRow)
    parametersData(updatedData)
    
    #Clear input values when user adds more paramaters 
    updateNumericInput(session, "number.of.injections", value = 0)
    updateNumericInput(session, "ref.mass.one", value = 0)
    updateNumericInput(session, "ref.mass.two", value = 0)
    updateNumericInput(session, "ref.mass.window.ppm", value = 0)
    updateNumericInput(session, "ref.mass.minimum.counts", value = 0)
    updateCheckboxInput(session, "apply.mass.calibration", value = FALSE)
    updateCheckboxInput(session, "apply.smoothing", value = FALSE)
    updateTextInput(session, "plot.format", value = "")
  })
  
  # Deleting selected parameter row
  observeEvent(input$deleteparamrow, {
    selected_row <- input$parametersData_rows_selected
    if (length(selected_row) > 0) {
      updatedData <- parametersData()[-selected_row, ]
      parametersData(updatedData)
    } else {
      showNotification("Please select a row to delete.", type = "error")}})
  # Clearing the parameters list
  observeEvent(input$clearparam, {
    parametersData(data.frame())
    showNotification("Parameters List has been cleared.", type = "message")})
  # Data table output
  output$parametersData <- DT::renderDataTable({
    DT::datatable(parametersData(), selection = 'single', editable = TRUE)})
  
  #####2.6 Project information input#####
  #The idea behind this tab is to automatically generate a metadata file on the project information that is included with the analysis. Makes environment more reproducible and traceable. Additionally, project information will be saved into the generated results folder so results are easier to identify/organize on your computer. 
  
  #Generate reactive values for information
  projectName <- reactiveVal("")
  projectDescription <- reactiveVal("")
  projectSupervisor <- reactiveVal("")
  projectContact <- reactiveVal("")
  #Update reactive values when someone clicks Save project info)
  observeEvent(input$saveProjectInfo, {
    projectName(input$projectName)  
    projectDescription(input$projectDescription)  
    projectSupervisor(input$projectSupervisor)  
    projectContact(input$projectContact)  
  })
  #Display output information from reactive variables
  output$currentProjectInfo <- renderText({
    paste("Project Name:", projectName(), 
          "\nProject Description:", projectDescription(), 
          "\nProject Supervisor:", projectSupervisor(), 
          "\nProject Contact:", projectContact())
  })
  
  #Server function for clearing project information when button is clicked
  observeEvent(input$clearproject, {
    # Clear the input fields
    updateTextInput(session, "projectName", value = "")
    updateTextInput(session, "projectDescription", value = "")
    updateTextInput(session, "projectSupervisor", value = "")
    updateTextInput(session, "projectContact", value = "")
    # Reset reactive values
    projectName("")
    projectDescription("")
    projectSupervisor("")
    projectContact("")
    showNotification("Project information has been cleared.", type = "message")
  })
  
  #####2.7 Save input information#####
  #Function for saving datatable once user has uploaded their values
  output$downloadData <- downloadHandler(
    #Save data datble as excel sheet with name "Mass List and Parameters"
    #Will add function to save it with a project identifier as well for organizational purposes
    filename = function() {
      paste("Mass List and Parameters", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Create a list of data frames for each sheet
      write_xlsx(list(
        Parameters = parametersData(),
        'Mass List' = massData(),
        'Reference Mass List' = refMassListData()
      ), path = file)
    }
  )
  
  ####3. Engine tab####
  #Where users upload their data
  
  #Closing bracket
}
  



