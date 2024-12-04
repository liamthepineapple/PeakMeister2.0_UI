#PeakMeister2.0_UI was written by Erick Helmeczi and Liam Surry 
#Initialized 2024-11-12 @McMaster University
#Department of Chemistry and Chemical Biology

#Set maximum file upload size. 
# Set maximum file upload size to 150 GB
options(shiny.maxRequestSize = 150 * 1024^3)
options(shiny.timeout = 600)  # Set timeout to 600 seconds (10 minutes)

#Initialize server
server <- function(input, output, session){ 
  
  # Initialize plotly_data as an empty reactive variable
  plotly_data <- reactiveVal(list())
  
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
    Manual.Indexes = character(),
    stringsAsFactors = FALSE
  ))
  #May need to include peak migfration times into these? Not sure???
  
  #Reactive values for manual MTI list
  userMTI <- reactiveVal(data.frame(
    name = character(),
    left_is = character(),
    right_is = character(),
    description = character(),
    stringsAsFactors = FALSE
  ))
  
  # Render editable tables
  output$massListData <- DT::renderDataTable({ massData() }, editable = TRUE)
  output$refMassListData <- DT::renderDataTable({ refMassListData() }, editable = TRUE)
  output$parametersData <- DT::renderDataTable({ parametersData() }, editable = TRUE)
  output$userMTIdata <- DT::renderDataTable({userMTIdata()}, editable = TRUE)
  
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
      Manual.Indexes = input$Manual.Indexes,
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
    updateCheckboxInput(session, "Manual.Indexes", value = FALSE)
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
  
  #####2.7 Manual MTI input#####
  #Upload file sheet
  observeEvent(input$userMTI, {
    req(input$userMTI)
    userMTI_df <- readxl::read_excel(input$userMTI$datapath, sheet = "User Supplied Migration Indexes") %>% as.data.frame()
    
    #Store file in reactive value
    userMTI(userMTI_df)
  })
  
  #Manual Data Input
  observeEvent(input$addMTIrow, {
    newRow <- data.frame(
      name = input$name,
      left_is = input$left_is,
      right_is = input$right_is,
      description = input$description,
      `1` = input$peak.1.mt,
      `2` = input$peak.2.mt,
      `3` = input$peak.3.mt,
      `4` = input$peak.4.mt,
      `5` = input$peak.5.mt,
      `6` = input$peak.6.mt,
      `7` = input$peak.7.mt,
      `8` = input$peak.8.mt,
      `9` = input$peak.9.mt,
      `10` = input$peak.10.mt,
      `11` = input$peak.11.mt,
      `12` = input$peak.12.mt,
      `13` = input$peak.13.mt,
      stringsAsFactors = FALSE
    )
    #Update dataset
    updatedData <- rbind(userMTI(), newRow)
    userMTI(updatedData)
    
    # Clear inputs values when user adds more things
    updateTextInput(session, "name", value = "")
    updateTextInput(session, "left_is", value = "")
    updateTextInput(session, "right_is", value = "")
    updateTextInput(session, "description", value = "")
    lapply(1:13, function(i) {
      updateNumericInput(session, paste0("ref.peak.", i, ".mt"), value = 0)
    })
  })
  
  #Delete selected MTI row
  observeEvent(input$deleteMTIrow, {
    selected_row <- input$userMTIdata_rows_selected
    if (length(selected_row) > 0) {
      updatedData <- userMTI()[-selected_row, ]
      userMTI(updatedData)
    } else {
      showNotification("Please select a row to delete.", type = "error")
    }
  })
  
  #Clearing the MTI list
  observeEvent(input$clearMTIlist, {
    userMTI(data.frame())
    showNotification("User Supplied Migration Indexes list has been cleared.", type = "message")
  })
  
  #Data table output for userMTI information
  output$userMTIdata <- DT::renderDataTable({
    DT::datatable(userMTI(), selection = 'single', editable = TRUE)})
  
  
  #####2.8 Save input information#####
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
  #####3.1 mz5 file upload#####
  
  # Initialize reactive value to store uploaded files
  uploadedmz5 <- reactiveVal(data.frame(FileName = character(), FilePath = character(), stringsAsFactors = FALSE))

  observeEvent(input$mz5Files, {
    req(input$mz5Files)  
    
    # Get the names and paths of the uploaded files
    newFiles <- data.frame(
      FileName = input$mz5Files$name,
      FilePath = input$mz5Files$datapath,
      stringsAsFactors = FALSE)
    
    # Update the reactive value with the new files
    updatedFiles <- bind_rows(uploadedmz5(), newFiles) 
    uploadedmz5(updatedFiles)

    
    # Render the data table automatically when files are uploaded
    output$fileTable <- DT::renderDataTable({
      DT::datatable(uploadedmz5(), options = list(pageLength = 5))
    })
  })


  
  #####3.2 Initialze Run Button - Main Content######
  #Initialize run button - main engine content 
  observeEvent(input$initialize, {
    if (is.null(input$mz5Files)) {
      shiny::showNotification("File upload required", type = "error")
      return(NULL)}
    
    #Generate a results folder that will not overwrite previous results folders
    count <- 1
    file_name <- paste("Results", Sys.Date(), sep = " ")
    while (dir.exists(file_name)) {
      count <- count + 1
      file_name <- paste("Results", Sys.Date(), count, sep = " ")}
    dir.create(path = file_name, showWarnings = FALSE)
    
    #Create a "Plots" folder to store figures
    dir.create(path = file.path(file_name, "Plots"), showWarnings = FALSE)
    
    #Create a vector of metabolite and internal standard names
    name_vec <- c(refMassListData()$name, massData()$name)
    
    #Check for duplicate metabolite and internal standard names
    if (sum(duplicated(name_vec)) > 0) {
      stop("Mass List and Parameters contain duplicated metabolite or internal standard names.")
    }
    
    #Determine the number of metabolites and internal standards
    num_of_metabolites <- nrow(massData())
    num_of_is <- nrow(refMassListData())
    
    #Get the number of injections
    num_of_injections <- parametersData()$number.of.injections[1]
    
    #Create metabolite sub-folders if plotting is set to "Metabolite"
    if (parametersData()$plot.format == "Metabolite") {
      for (i in 1:length(name_vec)) {
        dir.create(path = file.path(file_name, "Plots", name_vec[i]), showWarnings = FALSE)
      }
    }
    
    #Generate an Excel file of the mass list and parameters
    excel_file_path <- file.path(file_name, paste("Mass List and Parameters", Sys.Date(), ".xlsx", sep = ""))
    
    write_xlsx(list(
      Parameters = parametersData(),
      'Mass List' = massData(),
      'Reference Mass List' = refMassListData()
    ), path = excel_file_path)
    
    #####3.3 Migration Index Calculation#####
    
    # Summarize user supplied migration time data
    metabolites_mt_df <- data.frame(name = massData()$name, massData()[, c((ncol(massData()) - num_of_injections + 1):ncol(massData()))])
    is_mt_df <- subset(refMassListData(), refMassListData()$class == "Reference")
    is_mt_df <- data.frame(name = is_mt_df$name, is_mt_df[, c((ncol(is_mt_df) - num_of_injections + 1):ncol(is_mt_df))])
    
    # Determine IS on the left
    is_left_vec <- character(num_of_metabolites)
    
    for (m in 1:nrow(metabolites_mt_df)) {
      is_temp <- (metabolites_mt_df[m, 2] - (is_mt_df[, 2]))
      is_temp <- set_names(is_temp, is_mt_df$name)
      is_temp <- is_temp[which(sign(is_temp) == 1 | sign(is_temp) == 0)] %>%
        which.min() %>%
        names()
      
      if (length(is_temp) == 0) {
        is_temp <- "none"
      }
      
      is_left_vec[m] <- is_temp
    }
    
    # Determine IS on the right
    is_right_vec <- character(num_of_metabolites)
    
    for (m in 1:nrow(metabolites_mt_df)) {
      is_temp <- (metabolites_mt_df[m, 2] - (is_mt_df[, 2]))
      is_temp <- set_names(is_temp, is_mt_df$name)
      is_temp <- is_temp[which(sign(is_temp) == -1 | sign(is_temp) == 0)] %>%
        which.max() %>%
        names()
      
      if (length(is_temp) == 0) {
        is_temp <- "none"
      }
      
      is_right_vec[m] <- is_temp
    }
    
    # Compute migration index
    mi_df <- matrix(NA, ncol = (num_of_injections + 1), nrow = nrow(metabolites_mt_df)) %>%
      as.data.frame()
    mi_df[, 1] <- metabolites_mt_df$name
    
    summary_vec <- character(nrow(metabolites_mt_df))
    
    for (m in 1:nrow(metabolites_mt_df)) {
      for (i in 2:(num_of_injections + 1)) {
        left <- is_left_vec[m]
        right <- is_right_vec[m]
        
        if (left == "none") {
          mi_df[m, i] <- metabolites_mt_df[m, i] / is_mt_df[which(is_mt_df$name == right), i]
          summary_vec[m] <- right
          next
        }
        
        if (right == "none") {
          mi_df[m, i] <- metabolites_mt_df[m, i] / is_mt_df[which(is_mt_df$name == left), i]
          summary_vec[m] <- left
          next
        }
        
        if (metabolites_mt_df[m, 2] / is_mt_df[which(is_mt_df$name == left), 2] < 1.01) {
          mi_df[m, i] <- metabolites_mt_df[m, i] / is_mt_df[which(is_mt_df$name == left), i]
          summary_vec[m] <- left
          is_right_vec[m] <- "none"
          next
        }
        
        if (metabolites_mt_df[m, 2] / is_mt_df[which(is_mt_df$name == right), 2] > 0.99) {
          mi_df[m, i] <- metabolites_mt_df[m, i] / is_mt_df[which(is_mt_df$name == right), i]
          summary_vec[m] <- right
          is_left_vec[m] <- "none"
          next
        }
        
        mi_df[m, i] <- (metabolites_mt_df[m, i] - is_mt_df[which(is_mt_df$name == left), i]) / 
          (is_mt_df[which(is_mt_df$name == right), i] - is_mt_df[which(is_mt_df$name == left), i])
        summary_vec[m] <- "mi"
      }
    }
    
    # Store results in one data frame
    colnames(mi_df)[2:ncol(mi_df)] <- c(1:num_of_injections)
    mi_df <- cbind(name = massData()$name,
                   left_is = is_left_vec,
                   right_is = is_right_vec,
                   description = summary_vec,
                   mi_df[2:ncol(mi_df)])
    #####3.4 User Supplied MTIs#####
    
    if (parametersData()$Manual.Indexes == "Yes") {
        
        names <- unique(c(user_mti_df$name, user_mti_df$left_is, user_mti_df$right_is, user_mti_df$description))
        
        if (all(names %in% c(name_vec, "mi", "none")) == FALSE) {
          stop("Names detected in User Supplied Migration Indexes.csv not found in Mass List and Parameters.")
        }
        
        if (ncol(user_mti_df) != (4 + num_of_injections)) {
          stop(paste("Make sure User Supplied Migration Indexes.csv contains migration time indexes for all", num_of_injections, "injections."))
        }
        
        for (i in 1:nrow(user_mti_df)) {
          row <- which(mi_df$name == user_mti_df$name[i])
          
          if (length(row) == 0) {
            stop(paste("Metabolite", user_mti_df$name[i], "from User Supplied Migration Indexes.csv is not in Mass List and Parameters."))
          }
          
          mi_df[row, 2:ncol(mi_df)] <- user_mti_df[i, 2:ncol(user_mti_df)]
        }
      }
    
    # Write the migration index summary to a CSV file
    write.csv(mi_df, 
              file.path(file_name, "Migration Index Summary.csv"), 
              row.names = FALSE)
    
    #####3.5 Data File Analysis#####
    # Create vectors for data file directories and name
    data_files <- uploadedmz5()$FilePath
    data_file_names <- uploadedmz5()$FileName  
    

    
    for (d in 1:length(data_files)){
      #Initialize progress bar
      withProgress(message = paste("Processing data file:", data_file_names[d]), value = 0,{ total_steps <- 7
 
      
      print(paste(d, ". ", "Analyzing Data File: ", data_file_names[d], sep = ""))
      
      # Make a copy of the data file as data will be written directly to this file during mass calibration
      
      file <- gsub(".mz5", "", data_files[d])
      
      file.copy(data_files[d], to = paste(file, "temp.mz5", sep = "_"))
      
      # Read copied data file and update progress bar
      
      
      incProgress(1/total_steps, detail = paste("Reading Data File"))
      
      
      print("Reading Data File")
      
      run_data <- readMSData(
        file = paste(file, "temp.mz5", sep = "_"),
        pdata = NULL,
        msLevel = 1,
        verbose = isMSnbaseVerbose(),
        centroided. = FALSE,
        smoothed. = FALSE,
        cache. = 0,
        mode = "inMemory"
      )
      
      print("File Reading Complete")
      
      # Unlock "assayData" environment 
      
      env_binding_unlock(run_data@assayData)
      
      #####3.6 Perform Mass Calibration#####
      
      #This is a stupid line but not sure way around it currently
      mass_df <- massData()
      is_df <- refMassListData()
      parameters_df <- parametersData()
      user_mti_df <- userMTI()
      calibration_response <- parameters_df$apply.mass.calibration
      
      # Confirm response is "Yes" or "No". Otherwise, produce an error.
      
      if(calibration_response != "Yes" & calibration_response != "No"){
        stop(paste("apply.mass.calibration parameter should be 'Yes' or 'No'. ", "'", calibration_response, "' is not an acceptable input.", sep = ""))
      }
      
      # Check if mass calibration should be applied
      
      if (calibration_response == "No"){
        
        print("Skipping Mass Calibration")
        
      }
      
      if (calibration_response == "Yes"){
        
        #Update progress bar
        incProgress(1/total_steps, detail = paste("Performing Mass Calibration"))
        
        print("Performing Mass Calibration")
        
        run_data <- local({
          
          # Define mass window and minimum lock mass counts
          
          mass_window <- parameters_df$ref.mass.window.ppm[1]
          
          minimum_counts <- parameters_df$ref.mass.minimum.counts[1]
          
          # Define a function to calculate 1. experimental m/z of lock masses, 2. the corresponding mass error, and 3. the index of the lock mass
          # The lock mass value is currently the most intense point in the mass window plus an adjustment to better predict the lock mass value.
          
          calibration_parameters <- function(spectrum, lock_mass, minimum_counts, mass_window) {
            
            lock_mass_range <- c((lock_mass - lock_mass * mass_window / 1000000), 
                                 (lock_mass + lock_mass * mass_window / 1000000))
            
            # Find the index of the most intense point in the lock mass window
            
            max_intensity_index <- spectrum %>%
              filter(mz >= lock_mass_range[1] & mz <= lock_mass_range[2]) %>%
              slice_max(intensity) %>%
              pull(index)
            
            if(length(max_intensity_index) == 1){
              if(spectrum$intensity[max_intensity_index] > minimum_counts){
                
                # Create a data frame with all four points
                line_points <- data.frame("mz" = c(spectrum$mz[(max_intensity_index - 2):(max_intensity_index - 1)], 
                                                   spectrum$mz[(max_intensity_index + 1):(max_intensity_index + 2)]),
                                          "intensity" = c(spectrum$intensity[(max_intensity_index - 2):(max_intensity_index - 1)], 
                                                          spectrum$intensity[(max_intensity_index + 1):(max_intensity_index + 2)]))
                
                # Create a model for all four points
                model_left <- lm(formula = intensity ~ mz, data = line_points[c(1,2),])
                model_right <- lm(formula = intensity ~ mz, data = line_points[c(3,4),])
                
                # Get the coefficients for both models
                coefficients_left <- c(model_left$coefficients["mz"], model_left$coefficients["(Intercept)"])
                coefficients_right <- c(model_right$coefficients["mz"], model_right$coefficients["(Intercept)"])
                
                # Calculate the slope and intercept
                slope <- coefficients_left[1] - coefficients_right[1]
                intercept <- coefficients_right[2] - coefficients_left[2]
                
                # Solve for the experimental_mz
                experimental_mz <- solve(slope, intercept)
                
                experimental_mass_diff <- lock_mass - experimental_mz
                
                return(c(experimental_mz, experimental_mass_diff, max_intensity_index))
              }
            }
          }
          
          # Loop through each spectrum to build a model and perform the mass calibration
          
          for (s in 1:end(rtime(run_data))[1]){
            
            spectrum_name <- ls(run_data@assayData)[s]
            
            spectrum <- data.frame(index = 1:length(run_data@assayData[[spectrum_name]]@mz),
                                   mz = run_data@assayData[[spectrum_name]]@mz,
                                   intensity = run_data@assayData[[spectrum_name]]@intensity)
            
            # Lower lock mass
            
            lock_mass <- parameters_df$ref.mass.one[1]
            
            cal_para_1 <- calibration_parameters(spectrum = spectrum,
                                                 lock_mass = lock_mass,
                                                 minimum_counts = minimum_counts,
                                                 mass_window = mass_window)
            
            # Upper lock mass
            
            lock_mass <- parameters_df$ref.mass.two[1]
            
            cal_para_2 <- calibration_parameters(spectrum = spectrum,
                                                 lock_mass = lock_mass,
                                                 minimum_counts = minimum_counts,
                                                 mass_window = mass_window)
            
            if (is.null(cal_para_1) | is.null(cal_para_2)){
              next
            }
            
            ######3.6.1 Develop correction model######
            
            model_data <- data.frame("x" = c(cal_para_1[1], cal_para_2[1]),
                                     "y" = c(cal_para_1[2], cal_para_2[2]))
            
            model <- lm(y ~ x, model_data)
            
            ######3.6.2 Apply correction model######
            
            correction_vector <- c(model[["coefficients"]][["x"]] * spectrum$mz) + model[["coefficients"]][["(Intercept)"]]
            
            correction_vector[1:cal_para_1[3]] <- cal_para_1[2]
            
            correction_vector[cal_para_2[3]:length(correction_vector)] <- cal_para_2[2]
            
            run_data@assayData[[spectrum_name]]@mz <- run_data@assayData[[spectrum_name]]@mz + correction_vector
            
          }
          
          run_data
          
        })
      }
      
      
      print("Mass Calibration Complete")
      
     #####3.7 Extract Electropherograms#####
      
      #Update progress bar
      incProgress(1/total_steps, detail = paste("Extracting Electropherograms"))
      
      print("Extracting Electropherograms")
      
      # Define mass error in ppm
      
      mass_error_vec <- c(is_df$extraction.window.ppm, mass_df$extraction.window.ppm)
      
      # Create a matrix of minimum and maximum m/z values for each internal standard and metabolite
      
      mz_vec <- c(is_df$mz, mass_df$mz)
      
      min <- mz_vec - mz_vec * mass_error_vec / 1000000
      max <- mz_vec + mz_vec * mass_error_vec / 1000000
      mzr <- matrix(c(min, max), ncol = 2)
      
      # Extract electropherograms
      
      electropherograms <- chromatogram(run_data,
                                        mz = mzr,
                                        rt = c(0,end(rtime(run_data))),
                                        aggregationFun = "mean",
                                        missing = 0,
                                        msLevel = 1)
      
      # Create a data frame of migration times and intensities with electropherograms data
      
      eie_df <- data.frame("mt.seconds" = electropherograms[1]@rtime)
      
      for (n in 1:length(name_vec)){
        temp_df <- data.frame(electropherograms[n]@intensity)
        colnames(temp_df) <- paste(name_vec[n], "intensity", sep = " ")
        eie_df <- cbind(eie_df,temp_df)
      }
      
  
      print("Extraction Complete")
      
      #####3.8 Smooth Intensity Vectors#####
      
      # Check if mass calibration should be applied
      
      smoothing_response <- parameters_df$apply.smoothing
      
      # Confirm response is "Yes" or "No". Otherwise, produce an error.
      
      if(smoothing_response != "Yes" & smoothing_response != "No"){
        stop(paste("apply.smoothing parameter should be 'Yes' or 'No'. ", "'", smoothing_response, "' is not an acceptable input.", sep = ""))
      }
      
      if (smoothing_response == "No"){
        
        print("Skipping Mass Calibration")
        
      }
      
      if (smoothing_response == "Yes"){
        
        #Update progress bar
        incProgress(1/total_steps, detail = paste("Smoothing Electropherograms"))
        
        print("Smoothing Electropherograms")
        
        
        smoothing_kernel_vec <- c(is_df$smoothing.kernel, mass_df$smoothing.kernel)
        smoothing_strength_vec <- c(is_df$smoothing.strength, mass_df$smoothing.strength)
        
        for (n in 1:length(name_vec)){ 
          Smooth <- with(eie_df, 
                         ksmooth(x = mt.seconds, 
                                 y = eie_df[,n + 1], 
                                 kernel = smoothing_kernel_vec[n], 
                                 bandwidth = smoothing_strength_vec[n]))
          eie_df[,n + 1] <- Smooth[["y"]]
        }
        
        # Clean-up global environment
        
        rm(list = c("electropherograms", "mzr", "Smooth", "temp_df", "max", "min", 
                    "n", "mass_error_vec", "run_data", "smoothing_strength_vec",
                    "smoothing_kernel_vec"))
        
      }
      
      renderText("Electropherograms Smoothing Complete")
      print("Electropherograms Smoothing Complete")
      
      #####3.9 Internal Standard Peak Detection, Integration, and Filtering#####
      
      #Update progress bar
      incProgress(1/total_steps, detail = paste("Performing Peak Picking and Filtering for Internal Standards"))
      
      print("Performing Peak Picking and Filtering for Internal Standards")
      
      ######3.9.1 Peak detection###### 
      
      is_peaks_df <- local({
        
        n <- 1
        
        for (s in 1:num_of_is){
          
          rle_output <- eie_df[,s+1] %>%
            diff() %>%
            sign() %>%
            rle()
          
          consecutive_runs <- which(rle_output$lengths > n & rle_output$values == 1)
          consecutive_runs <- subset(consecutive_runs, (consecutive_runs + 1) %in% (which(rle_output$lengths > n)) == TRUE)
          
          run_lengths <- cumsum(rle_output$lengths) + 1
          
          start <- eie_df$mt.seconds[run_lengths[consecutive_runs - 1]]
          apex <- eie_df$mt.seconds[run_lengths[consecutive_runs]]
          end <- eie_df$mt.seconds[run_lengths[consecutive_runs + 1]]
          
          # For FWHM calculations I will also add intensity values here as well
          
          start_intensity <- eie_df[run_lengths[consecutive_runs - 1], (s+1)]
          apex_intensity <- eie_df[run_lengths[consecutive_runs], (s+1)]
          end_intensity <- eie_df[run_lengths[consecutive_runs + 1], (s+1)]
          
          # Account for peaks that start immediately during the analysis
          
          if(length(start) != length(apex)){
            start <- append(start, 0, 0)
            start_intensity <- append(start_intensity, 0, 0)
          }
          
          # Create a data frame containing the start, apex, and end migration times of each peak
          
          peak_df <- data.frame(start,
                                apex,
                                end,
                                start_intensity,
                                apex_intensity,
                                end_intensity)
          
          ######3.9.2 Migration time filtering######
          
          # Filter peaks that are outside migration time limits
          
          peak_df <- subset(peak_df, peak_df$apex >= is_df$min.mt.min[s] * 60 & peak_df$apex <= is_df$max.mt.min[s] * 60)
          
          ######3.9.3 Integrate peaks######
          
          peak_area_vector = c(1:nrow(peak_df))
          
          # If the length of peak_area_vector is less than the number of injections, 
          # print an error and suggest a solution
          
          if (length(peak_area_vector) < num_of_injections){
            cat(paste("Warning: ", name_vec[s], " EIE contains insufficient data. If an error occurs try:
        1. Increasing the extraction.window.ppm parameter 
        2. Adjusting min.mt.min and max.mt.min parameters", sep =""))
          }
          
          for (p in 1:nrow(peak_df)){
            
            peak_area_vector[p] <- AUC(eie_df$mt.seconds,
                                       eie_df[,s+1],
                                       method = "trapezoid",
                                       from = peak_df[p,1],
                                       to = peak_df[p,3],
                                       absolutearea = FALSE,
                                       na.rm = FALSE)
            
            # Perform baseline correction
            
            peak_area_vector[p] <- peak_area_vector[p] - (peak_df[p,3] - peak_df[p,1]) * min(peak_df[p,4], peak_df[p,6])
          }
          
          peak_df <- cbind(peak_df, peak_area_vector)
          
          # rename peak_df columns
          
          colnames(peak_df) <- c(paste(name_vec[s], "start.seconds", sep = "."),
                                 paste(name_vec[s], "apex.seconds", sep = "."),
                                 paste(name_vec[s], "end.seconds", sep = "."),
                                 paste(name_vec[s], "start_intensity", sep = "."),
                                 paste(name_vec[s], "apex_intensity", sep = "."),
                                 paste(name_vec[s], "end_intensity", sep = "."),
                                 paste(name_vec[s], "peak.area", sep = "."))
          
          # Retain peak_df for future filtering steps
          
          peak_df_fill <- peak_df
          
          ######3.9.4 FWHM filtering######
          
          # Do not not apply the FWHM filter if the number of injections is equal to 1
          
          if (parameters_df$number.of.injections != 1) {
            
            # Find the peak intensity at half the peak height
            
            intensity_fwhm <- peak_df[,4] + (peak_df[,5] - peak_df[,4])/2 
            
            # Find the migration times closest to these intensities within each peak
            
            fwhm_vec <- vector()
            single_eie <- eie_df[,c(1,s+1)]
            
            for(p in 1:nrow(peak_df)){
              
              single_eie_temp <- subset(single_eie, single_eie$mt.seconds >= peak_df[p,1] & single_eie$mt.seconds <= peak_df[p,2])
              fwhm_mt_left <- single_eie_temp$mt.seconds[which.min(abs(single_eie_temp[,2] - intensity_fwhm[p]))]
              
              single_eie_temp <- subset(single_eie, single_eie$mt.seconds <= peak_df[p,3] & single_eie$mt.seconds >= peak_df[p,2])
              fwhm_mt_right <- single_eie_temp$mt.seconds[which.min(abs(single_eie_temp[,2] - intensity_fwhm[p]))]
              
              fwhm_vec <- append(fwhm_vec, fwhm_mt_right - fwhm_mt_left)
              
            }
            
            peak_df$fwhm <- fwhm_vec
            
            # Determine the fwhm of the peaks (n = num_of_injections) with the greatest area
            
            df_temp <- peak_df[order(-peak_df[,8]),]
            df_temp <- df_temp[1:num_of_injections,]
            
            fwhm_cutoff <- median(df_temp$fwhm)
            
            peak_df <- subset(peak_df, peak_df$fwhm <= (fwhm_cutoff * is_df$peak.fwhm.tolerance.multiplier[s]))
            peak_df <- peak_df[,c(1:7)]
            
          }
          
          # subset peak_df so that only the peaks (n = number.of.injections) with the greatest area are kept
          
          cut_off <- sort(peak_df[,7], decreasing = TRUE)[num_of_injections]
          
          peak_df <- subset(peak_df, peak_df[,7] >= cut_off)
          
          ######3.9.5 Peak space filtering######
          
          # Do not apply peak space filtering if the number of injections is equal to 1
          
          if(num_of_injections != 1){
            
            # Determine the upper and lower migration time limits for space between peaks
            
            median_space <- peak_df[,2] %>%
              diff() %>%
              median()
            
            median_space_tol <- is_df$peak.space.tolerance.percent[s] / 100
            
            median_space_lower_lim <- median_space - median_space * median_space_tol
            median_space_upper_lim <- median_space + median_space * median_space_tol
            
            # Check if peaks migrate within the tolerance limits
            
            peak_space_tol_check <- between(diff(peak_df[,2]), median_space_lower_lim, median_space_upper_lim)
            
            if(all(peak_space_tol_check) != TRUE){
              bad_space <- which(peak_space_tol_check == FALSE)
            }else{
              bad_space <- NA
            }
            
            ######3.9.6 Scenario 1######
            # Only one bad space is detected
            
            if(length(bad_space) == 1 & is.na(bad_space[1]) == FALSE){
              
              false_peak_diff <- peak_df[num_of_injections, 2] - peak_df[(num_of_injections - 1), 2]
              
              # This algorithm always assumes the final peak is false - likely due to carryover
              # It is possible the first peak is false but this seems less likely
              # Final peak only removed if case 1 does not produce a duplicate
              
              # Case 1- An interior peak is missing (usually a blank)
              
              if(bad_space != (num_of_injections - 1)){
                
                # Since the we know the bad space is not at the end, use the space after the bad space to find the expected apex
                
                expected_peak_apex <- peak_df[bad_space[1],2] + peak_df[(bad_space[1] + 2),2] - peak_df[(bad_space[1] + 1),2]
                
              }
              
              # Case 2 -  Final space is false and less than median - suspect that true final peak was missed
              
              if(bad_space == (num_of_injections - 1) & false_peak_diff < median_space_upper_lim){
                expected_peak_apex <- peak_df[(num_of_injections - 1 ),2] + median_space
              }
              
              # Case 3 - Final space is false and greater than median - suspect that peak 1 was missed
              
              if(bad_space == (num_of_injections - 1) & false_peak_diff > median_space_lower_lim){
                expected_peak_apex <- peak_df[1,2] - median_space
              }
              
              peak <- which.min(abs(peak_df_fill[,2] - expected_peak_apex))
              
              # If the nearest peak is too far from the expected migration time use a place holder
              
              if (abs(peak_df_fill[peak,2] - expected_peak_apex) > (median_space / 2)){
                
                nearest_mt <- (eie_df$mt.seconds - expected_peak_apex) %>%
                  abs() %>%
                  which.min(.)
                
                peaks <- data.frame(eie_df[nearest_mt,1],
                                    eie_df[nearest_mt,1],
                                    eie_df[nearest_mt,1],
                                    eie_df[nearest_mt,s + 1],
                                    eie_df[nearest_mt,s + 1],
                                    eie_df[nearest_mt,s + 1],
                                    0)
                
                colnames(peaks) <- colnames(peak_df)
                
                peak_df <- rbind(peak_df, peaks)
                
                peak_df <- peak_df[order(peak_df[,2]),]
                
              }else{
                
                peak_df <- rbind(peak_df, peak_df_fill[peak,])
                
                peak_df <- peak_df[order(peak_df[,2]),]
                
              }
              
              # Remove bad peak
              
              if (any(duplicated(peak_df[,2]))){
                
                peak_df <- peak_df[-c(which(duplicated(peak_df[,2]))),]
                
              }else{
                
                peak_df <- peak_df[1:(num_of_injections),]
                
              }
            }
            
            ######3.9.7 Scenario 2###### 
            #Two or three bad spaces are detected 
            
            if(length(bad_space) == 2 | length(bad_space) == 3){
              
              peaks_to_check <- c(bad_space, (bad_space + 1)) %>%
                unique() %>%
                sort()
              
              # Outside peaks must be checked last
              
              peaks_to_check <- c(peaks_to_check[-1], peaks_to_check[1])
              
              # Loop through each suspect bad peak and remove them iteratively until the number of bad spaces reaches 1
              
              for (p in 1:length(peaks_to_check)){
                
                num_bad_space <- peak_df[,2] %>%
                  .[-c(peaks_to_check[p])] %>%
                  diff(.) %>%
                  between (median_space_lower_lim, median_space_upper_lim) 
                num_bad_space <- length(which(num_bad_space == FALSE))
                
                if (num_bad_space == 1){
                  peak_df <- peak_df[-c(peaks_to_check[p]),]
                  break
                }
              }
              
              # To avoid errors where removing a peak results in 0 bad spaces only fill
              # in gap if nrow(peak_df) == number of injections - 1
              
              if(nrow(peak_df) == (num_of_injections - 1)){
                
                # Find the peak gap and calculate the expected migration time for the missing peak
                
                gap <- which(between(diff(peak_df[,2]), median_space_lower_lim, median_space_upper_lim) == FALSE)
                
                expected_peak_apex <- (peak_df[(gap[1] + 1),2] - peak_df[gap[1],2])/2 + peak_df[gap[1],2]
                
                # Find the nearest peak in the peak_df_fill data frame to the expected migration time
                # Avoid duplicate peaks by not using exisitng peaks in peak_df
                
                peak_df_fill <- subset(peak_df_fill, !(peak_df_fill[,2] %in% peak_df[,2]))
                
                peak <- which.min(abs(peak_df_fill[,2] - expected_peak_apex))
                peak_df <- rbind(peak_df, peak_df_fill[peak,])
                
                peak_df <- peak_df[order(peak_df[,2]),]
                
              }
            }
          }
          
          # Summarize peak_df data in is.peak_df
          
          if(s == 1){
            is_peaks_df = peak_df
          }else{
            is_peaks_df = cbind(is_peaks_df, peak_df)  
          }
        }
        
        is_peaks_df
        
      })
      
      # Make a data frame containing the apex migration times of the internal standards
      # to be used to filter metabolite peaks
      
      is_mt_df <- is_peaks_df[,seq(from = 2,
                                   to = ncol(is_peaks_df),
                                   by = 7)]
      
      print("Peak Picking and Filtering for Internal Standards Complete")
      
      #####3.10 Metabolite  Peak Detection, Integration, and Filtering#####
      
      #Update progress bar
      incProgress(1/total_steps, detail = paste("Performing Peak Picking and Filtering for Analytes"))
      
      print("Performing Peak Picking and Filtering for Analytes")
      
      ######3.10.1 Peak detection###### 
      
      metabolite_peaks_df <- local ({
        
        for (m in (num_of_is + 1):length(name_vec)){
          
          peak_df <- data.frame()
          
          # Determine the start, apex, and end of peaks. Use the user defined value "n" to detect peaks.
          # If n results in fewer peaks then injection, decrease n by 1 and repeat
          
          n <- 1
          
          rle_output <- eie_df[,m + 1] %>%
            diff() %>%
            sign() %>%
            rle()
          
          consecutive_runs <- which(rle_output$lengths > n & rle_output$values == 1)
          consecutive_runs <- subset(consecutive_runs, (consecutive_runs + 1) %in% (which(rle_output$lengths > n)) == TRUE)
          
          run_lengths <- cumsum(rle_output$lengths) + 1
          
          start <- eie_df$mt.seconds[run_lengths[consecutive_runs - 1]]
          apex <- eie_df$mt.seconds[run_lengths[consecutive_runs]]
          end <- eie_df$mt.seconds[run_lengths[consecutive_runs + 1]]
          
          # I will also add intensity values here as well
          
          start_intensity <- eie_df[run_lengths[consecutive_runs - 1], (m+1)]
          apex_intensity <- eie_df[run_lengths[consecutive_runs], (m+1)]
          end_intensity <- eie_df[run_lengths[consecutive_runs + 1], (m+1)]
          
          # Account for peaks that start immediately during the analysis
          
          if(length(start) != length(apex)){
            start <- append(start, 0, 0)
            start_intensity <- append(start_intensity, 0, 0)
          }
          
          # Create a data frame containing the start, apex, and end migration times of each 
          # peak in addition to required intensities for FWHM calculations
          
          peak_df <- data.frame(start,
                                apex,
                                end,
                                start_intensity,
                                apex_intensity,
                                end_intensity)
          
          ######3.10.2 Filter peaks by peak width######
          
          # Define a minimum peak width cut off in seconds. Remove peaks with a width <= cutoff
          
          min_width_cut_off <- mass_df$minimim.peak.width.seconds[m - num_of_is]
          
          peak_df <- subset(peak_df, (peak_df$end - peak_df$start) >= min_width_cut_off)
          
          ######3.10.3 Integrate peaks######
          
          peak_area_vector = c(1:nrow(peak_df))
          
          # If the length of peak_area_vector is less than the number of injections, 
          # print an error and suggest a solution
          
          if (length(peak_area_vector) < num_of_injections){
            cat(paste("Warning: ", name_vec[m], " EIE contains insufficient data. If an error occurs try:
        1. Increasing the extraction.window.ppm parameter 
        2. Decreasing the minimim.peak.width.seconds parameter", sep =""))
          }
          
          for (p in 1:nrow(peak_df)){
            
            peak_area_vector[p] <- AUC(eie_df$mt.seconds,
                                       eie_df[,m+1],
                                       method = "trapezoid",
                                       from = peak_df[p,1],
                                       to = peak_df[p,3],
                                       absolutearea = FALSE,
                                       na.rm = FALSE)
            
            peak_area_vector[p] <- peak_area_vector[p] - (peak_df[p,3] - peak_df[p,1]) * min(peak_df[p,4], peak_df[p,6])
          }
          
          peak_df <- cbind(peak_df, peak_area_vector)
          
          # rename peak_df columns
          
          colnames(peak_df) <- c(paste(name_vec[m], "start.seconds", sep = "."),
                                 paste(name_vec[m], "apex.seconds", sep = "."),
                                 paste(name_vec[m], "end.seconds", sep = "."),
                                 paste(name_vec[m], "start_intensity", sep = "."),
                                 paste(name_vec[m], "apex_intensity", sep = "."),
                                 paste(name_vec[m], "end_intensity", sep = "."),
                                 paste(name_vec[m], "peak.area", sep = "."))
          
          ######3.10.4 Filter peaks######
          
          # Filter peaks based on smallest mt difference 
          
          # Determine the expected migration times of the metabolites
          
          n <- m - num_of_is
          left_is <- paste(mi_df$left_is[n], ".apex.seconds", sep ="")
          right_is <- paste(mi_df$right_is[n], ".apex.seconds", sep ="")
          rmt_is <- paste(mi_df$description[n], ".apex.seconds", sep ="")
          
          if(mi_df$description[n] == "mi"){
            
            mf_vec <- mi_df[n,5:ncol(mi_df)] %>%
              unlist() %>%
              unname()
            
            expected_mt <- mf_vec * (is_mt_df[,which(colnames(is_mt_df) == right_is)] - is_mt_df[,which(colnames(is_mt_df) == left_is)]) + is_mt_df[,which(colnames(is_mt_df) == left_is)]
            
          }else{
            
            mf_vec <- mi_df[n,5:ncol(mi_df)] %>%
              unlist() %>%
              unname()
            
            expected_mt <- mf_vec * is_mt_df[,which(colnames(is_mt_df) == rmt_is)] 
            
          }
          
          # Filter peak_df for peaks within migration time tolerance
          
          migration_window <- mass_df$migration.window.seconds[m - num_of_is]
          
          for (i in 1:num_of_injections){
            
            peaks <- peak_df %>%
              filter(., peak_df[,2] <= expected_mt[i] + migration_window & 
                       peak_df[,2] >= expected_mt[i] - migration_window)
            
            # If more than one peak is found choose the nearest one
            
            if(nrow(peaks) > 1){
              peaks <- (peak_df[,2] - expected_mt[i]) %>%
                abs() %>%
                which.min(.)
              peaks <- peak_df[peaks,]
            }
            
            # If no peak is found, generate a place holder
            
            if(nrow(peaks) == 0){
              
              nearest_mt <- (eie_df$mt.seconds - expected_mt[i]) %>%
                abs() %>%
                which.min(.)
              
              peaks <- data.frame(eie_df[nearest_mt,1],
                                  eie_df[nearest_mt,1],
                                  eie_df[nearest_mt,1],
                                  eie_df[nearest_mt,m + 1],
                                  eie_df[nearest_mt,m + 1],
                                  eie_df[nearest_mt,m + 1],
                                  0)
              
              colnames(peaks) <- colnames(peak_df)
            }
            
            if(i == 1){
              filtered_peaks_df <- peaks
            }else{
              filtered_peaks_df <- rbind(filtered_peaks_df, peaks)
            }
          }
          
          ######3.10.5 Filter peaks outside of run time limits######
          
          # Set a place holder for peaks where the expected migration time > total run time
          
          total_run_time <- eie_df$mt.seconds[nrow(eie_df)]
          
          late_peaks <- (expected_mt > total_run_time) %>%
            which()
          
          # Find migration times to use as placeholders that do not belong to other identified peaks
          
          mt <- tail(eie_df$mt.seconds, n = 15)
          
          mt <- mt[!(mt %in% filtered_peaks_df[,2])]
          
          for (i in late_peaks){
            mt_temp <- mt[i]
            filtered_peaks_df[i,] <- c(mt_temp,
                                       mt_temp,
                                       mt_temp,
                                       eie_df[which(eie_df$mt.seconds == mt_temp) ,m + 1],
                                       eie_df[which(eie_df$mt.seconds == mt_temp) ,m + 1],
                                       eie_df[which(eie_df$mt.seconds == mt_temp) ,m + 1],
                                       0)
          }
          
          
          ######3.10.6 Filter duplicated peaks######
          
          # If the same peak is assigned to multiple injection numbers, reapply rmt filter with more austere rmt tolerances
          # New rmt tolerance will be the original / count, which starts at 2 and increases by 1 each iteration
          
          count = 2
          
          while (any(duplicated(filtered_peaks_df[,2])) & count < 100){
            
            strict_migration_window <- migration_window/count
            
            # find rows with duplicated values 
            
            duplicate_location <- filtered_peaks_df[,2] %>%
              duplicated() %>%
              which()
            
            duplicate_rows <- which(filtered_peaks_df[,2] %in% filtered_peaks_df[duplicate_location,2])
            
            # reapply filtering for these peaks with the more strict rmt tolerance
            
            for (r in duplicate_rows){
              
              peaks <- peak_df %>%
                filter(., peak_df[,2] <= expected_mt[r] + strict_migration_window & 
                         peak_df[,2] >= expected_mt[r] - strict_migration_window)
              
              # If more than one peak is found choose the nearest one
              
              if(nrow(peaks) > 1){
                peaks <- (peak_df[,2] - expected_mt[r]) %>%
                  abs() %>%
                  which.min(.)
                peaks <- peak_df[peaks,]
              }
              
              # If no peak is found, generate a place holder
              
              if(nrow(peaks) == 0){
                
                nearest_mt <- (eie_df$mt.seconds - expected_mt[r]) %>%
                  abs() %>%
                  which.min(.)
                
                peaks <- data.frame(eie_df[nearest_mt,1],
                                    eie_df[nearest_mt,1],
                                    eie_df[nearest_mt,1],
                                    eie_df[nearest_mt,m + 1],
                                    eie_df[nearest_mt,m + 1],
                                    eie_df[nearest_mt,m + 1],
                                    0)
                
                colnames(peaks) <- colnames(peak_df)
              }
              
              filtered_peaks_df[r,] <- peaks
              
            }
            
            count = count + 1
          }
          
          # If the duplicate peak filter fails (count = 100) then generate a place holder peak_df
          # and generate a warning that the filter failed. This filter fails when two or more expected migration 
          # times are too close to each other.
          
          if (count == 100){
            
            mt.temp <- eie_df$mt.seconds[seq(1, num_of_injections * 10, 10)]
            
            filtered_peaks_df[,c(1:3)] <- mt.temp
            filtered_peaks_df[,c(4:7)] <- 0
            
          }
          
          ######3.10.7 Filter using peak spaces######
          
          # Do not apply peak space filtering if the number of injections is equal to 1
          
          if(num_of_injections != 1){
            
            # Get peak space tolerance
            
            space_tol <- mass_df$peak.space.tolerance.percent[m - num_of_is] / 100
            
            # Make a vector containing all the expected space lengths
            
            space_vec <- expected_mt %>%
              diff()
            
            # Define upper and lower peak space limits
            
            space_lower_lim <- space_vec - space_vec * space_tol
            space_upper_lim <- space_vec + space_vec * space_tol
            
            # Check if peaks migrate within the tolerance limits
            
            peak_space_tol_check <- between(diff(filtered_peaks_df[,2]), space_lower_lim, space_upper_lim)
            
            if(all(peak_space_tol_check) != TRUE){
              bad_space <- which(peak_space_tol_check == FALSE)
            }else{
              bad_space <- NA
            }
            
            # identify which peaks are potentially incorrectly assigned (bad peaks)
            # these are peaks before and after each bad space
            
            bad_peaks <- c(bad_space, bad_space + 1) %>%
              unique() %>%
              sort()
            
            # Define a count that will be used to modify the peak space tolerance
            
            count = 4
            
            # Keep unaltered filtered peaks data frame for the event that the peak space algorithm fails
            
            filtered_peaks_df_retain <- filtered_peaks_df
            
            # set count limit to determine when the algorithm fails
            
            count_limit = 100
            
            # Identify bad peaks, and replace them with peaks meeting peak space criteria
            # If the number of bad peaks is equal to the number of injections, do not apply this filter
            
            while(length(bad_peaks) > 0 & length(bad_peaks) < (num_of_injections - 1) & count < count_limit){
              
              # define remaining peaks which are correctly assigned (good peaks)
              
              good_peaks <- c(1:num_of_injections) %>%
                setdiff(., c(bad_peaks))
              
              # Use the median of the expected peak space times to find peaks
              
              peak_tolerance <- expected_mt %>%
                diff() %>%
                median () / count
              
              for (b in 1:length(bad_peaks)){
                
                # find the nearest good peak neighbor for each bad peak
                
                nearest_good_peak <- (good_peaks - bad_peaks[b]) %>%
                  abs() %>%
                  which.min()
                
                # calculate the expected migration time 
                
                expected_mt <- filtered_peaks_df[good_peaks[nearest_good_peak], 2] - 
                  (good_peaks[nearest_good_peak] - bad_peaks[b]) * median(space_vec)
                
                # find peaks nearest to the expected migration time within the tolerance
                
                peaks <- peak_df %>%
                  filter(., peak_df[,2] <= expected_mt + peak_tolerance & peak_df[,2] >= expected_mt - peak_tolerance)
                
                # if more than one peak is found, select the closest one
                
                if(nrow(peaks) > 1){
                  peaks <- (peak_df[,2] - expected_mt) %>%
                    abs() %>%
                    which.min(.)
                  peaks <- peak_df[peaks,]
                }
                
                # if no peaks are found, define a place holder
                
                if(nrow(peaks) == 0){
                  
                  nearest_mt <- (eie_df$mt.seconds - expected_mt) %>%
                    abs() %>%
                    which.min(.)
                  
                  peaks <- data.frame(eie_df[nearest_mt,1],
                                      eie_df[nearest_mt,1],
                                      eie_df[nearest_mt,1],
                                      eie_df[nearest_mt,m + 1],
                                      eie_df[nearest_mt,m + 1],
                                      eie_df[nearest_mt,m + 1],
                                      0)
                  
                  colnames(peaks) <- colnames(peak_df)
                  
                  filtered_peaks_df[bad_peaks[b],] <- peaks
                }
                
                # if only one peak is found
                
                filtered_peaks_df[bad_peaks[b],] <- peaks
                
              }
              
              count = count + 1
              
              duplicate_location <- filtered_peaks_df[,2] %>%
                duplicated() %>%
                which()
              
              # bad peaks correspond to any rows that are not unique
              
              bad_peaks <- which(filtered_peaks_df[,2] %in% filtered_peaks_df[duplicate_location,2])
              
            }
            
            # if algorithm failed, revert back to filtered_peaks_df
            
            if (count == count_limit){
              
              filtered_peaks_df <- filtered_peaks_df_retain
              
            }
          }
          
          # Summarize filtered.peak_df data in metabolite_peak_df
          
          if(m == (num_of_is + 1)){
            metabolite_peaks_df = filtered_peaks_df
          }else{
            metabolite_peaks_df = cbind(metabolite_peaks_df, filtered_peaks_df)  
          }
          
        }
        
        metabolite_peaks_df
        
      })
      
      ######3.10.8 Filter peaks below LOD######
      
      # Build a data frame to store comments for each metabolite peak
      
      comment_df <- matrix(nrow = num_of_injections, ncol = num_of_metabolites, "") %>%
        as.data.frame
      
      colnames(comment_df) <- mass_df$name
      
      # Loop through each metabolite and see if its area is below the LOD threshold
      
      for (m in 1:num_of_metabolites){
        
        # Determine the noise of the electropherogram
        # Fine the noise levels in 60 seconds intervals
        
        region_start <- seq(1, nrow(eie_df), 60)
        region_end <- seq(60, nrow(eie_df), 60)
        length(region_start) <- length(region_end)
        
        # Generate a vector to store noise data
        
        noise_vec <- rep(NA, length(region_start))
        
        # Define a function to calculate noise
        
        noise_calculation <- function(temp_noise) {
          mean(temp_noise) + mass_df$snr.threshold[m] * sd(temp_noise)
        }
        
        # Calculate the noise in each region
        
        for (r in 1:length(region_start)){
          temp_noise <- eie_df[region_start[r]:region_end[r], m + num_of_is + 1]
          noise_vec[r] <- noise_calculation(temp_noise)
        }
        
        # Define the noise as the 20th percentile noise region
        
        noise <- noise_vec %>%
          sort()
        
        noise <- noise[as.integer(length(noise)/5)]
        
        peak_area_df <- metabolite_peaks_df[,seq(7, ncol(metabolite_peaks_df), 7)]
        
        comment_df[,m] <- ifelse(peak_area_df[,m] < noise, "<LOD", comment_df[,m])
        
        ### Annotate injections that are not detected
        
        comment_df[,m] <- ifelse(peak_area_df[,m] == 0, "NPD", comment_df[,m])
        
      }
      
      ######3.10.9 Filter interfered peaks######
      
      # Build an interference data frame since some are metabolites and some are internal standards
      
      interference_df <- cbind(is_peaks_df[,seq(2, ncol(is_peaks_df), 7)],
                               metabolite_peaks_df[,seq(2, ncol(metabolite_peaks_df), 7)])
      
      for (m in 1:num_of_metabolites){
        
        # Skip metabolites with no reported interference
        
        if(is.na(mass_df$interference[m])){
          next
        }
        
        # Get the names of the interferences from mass_df
        
        interferences <- strsplit(mass_df$interference[m], ", ") %>%
          unlist()
        
        for (k in 1:length(interferences)){
          
          interference <- paste(interferences[k], ".apex.seconds", sep = "")
          
          # Check if interference appears as a metabolite or internal standard.
          # If not, provide a warning
          
          if(!(interferences[k] %in% name_vec)){
            print(paste("Error: ", "Interference ", interferences[k], " is not an analyte or internal standard", sep = ""))
          }
          
          # Check if a interference window was provided. If not, produce an error.
          
          if(is.na(mass_df$interference.comigration.threshold.seconds[m])){
            print(paste("Error: No interference.comigration.threshold.seconds provided for ", mass_df$name[m], sep = ""))
          }
          
          # See if there is any overlap between the metabolite peak and its interference 
          
          metabolite_name <- paste(mass_df$name[m], ".apex.seconds", sep = "")
          
          for (i in 1:num_of_injections){
            for (j in 1:num_of_injections){
              
              diff_temp <- (metabolite_peaks_df[i,metabolite_name] - interference_df[j,interference]) %>%
                abs() 
              
              comment_df[i,mass_df$name[m]] <- ifelse(diff_temp < mass_df$interference.comigration.threshold.seconds[m], "Interfered", comment_df[i,mass_df$name[m]])
            }
          }
        }
      }
      
      # Combine internal standard and metabolite data frames for plotting
      
      peaks_df <- cbind(is_peaks_df, metabolite_peaks_df)
      
      # update comment data frame account for internal standards
      
      is_comment_df <- matrix(nrow = num_of_injections, ncol = nrow(is_df), "") %>%
        as.data.frame()
      
      colnames(is_comment_df) <- is_df$name
      
      comment_df <- cbind(is_comment_df, comment_df)
      

      print("Peak Picking and Filtering for Analytes Complete")
      
      #####3.11 Plotting#####
      
 
      
      incProgress(1/total_steps, detail = paste("Plotting & Exporting Electropherograms"))
      print("Plotting Electropherograms")
      
      # Make a list to save plots to
      
      plot_list <- vector("list", length(name_vec))
      names(plot_list) <- name_vec
      
      for (n in 1:length(name_vec)){
        
        ######3.11.1 Create annotation and peak fill data frame######
        
        peak_mt_df <- peaks_df[,seq(from = 2, to = ncol(peaks_df), by = 7)]
        
        ann_df <- data.frame("peak.number" = c(1:num_of_injections),
                             "comment" = comment_df[,n],
                             "peak.apex.seconds" = peak_mt_df[,n],
                             "peak.height.counts" = eie_df[which(eie_df$mt.seconds %in% (peak_mt_df[,n])),n+1])
        
        #Create peak fill data frame
        
        pf_df <- data.frame("peak.number" = 1,
                            "mt.seconds" = eie_df[,1],
                            "intensity" = eie_df[,n+1])
        
        mt_vec <- vector()
        start_df <- peaks_df[,seq(from = 1, to = ncol(peaks_df), by = 7)]
        end_df <- peaks_df[,seq(from = 3, to = ncol(peaks_df), by = 7)]
        
        for (i in 1:num_of_injections){
          
          # Create a migration time vector to track where peaks elute
          
          if(comment_df[i,n] == ""){
            mt_vec_temp <- eie_df$mt.seconds[between(eie_df$mt.seconds, start_df[i,n], end_df[i,n])]
            mt_vec <- append(mt_vec, mt_vec_temp)
            
            # Update peak.number in pf_df
            
            pf_df$peak.number <- ifelse(pf_df$mt.seconds >= start_df[i,n], i, pf_df$peak.number)
            pf_df$peak.number <- as.factor(pf_df$peak.number)
          }else{
            next
          }
        }
        
        pf_df$intensity <- ifelse(pf_df$mt.seconds %in% mt_vec == TRUE, pf_df$intensity , 0)
        
        ## Add baseline intensity
        
        pf_df$baseline <- 0
        
        for (i in 1:num_of_injections){
          if(comment_df[i,n] == ""){
            lower_intensity <- min(c(peaks_df[i, n * 7 - 3], peaks_df[i, n * 7 - 1]))
            pf_df$baseline <- ifelse(pf_df$mt.seconds >= start_df[i,n] & pf_df$mt.seconds <= end_df[i,n], lower_intensity, pf_df$baseline)
          }else{
            next
          }
        }
        
        # Only retain filling data required for plotting
        
        pf_df <- subset(pf_df, pf_df$intensity != 0)
        
        # Save variables to a list
        
        plot_list[[name_vec[n]]] <- list("eie_data" = eie_df[,c(1,(n + 1))], 
                                         "annotation_data" = ann_df, 
                                         "integration_data" = pf_df,
                                         "label_data" = c(name_vec[n],
                                                          mz_vec[n]),
                                         "x_axis_data" = c(start_df[1,n],
                                                           end_df[num_of_injections,n]),
                                         "y_axis_data" = c(max(ann_df$peak.height.counts), 
                                                           max(eie_df[,(n + 1)])))
        
      }
      ######3.11.2 Create plot function for ggplots######
      plot_function <- function(eie_data, annotation_data, integration_data, label_data, x_axis_data, y_axis_data){
        
        extra_space <- ifelse(x_axis_data[1] > 70, 1, 0)
        
        ggplot(data = eie_data) +
          geom_line(aes(x = mt.seconds / 60, y = eie_data[,2]), colour = "grey50") +
          theme_classic() +
          coord_cartesian(xlim = c(x_axis_data[1] / 60 - extra_space, x_axis_data[2] / 60 + extra_space),
                          ylim = c(0, 1.5 * y_axis_data[1])) +
          scale_y_continuous(name = "Ion Counts",
                             labels = function(x) format(x, scientific = TRUE),
                             expand = c(0,0),
                             breaks = scales::pretty_breaks(n = 10)) +
          scale_x_continuous(name = "Migration Time (Minutes)",
                             breaks = scales::pretty_breaks(n = 10))+
          ggtitle(paste(label_data[1], " EIE", " (m/z = ", label_data[2],")",sep = ""),
                  subtitle = paste("Data File: ", data_files[d])) +
          geom_ribbon(data = integration_data,
                      aes(x = mt.seconds/60, ymax = intensity, ymin = baseline, fill = peak.number),
                      alpha =0.4) +
          geom_text(data = annotation_data,
                    label = annotation_data$peak.number,
                    size  = font_size_1,
                    family = "sans",
                    aes(x = peak.apex.seconds/60,
                        y = peak.height.counts + 0.1 * y_axis_data[1])) +
          geom_text(data = annotation_data,
                    label = annotation_data$comment,
                    size  = font_size_1,
                    family = "sans",
                    aes(x = peak.apex.seconds/60,
                        y = peak.height.counts + 0.2 * y_axis_data[1])) +
          theme(legend.position = "none",
                text = element_text(size = font_size_2, family = "sans"))
        
      }
      
      ######3.11.3 Create Plot Function for making plotly plots######
      #Note: this may be redundant and may get removed later.
      plot_function2 <- function(eie_data, annotation_data, integration_data, label_data, x_axis_data, y_axis_data, font_size_1, font_size_2) {
        extra_space <- ifelse(x_axis_data[1] > 70, 1, 0)
        
        ggplot(data = eie_data) +
          geom_line(aes(x = mt.seconds / 60, y = eie_data[, 2]), colour = "grey50") +
          theme_classic() +
          coord_cartesian(xlim = c(x_axis_data[1] / 60 - extra_space, x_axis_data[2] / 60 + extra_space),
                          ylim = c(0, 1.5 * y_axis_data[1])) +
          scale_y_continuous(name = "Ion Counts",
                             labels = function(x) format(x, scientific = TRUE),
                             expand = c(0, 0),
                             breaks = scales::pretty_breaks(n = 10)) +
          scale_x_continuous(name = "Migration Time (Minutes)",
                             breaks = scales::pretty_breaks(n = 10)) +
          ggtitle(paste(label_data[1], " EIE", " (m/z = ", label_data[2], ")", sep = ""),
                  subtitle = paste("Data File: ", data_files[d])) +
          geom_ribbon(data = integration_data,
                      aes(x = mt.seconds / 60, ymax = intensity, ymin = baseline, fill = peak.number),
                      alpha = 0.4) +
          geom_text(data = annotation_data,
                    label = annotation_data$peak.number,
                    size = font_size_1,
                    family = "sans",
                    aes(x = peak.apex.seconds / 60,
                        y = peak.height.counts + 0.1 * y_axis_data[1])) +
          geom_text(data = annotation_data,
                    label = annotation_data$comment,
                    size = font_size_1,
                    family = "sans",
                    aes(x = peak.apex.seconds / 60,
                        y = peak.height.counts + 0.2 * y_axis_data[1])) +
          theme(legend.position = "none",
                text = element_text(size = font_size_2, family = "sans"))
      }
      
      ######3.11.4 Save plots######
      plotly_objects <- list()
      
      if (parameters_df$plot.format == "Sample"){
        
        data_files_name <- list.files(path = "mzML Files")
        data_files_name <- gsub(".mzML", "", data_file_names, fixed = TRUE)
        
        # Create sub-folders
        
        if (d == 1){
          
          dir.create(path = paste(file_name, "/Plots/", "Analytes", sep = ""),
                     showWarnings = TRUE)
          
          dir.create(path = paste(file_name, "/Plots/", "Internal Standards", sep = ""),
                     showWarnings = TRUE)
          
        }
        
        dir.create(path = paste(file_name, "/Plots/", "Internal Standards", "/", data_files_name[d], sep = ""),
                   showWarnings = TRUE)
        
        dir.create(path = paste(file_name, "/Plots/", "Analytes", "/", data_files_name[d], sep = ""),
                   showWarnings = TRUE)
        
        # Save Internal Standard Plots
        
        for (n in 1:num_of_is){
          
          folder <- "Internal Standards"
          name <- name_vec[n]
          
          font_size_1 <- 7
          font_size_2 <- 25
          
          ggsave(filename = paste(n, "_", name,".png",sep=""),
                 width = 16,
                 height = 9,
                 plot = plot_function(eie_data = plot_list[[n]][[1]], 
                                      annotation_data = plot_list[[n]][[2]], 
                                      integration_data = plot_list[[n]][[3]],
                                      label_data = plot_list[[n]][[4]],
                                      x_axis_data = plot_list[[n]][[5]],
                                      y_axis_data = plot_list[[n]][[6]]),
                 path = paste(file_name, "/Plots/", folder, "/", data_files_name[d], sep = ""))
          
        }
        
        # Save Internal Standard Plots as plotly plots
        for (n in 1:num_of_is) {
          folder <- "Internal Standards"
          name <- name_vec[n]
          
          font_size_1 <- 7
          font_size_2 <- 25
          
          plot_obj <- plot_function2(
            eie_data = plot_list[[n]][[1]], 
            annotation_data = plot_list[[n]][[2]], 
            integration_data = plot_list[[n]][[3]],
            label_data = plot_list[[n]][[4]],
            x_axis_data = plot_list[[n]][[5]],
            y_axis_data = plot_list[[n]][[6]],
            font_size_1 = font_size_1,
            font_size_2 = font_size_2
          )
          
          #Name plots based on the file being processed and the metabolite
          plot_name <- paste0(data_files_name[d], "_", name_vec[n])
          #Convert generated ggplots into plotly functions with ggplotly
          plotly_objects[[plot_name]] <- ggplotly(plot_obj)
          #Shadow code: plotly_objects[[n]] <- ggplotly(plot_obj)
        }
        
        # Save Analyte Plots
        
        for (n in (num_of_is + 1):length(name_vec)){
          
          folder <- "Analytes"
          name <- name_vec[n]
          
          # Analytes using RMT
          
          if (mi_df$description[n - num_of_is] != "mi"){
            
            font_size_1 <- 4
            font_size_2 <- 12
            
            is_index <- which(name_vec == mi_df$description[(n - num_of_is)])
            
            figure <- ggarrange(plot_function(eie_data = plot_list[[n]][[1]], 
                                              annotation_data = plot_list[[n]][[2]], 
                                              integration_data = plot_list[[n]][[3]],
                                              label_data = plot_list[[n]][[4]],
                                              x_axis_data = c(min(eie_df$mt.seconds), max(eie_df$mt.seconds)),
                                              y_axis_data = plot_list[[n]][[6]][2]), 
                                plot_function(eie_data = plot_list[[n]][[1]], 
                                              annotation_data = plot_list[[n]][[2]], 
                                              integration_data = plot_list[[n]][[3]],
                                              label_data = plot_list[[n]][[4]],
                                              x_axis_data = plot_list[[n]][[5]],
                                              y_axis_data = plot_list[[n]][[6]]), 
                                plot_function(eie_data = plot_list[[is_index]][[1]], 
                                              annotation_data = plot_list[[is_index]][[2]], 
                                              integration_data = plot_list[[is_index]][[3]],
                                              label_data = plot_list[[is_index]][[4]],
                                              x_axis_data = plot_list[[n]][[5]],
                                              y_axis_data = plot_list[[is_index]][[6]]),
                                ncol = 1, nrow = 3)
            
            ggsave(filename = paste(n, "_", name,".png",sep=""),
                   width = 16,
                   height = 9,
                   plot = figure,
                   path = paste(file_name, "/Plots/", folder, "/", data_files_name[d], sep = ""))
          }
          
          # Analytes using MI
          
          if (mi_df$description[n - num_of_is] == "mi"){
            
            font_size_1 <- 4
            font_size_2 <- 12
            
            is_index <- which(name_vec == mi_df$description[(n - num_of_is)])
            
            figure <- ggarrange(plot_function(eie_data = plot_list[[n]][[1]], 
                                              annotation_data = plot_list[[n]][[2]], 
                                              integration_data = plot_list[[n]][[3]],
                                              label_data = plot_list[[n]][[4]],
                                              x_axis_data = c(min(eie_df$mt.seconds), max(eie_df$mt.seconds)),
                                              y_axis_data = plot_list[[n]][[6]][2]), 
                                plot_function(eie_data = plot_list[[n]][[1]], 
                                              annotation_data = plot_list[[n]][[2]], 
                                              integration_data = plot_list[[n]][[3]],
                                              label_data = plot_list[[n]][[4]],
                                              x_axis_data = plot_list[[n]][[5]],
                                              y_axis_data = plot_list[[n]][[6]]),
                                ncol = 1, nrow = 2)
            
            ggsave(filename = paste(n, "_", name,".png",sep=""),
                   width = 16,
                   height = 9,
                   plot = figure,
                   path = paste(file_name, "/Plots/", folder, "/", data_files_name[d], sep = ""))
          }
        }
      
        # Save Analyte Plots as Plotly Objects 
        for (n in (num_of_is + 1):length(name_vec)) {

          folder <- "Analytes"
          name <- name_vec[n]

          # Analytes using RMT
          if (mi_df$description[n - num_of_is] != "mi") {

            font_size_1 <- 4
            font_size_2 <- 12

            is_index <- which(name_vec == mi_df$description[(n - num_of_is)])

            plotly_figure <- subplot(
              plot_function2(
                eie_data = plot_list[[n]][[1]],
                annotation_data = plot_list[[n]][[2]],
                integration_data = plot_list[[n]][[3]],
                label_data = plot_list[[n]][[4]],
                x_axis_data = c(min(eie_df$mt.seconds), max(eie_df$mt.seconds)),
                y_axis_data = plot_list[[n]][[6]][2],
                font_size_1 = font_size_1,
                font_size_2 = font_size_2
              ),
              plot_function2(
                eie_data = plot_list[[n]][[1]],
                annotation_data = plot_list[[n]][[2]],
                integration_data = plot_list[[n]][[3]],
                label_data = plot_list[[n]][[4]],
                x_axis_data = plot_list[[n]][[5]],
                y_axis_data = plot_list[[n]][[6]],
                font_size_1 = font_size_1,
                font_size_2 = font_size_2
              ),
              plot_function2(
                eie_data = plot_list[[is_index]][[1]],
                annotation_data = plot_list[[is_index]][[2]],
                integration_data = plot_list[[is_index]][[3]],
                label_data = plot_list[[is_index]][[4]],
                x_axis_data = plot_list[[n]][[5]],
                y_axis_data = plot_list[[is_index]][[6]],
                font_size_1 = font_size_1,
                font_size_2 = font_size_2
              ),
              nrows = 3
            )
      
            #Name plots based on the file being processed and the metabolite
            plot_name <- paste0(data_files_name[d], "_", name_vec[n])
            #Convert generated ggplots into plotly functions with ggplotly
            plotly_objects[[plot_name]] <- ggplotly(plotly_figure)
          }

          #Saving Analytes using MI as plotly objects
          if (mi_df$description[n - num_of_is] == "mi") {

            font_size_1 <- 4
            font_size_2 <- 12

            figure_plotly <- subplot(
              plot_function2(
                eie_data = plot_list[[n]][[1]],
                annotation_data = plot_list[[n]][[2]],
                integration_data = plot_list[[n]][[3]],
                label_data = plot_list[[n]][[4]],
                x_axis_data = c(min(eie_df$mt.seconds), max(eie_df$mt.seconds)),
                y_axis_data = plot_list[[n]][[6]][2],
                font_size_1 = font_size_1,
                font_size_2 = font_size_2
              ),
              plot_function2(
                eie_data = plot_list[[n]][[1]],
                annotation_data = plot_list[[n]][[2]],
                integration_data = plot_list[[n]][[3]],
                label_data = plot_list[[n]][[4]],
                x_axis_data = plot_list[[n]][[5]],
                y_axis_data = plot_list[[n]][[6]],
                font_size_1 = font_size_1,
                font_size_2 = font_size_2
              ),
              nrows = 2
            )

            #Name plots based on the file being processed and the metabolite
            plot_name <- paste0(data_files_name[d], "_", name_vec[n])
            #Convert generated ggplots into plotly functions with ggplotly
            plotly_objects[[plot_name]] <- ggplotly(figure_plotly)
          }
        }
      }
      
      #Save plotly_objects to reactive varibale plotly_data
      plotly_data(plotly_objects)
      
      if (parameters_df$plot.format == "Metabolite"){
        
        # Save plots to their respective folders within the "Plots" folder
        data_files_name <- list.files(path = "mzML Files")
        data_files_name <- gsub(".mz5", "", data_file_names, fixed = TRUE)
        
        # Save Internal Standard Plots
        for (n in 1:length(name_vec)){
          
          folder <- name_vec[n]
          
          font_size_1 <- 7
          font_size_2 <- 25
          
          ggsave(filename = paste(data_files_name[d],".png",sep=""),
                 width = 16,
                 height = 9,
                 plot = plot_function(eie_data = plot_list[[n]][[1]], 
                                      annotation_data = plot_list[[n]][[2]], 
                                      integration_data = plot_list[[n]][[3]],
                                      label_data = plot_list[[n]][[4]],
                                      x_axis_data = plot_list[[n]][[5]],
                                      y_axis_data = plot_list[[n]][[6]]),
                 path = paste(file_name, "/Plots/", folder, "/", sep = ""))
        }
        
        #Save plots as editable plotly plots
        for (n in 1:length(name_vec)) {

          folder <- name_vec[n]

          font_size_1 <- 7
          font_size_2 <- 25

          plot_plot <- plot_function2(
            eie_data = plot_list[[n]][[1]],
            annotation_data = plot_list[[n]][[2]],
            integration_data = plot_list[[n]][[3]],
            label_data = plot_list[[n]][[4]],
            x_axis_data = plot_list[[n]][[5]],
            y_axis_data = plot_list[[n]][[6]],
            font_size_1 = font_size_1,
            font_size_2 = font_size_2
          )
          
          #Name plots based on the file being processed and the metabolite
          plot_name <- paste0(data_files_name[d], "_", name_vec[n])
          #Convert generated ggplots into plotly functions with ggplotly
          plotly_objects[[plot_name]] <- ggplotly(plot_plot)
        }
      }
      
      #Save plotly_objects to plotly_data
      plotly_data(plotly_objects)
      
      print("Plotting Complete")
      
      #####3.12 Export Data#####
      
      ######3.12.1 Generate peak area data frame######
      
      peak_area_df <- cbind("file.name" = c(data_files_name[d], rep("", num_of_injections - 1)),
                            "peak.number" = c(1:num_of_injections),
                            peaks_df[,seq(from = 7, to = ncol(peaks_df), by = 7)])
      colnames(peak_area_df)[3:(length(name_vec) + 2)] <- name_vec
      
      # Update values to include <LOD and Interfered
      
      for (i in 1:num_of_injections){
        peak_area_df[i,3:ncol(peak_area_df)] <- ifelse(comment_df[i,] == "", peak_area_df[i, 3:ncol(peak_area_df)], comment_df[i,])
      }
      
      if(d == 1){
        peak_area_report = peak_area_df
      }else{
        peak_area_report = rbind(peak_area_report, peak_area_df)
      }
      
      ######3.12.2 Generate peak migration time data frame######
      
      peak_mt_df <- cbind("file.name" = c(data_files_name[d], rep("", num_of_injections - 1)),
                          "peak.number" = c(1:num_of_injections),
                          peaks_df[,seq(from = 2, to = ncol(peaks_df), by = 7)] / 60)
      colnames(peak_mt_df)[3:(length(name_vec) + 2)] <- name_vec
      
      if(d == 1){
        peak_mt_report = peak_mt_df
      }else{
        peak_mt_report = rbind(peak_mt_report, peak_mt_df)
      }
      
      # Delete temporary mz5 file
    
      file.remove(paste(file, "temp.mz5", sep = "_"))
  
      print(paste("Completed Analysis of Data File: ", data_file_names[d], sep = ""))
      print("")
      
      ######3.12.3 Export data######
      
      write.csv(peak_area_report,
                file = paste(file_name, "/", "Metabolite Peak Areas.csv", sep = ""),
                row.names = FALSE)
      
      write.csv(peak_mt_report,
                file = paste(file_name, "/", "Metabolite Migration Times.csv", sep = ""),
                row.names = FALSE)
      })
    }#End of loop
  })#End of main button
  
  #####4. Visualization tab####
  # Populate the reactive variable with the Plotly objects
  observe({
    plot_names <- names(plotly_data())
    updateSelectInput(session, "plot_selector", choices = plot_names)
  })
  
  # Render the selected Plotly plot
  output$selected_plot <- renderPlotly({
    req(input$plot_selector)  # Ensure a plot is selected
    plot <- plotly_data()[[input$plot_selector]]
    if (is.null(plot)) {
      plotly_empty()  # Return an empty plot if the plot is NULL
    } else {
      plot
    }
  })
  
 
 
    
}#Closing bracket
  



