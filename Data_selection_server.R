###########################  FUNCTIONS #################################


# helper function for making channel panel layout
shinyInput <- function(FUN, len, id, Val,...) {
  if (is.numeric(Val) ) 
    inputs <- numeric(len)
  else
    inputs <- character(len)
  
  for (i in seq_len(len)) {
    if (length  (Val) > 1)
      v = Val[i]
    else
      v = Val
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, value = v, ...)) 
  } 
  inputs 
} 

# obtain the values of inputs of the checkboxes
shinyValue <- function(id, len) {
  unlist(lapply(seq_len(len), function(i) {
    value = input[[paste0(id, i)]]
    if (is.null(value)) TRUE else value
  }))
}

# Get order number of selected file
getFileNr <- function(){

  n <- match(input$selectedFile,fileChoice())

  return(as.numeric(n))
}

#Update channels pannel
updateChannels <- function(){
  
  n <- getFileNr()
  
  for (j in 1:4){
    for (i in 1:9){
      col <- paste0("cbox",i)
      shinyjs::enable(paste0(col,j))
      updatePrettyCheckbox(session, paste0(col,j), value = FALSE)
    }
  }
  
  req(Conditions$df)
  
  if(nrow(Conditions$df)>0){
    for (k in 1:nrow(Conditions$df)){
      if (Conditions$df[k,1] == ImportedFiles()[n]){
        
        ch <- Conditions$df[k,4]
        row <- ceiling(ch/8)
        col <- ch-8*(row-1)
        shinyjs::disable(paste0(paste0("cbox",col),row))
        shinyjs::disable(paste0(paste0("cbox",9),row))}}
  }
}

#Verify dates
verifyDates <- function(){
  
  Files <- unique(Conditions$df[,1])
  for (i in 1:length(Files)){
    indexes <- which(Conditions$df[,1]==unique(Conditions$df[,1])[1])
    start_date <- unique(Conditions$df[indexes,2])
    stop_date <- unique(Conditions$df[indexes,3])
    if (stop_date <= start_date){
      showNotification(paste("Start date is higher than stop date in file",Files[i]),type = "error",duration = 5)
      return(FALSE)
    }
    }
  return(TRUE)
}

#Update dates and zeitgeber time
updateDates <- function(){
  
  #Update data fields to allow only data present in the Monitor files
  n <- getFileNr()
  Date1 <- as.POSIXct(dates$startDateTime[n])
  Date2 <- as.POSIXct(dates$stopDateTime[n])
  Time1 <- as.POSIXct(dates$startDateTime[n], format = "%H:%M:%S")
  Time2 <- as.POSIXct(dates$stopDateTime[n], format = "%H:%M:%S")

  
  updateDateInput (session,"Start_date", value=Date1)
  updateDateInput (session,"Finish_date", value=Date2)
  updateTimeInput (session,"start_time", value = Time1)
  updateTimeInput (session,"finish_time", value = Time2)
  
  
  #Update shown zeitgeber time
  # Time = as.POSIXct(ZT()[n], format = "%H:%M:%S")
  ZTime = strptime(ZT()[n],'%T')
  updateTimeInput (session,"zeitgeberTime", value = ZTime)
}

########################## VARIABLES #################################

#Volumes for save buttons
Volumes <- reactiveVal()

#Folder directory
Directory <- reactiveVal()

#Files imported by the folder selection
ImportedFiles <- reactiveVal()

#Choices for Data selection
fileChoice <- reactiveVal()

#Channels variables
Channels <- reactiveValues(df = NULL)

#Conditions variable
Conditions <- reactiveValues(df = NULL)

#Thresholds of data
thresholds <- reactiveValues(l = c())

#Data extracted from monitor files
myData<- reactiveVal()

#Date and Time from monitor files
dateTime <- reactive ({
  date <- paste(myData()[,2],match(myData()[,3],month.abb),myData()[,4], sep = "-")
  time <- myData()[,5]
  dateTime <- as.POSIXct(paste(date,time,sep=" "), format = "%d-%m-%y %H:%M:%S")
})

#Data presented in datatable
tableData <- reactiveValues(df = NULL)

#Selected dates
dates <- reactiveValues (startDateTime = NULL, stopDateTime = NULL)

#Light onset time time
ZT <- reactiveVal()

#Files added and zeitgeber table
zt_table <- reactiveVal()

#LD cycle hours
l_hours <- reactiveVal()

#Light period per LD cycle
l_period <- reactiveVal()

#Data imported as behavr table
damData <- reactiveValues(dt = NULL)

#Remove dead animals
channelsToRemove <- reactiveVal()

#Change minimum time to zeitgeber time
MinTime <- reactiveVal(FALSE)

### Dead animal variable
Dead_animal <- reactiveVal()
Live_animal <- reactiveVal()

#Dead animal table
deadTable <- reactiveVal()

#Data without last inactivity period
cleanData <- reactiveVal()
Dead_noInactivity <- reactiveVal()
Live_noInactivity <- reactiveVal()

#############################################################################
############################## SESSION ######################################

#Disable buttons previously to have directory and files
shinyjs::disable("importConditions")
shinyjs::disable("saveMetadata")


###################### GET DATA AND UPDATE FIELDS ###########################

### Get directory and data
observeEvent(input$files,{
  
  #Get directory
  validate(
    need(nrow(input$files)>0,""))
  
  base_path <- tools::file_path_as_absolute(input$files$datapath[1])
  
  if (nrow(input$files)>0){
    split_path <- str_split(base_path, '/')[[1]]
    path <- ""
    for (i in 1:(length(split_path)-1)){
      path <- paste0(path,split_path[i],sep="/")
    }
  }

  validate(
    need(length(path)>0,"")
  )
  
  #Rename the temporary files
  for(i in 1:nrow(input$files)){
    file.rename(input$files$datapath[i], paste(path,input$files$name[i],sep="/"))
  }
  
  ##### RESET  variables #####
  
  #Data
  Conditions$df <- NULL
  damData$dt <- NULL
  tableData$df <- NULL
  myData(NULL)
  ZT(NULL)
  zt_table(NULL)
  deadTable(NULL)
  
  Directory(path)
    
  withProgress(message = 'Importing files', value = 0, {
    
    files <- list.files(path = Directory(), pattern = '.txt') #Get only text files
    
    # Check for found files.
    # If no text files were found enable directory button for new selection
    
    if (length(files)<=0){
      showNotification("Select a folder with DAM text files", type = "error", duration = 5)
    }
    validate(
      need(length(files)>0, message = "Select a folder with DAM text files")
    )
    
    #Verify files found. Only text files without header and with 44 columns are counted as valid
    import <- c()
    thresholds$l <- c()
    table <- data.frame()
    for (i in 1:length(files)){
      data <- data.frame()
      datapath <- paste(Directory(),files[i],sep = "/" )
      
      tryCatch({data <- read.table(datapath)},
               warning = function(w) {
                 showNotification(paste(files[i], "is not a monitor file"), type = "error", duration = 5)
                 data <- data.frame()
               }, error = function(e) {
                 showNotification(paste(files[i], "is not a monitor file"), type = "error", duration = 5)
                 data <- data.frame()
               }, finally = {
               }
      )
      if (ncol(data)==44){
        if (is.null(import)){
          thresholds$l <- ({c(thresholds$l,length(data[,1]))})
        }
        else{
          thresholds$l <- ({c(thresholds$l,thresholds$l[i-1] + length(data[,1]))})
        }
        table <- rbind(table,data)
        import <- c(import,files[i])
      }
    }
  
    
    validate(
      need(!is.null(import), message = "Select a folder with DAM text files")
    )
    
    ImportedFiles(import) #Set imported files to ImportedFiles() variable
    myData(table)
    
    
    # Update files that can be selected
    fileChoice(paste(match(ImportedFiles(),ImportedFiles()),"-", tools::file_path_sans_ext(ImportedFiles())))
    
    updateSelectInput(session, "selectedFile", label = "File selection", choices = fileChoice())
    
    
    ##### Light onset time
    zeitgeberTime <- c()
    for (i in 1:length(ImportedFiles())){
      zeitgeberTime <- c(zeitgeberTime,paste(strftime(input$zeitgeberTime, format="%H:%M:%S")))
    }
    
    ZT(zeitgeberTime)
    
    
    ##### First date and times
    dates$startDateTime <- as.POSIXct(rep(NA,length(ImportedFiles())))
    dates$stopDateTime <- as.POSIXct(rep(NA,length(ImportedFiles())))
    time <- myData()[,5]
    
    Date1 <- dateTime()[1]
    Date2 <- dateTime()[thresholds$l[1]]
    Time1 <- strptime(time[1],"%T")
    Time2 <- strptime(time[thresholds$l[1]],"%T")
    
    
    #Update first date and time
    updateDateInput(session,"Start_date", value=Date1)
    updateDateInput(session,"Finish_date", value=Date2)
    updateTimeInput (session,"start_time", value = Time1)
    updateTimeInput (session,"finish_time", value = Time2)
    
    
    start <- as.POSIXct(paste(Date1," ",strftime(Time1, format="%H:%M:%S")),format="%Y-%m-%d %H:%M:%S")
    finish <- as.POSIXct(paste(Date2," ",strftime(Time2, format="%H:%M:%S")),format="%Y-%m-%d %H:%M:%S")
    dates$startDateTime[1] <- start
    dates$stopDateTime[1] <- finish
    
    for (i in 1:length(ImportedFiles())){
      if (i != 1){
        incProgress(1/length(ImportedFiles())) #Increment of progress bar
        Date1 <- dateTime()[thresholds$l[i-1]+1]
        Date2 <- dateTime()[thresholds$l[i]]
        Time1 <- strptime(time[thresholds$l[i-1]+1],"%T")
        Time2 <- strptime(time[thresholds$l[i]],"%T")
        start <- as.POSIXct(paste(Date1," ",strftime(Time1, format="%H:%M:%S")),format="%Y-%m-%d %H:%M:%S")
        finish <- as.POSIXct(paste(Date2," ",strftime(Time2, format="%H:%M:%S")),format="%Y-%m-%d %H:%M:%S")
        dates$startDateTime[i] <- start
        dates$stopDateTime[i] <- finish
      }
    }
    
    updateSelectInput(session,"linecolor", "Line color", choices = colorPallete, selected = "black")
    updateSelectInput(session,"linecolor2", "Line color", choices = colorPallete, selected = "black")
    updateSelectInput(session,"linecolor3", "Line color", choices = colorPallete, selected = "black")
  })

  enable("importConditions")
  
})

### Update data when selected file changes
observeEvent(input$selectedFile,{

  req(!is.null(Directory()))
  
  #Update dates
  updateDates()

  # Update Channels panel
  updateChannels()
})

### Light hours and period variables update
observeEvent(input$l_period,{
  
  updateSliderInput(session,'l_hours',max=input$l_period)
  l_period(input$l_period)
  
  req(damData$dt)
  
  #Activity and bouts variables for statistics plots
  damData$dt[, experimentDay := floor(periodT/(l_period()*3600))] #Experiment day
  damData$dt[, day_night := ((periodT-experimentDay*l_period()*3600)/3600)<l_hours()] #Day or night time
  damData$dt[, 'activityBoxPlot_time' := floor(damData$dt[,'t']/(input$activityGroupBoxTime * l_period()*3600))]
  damData$dt[, 'boutBoxPlot_time' := floor(damData$dt[,'t']/(input$boutGroupBoxTime * l_period()*3600))]
  
})
observeEvent(input$l_hours,{
  
  l_hours(input$l_hours)
  
  req(damData$dt)
  
  #Activity and bouts variables for statistics plots
  damData$dt[, experimentDay := floor(periodT/(l_period()*3600))] #Experiment day
  damData$dt[, day_night := ((periodT-experimentDay*l_period()*3600)/3600)<l_hours()] #Day or night time
  damData$dt[, 'activityBoxPlot_time' := floor(damData$dt[,'t']/(input$activityGroupBoxTime * l_period()*3600))]
  damData$dt[, 'boutBoxPlot_time' := floor(damData$dt[,'t']/(input$boutGroupBoxTime * l_period()*3600))]
})

### Zeitgeber time data
observe({
  
  req(Conditions$df)
  
  if(nrow(Conditions$df)>0){
  
    Files <- unique(Conditions$df[,1])
    Start_Dates <- as.character(unique(Conditions$df[,2]))
    Finish_Dates <- as.character(unique(Conditions$df[,3]))
    
    zts <- c() #Zeitgebers numbers
    
    for (i in 1:length(Files)){
      n <- match(Files[i],ImportedFiles())
      zts <- c(zts,n)
    }
    
    Light_onset <- ZT()[zts]
    
    df <- data.frame(Files, Light_onset,Start_Dates, Finish_Dates)
    
    zt_table(df)
    
    output$FilesAdded <- renderTable(zt_table())
  }
  else{
    Files <- NA
    Light_onset <- NA
    Start_Dates <- NA
    Finish_Dates <- NA
    
    output$FilesAdded <- renderTable(data.frame(Files, Light_onset,Start_Dates, Finish_Dates))
  }
})

### Update zeitgeber if field changes
observeEvent(input$zeitgeberTime,{
  
  req(length(ImportedFiles())>0)
  n <- getFileNr()

  zts <- ZT()
  zts[n] <- paste(strftime(input$zeitgeberTime, format="%H:%M:%S"))
  ZT(zts)
})

### Import metadata
observeEvent(input$importConditions,{
  
  #Get metadata file
  ext <- tools::file_ext(input$importConditions$name)
  
  if (ext != "xlsx"){      
    showNotification(paste("Files must be .xlsx"), type = "error", duration = 10)
  }
  validate(need(ext == "xlsx", ""))
  
  #Verify if the .xlsx have 2 pages
  wb <- loadWorkbook(input$importConditions$datapath)
  sheet <- getSheets(wb)
  if (length(sheet)!=2){      
    showNotification("Metadata files should have 2 sheets", type = "error", duration = 10)
  }
  validate(need(length(sheet)==2, ""))
  
  table <- read.xlsx(input$importConditions$datapath,1)

  error = FALSE
  
  ### Verify if all headers are according to the saved metadata files
  if (names(table)[1] != "file" || names(table)[2] != "start_datetime" || names(table)[3] != "stop_datetime" || names(table)[4] != "region_id" || names(table)[5] != "labels" || names(table)[6] != "order"){
    showNotification(paste("The headers of the metadata files are not valid"), type = "error", duration = 5)
    error = TRUE
  }
  
  validate(
    need(error == FALSE,""))
  
  File <- table[,1]
  table <- cbind(table,File)
  
  ### Turn dates into date and time format
  table[,2] <- as.POSIXct(table[,2])
  table[,3] <- as.POSIXct(table[,3])
  
  uniqueNames <- unique(table[,'file'])
  
  ### Verify if all files match with the ones present in the selected folder and import data
  import <- TRUE
  for(i in 1:length(uniqueNames)){
    if (is.na(match(uniqueNames[i],ImportedFiles()))){
      import <-  FALSE
      break
    }
    else{
      n <- match(uniqueNames[i],ImportedFiles())
      dateN <- match(uniqueNames[i],File)
      dates$startDateTime[n] <- table[dateN,'start_datetime']
      dates$stopDateTime[n] <- table[dateN,'stop_datetime']
      
    }
  }
  if (import == TRUE){
    Conditions$df <- table
  }
  else{
    showNotification(paste("Selected metadata file is not from files from selected folder"), type = "error", duration = 5)
  }
  
  validate(
    need(import == TRUE,"")
  )
  
  #Update zeitgeber times
  ztSheet <- read.xlsx(input$importConditions$datapath,2)
  
  if(nrow(ztSheet)<length(uniqueNames)){
    showNotification("Corrupted ZT sheet",type = "warning", duration = 5)
  }
  validate(
    need(nrow(ztSheet)>=length(uniqueNames),"")
  )
  
  zt <- c()
  
  for (i in 1:length(ImportedFiles())){
    zt <- c(zt,"08:00:00")
  }
  
  
  for (i in 1:nrow(ztSheet)){
    n <- match(ztSheet[i,1],ImportedFiles())
    if (!is.na(ztSheet[i,2])){
      zt[n] <- ztSheet[i,2]
    }
  }
  
  ZT(zt)
  
  
  #Update Dates
  updateDates()
  
  #Update Channels
  updateChannels()
  
})

### Save metadata
observeEvent(input$saveMetadata,{
  
  req(nrow(Conditions$df))
  
  shinySaveButton("save", "Save file", "Save file as ...", filetype=list(xlsx="xlsx"))
})


########### DATE AND TIME FIELDS AND VARIABLES ############

### Check for changes in date and time
changeDate <-reactive({
  list(input$Start_date,input$start_time,input$Finish_date,input$finish_time)
})

### Update date and time variable "dates"
observeEvent(changeDate(),{
  
  req(!is.null(ImportedFiles()))
  req(input$selectedFile)
  
  n <- getFileNr()
  
  start <- as.POSIXct(paste(input$Start_date," ",strftime(input$start_time, format="%H:%M:%S")),format="%Y-%m-%d %H:%M:%S")
  finish <- as.POSIXct(paste(input$Finish_date," ", strftime(input$finish_time, format="%H:%M:%S")),format="%Y-%m-%d %H:%M:%S")
  
  time <- myData()[,5]
  
  if (n == 1){
    minDate <- dateTime()[1]
    minTime <- strptime(time[1],"%T")
  }
  else{
    minDate <- dateTime()[thresholds$l[n-1]+1]
    minTime <- strptime(time[thresholds$l[n-1]+1],"%T")
  }
  
  maxDate <- dateTime()[thresholds$l[n]]
  maxTime <- strptime(time[thresholds$l[n]],"%T")
  
  # Prevent user to select dates outside of the range of the dates of the file
  if (start < minDate){
    updateDateInput (session,"Start_date", value = minDate)
    updateTimeInput (session,"start_time", value = minTime)
    showNotification (paste("Start date is too low"), type = "error", duration = 5)
  }
  if (finish < minDate){
    updateDateInput (session,"Finish_date", value = minDate)
    updateTimeInput (session,"finish_time", value = minTime)
    showNotification (paste("Start date is too low"), type = "error", duration = 5)
  }
  if (start > maxDate){
    updateDateInput (session,"Start_date", value = maxDate)
    updateTimeInput (session,"start_time", value = maxTime)
    showNotification (paste("Stop date is too high"), type = "error", duration = 5)
  }
  if (finish > maxDate){
    updateDateInput (session,"Finish_date", value = maxDate)
    updateTimeInput (session,"finish_time", value = maxTime)
    showNotification (paste("Stop date is too high"), type = "error", duration = 5)
  }
  
  # Prevent the start date from being higher than finish date 
  if (start>=finish){
    
    showNotification("Start date has to be lower than end date", type = "warning", duration = 10)
  }
  
  #Check if dates are correct
  validate(
    # need(start<finish, message = "Start date has to be lower than end date"),
    need(start>=minDate, message = "Start date is too low"),
    need(finish<=maxDate, message = "Stop date his too high")
  )
  
  dates$startDateTime[n] <- start
  dates$stopDateTime[n] <- finish
  
  n <- getFileNr()
  
  timeDiff <- difftime(finish,start,units = "days")
  
  if (timeDiff <1){
    updateRadioButtons(session,'activityBoxTime','Box time', c("Minutes" = "Min", "Hours" = "Hour"), selected = "Hour", inline = TRUE)
    updateRadioButtons(session,'boutBoxTime','Box time', c("Minutes" = "Min", "Hours" = "Hour"), selected = "Hour", inline = TRUE)
  }
  else{
    updateRadioButtons(session,'activityBoxTime','Box time', c("Hours" = "Hour", "Days" = "Day"), selected = "Day", inline = TRUE)
    updateRadioButtons(session,'boutBoxTime','Box time', c("Hours" = "Hour", "Days" = "Day"), selected = "Day", inline = TRUE)
  }
  
  req(Conditions$df)

  if(nrow(Conditions$df)>0){
    for (i in 1:nrow(Conditions$df)){
      if (Conditions$df[i,1] == ImportedFiles()[n]){

        Conditions$df[i,'start_datetime'] <- start
        Conditions$df[i,'stop_datetime'] <- finish
      }}
  }
})


################## CHANNELS PANEL ##################

#Create channels datatable
observe({

  req(is.null(Channels$df))

  #Create dataframe
  df <- data.frame(
    #Creation of checkbox datatable with lower width possible
    Col1 = shinyInput(prettyCheckbox, 4, paste0("cbox1"), FALSE, width = "5%"),
    Col2 = shinyInput(prettyCheckbox, 4, paste0("cbox2"), FALSE, width = "5%"),
    Col3 = shinyInput(prettyCheckbox, 4, paste0("cbox3"), FALSE, width = "5%"),
    Col4 = shinyInput(prettyCheckbox, 4, paste0("cbox4"), FALSE, width = "5%"),
    Col5 = shinyInput(prettyCheckbox, 4, paste0("cbox5"), FALSE, width = "5%"),
    Col6 = shinyInput(prettyCheckbox, 4, paste0("cbox6"), FALSE, width = "5%"),
    Col7 = shinyInput(prettyCheckbox, 4, paste0("cbox7"), FALSE, width = "5%"),
    Col8 = shinyInput(prettyCheckbox, 4, paste0("cbox8"), FALSE, width = "5%"),
    Col9 = shinyInput(prettyCheckbox, 4, paste0("cbox9"), FALSE, width = "5%")
  )
  
  #Column names
  names(df)[1] <- " "
  names(df)[2] <- " "
  names(df)[3] <- " "
  names(df)[4] <- " "
  names(df)[5] <- " "
  names(df)[6] <- " "
  names(df)[7] <- " "
  names(df)[8] <- " "
  names(df)[9] <- "Select All"
  
  Channels$df <- df
  
  #Set channels datatable to channels panel
  output$Channels <- DT::renderDataTable(
    expr = {Channels$df},
    server = FALSE,
    escape = FALSE,
    selection = "none",
    rownames= FALSE,
    options = list(
      stateSave = TRUE,
      ordering = FALSE,
      searching = FALSE,
      paging = TRUE,
      pagingType = "numbers",
      pageLength = 5,
      lengthChange = FALSE,
      info = FALSE,
      preDrawCallback = JS("function() {
          Shiny.unbindAll(this.api().table().node()); }"
      ),
      drawCallback = JS("function() {
          Shiny.bindAll(this.api().table().node());} "))
  )
  
  #Name each checkbox
  for (i in 1:4){
    for (j in seq_len(8)){
      col <- paste0("cbox",j)
      updatePrettyCheckbox(session,paste0(col,i),paste(j+8*(i-1)),FALSE)
    }
  }
})

#Select All buttons (if press one select All button, select or unselect all)
observeEvent(input[['cbox91']],{
  i=1
  value <- shinyValue(paste0("cbox",9),4)[i]==TRUE
    for (j in seq_len(8)){
      col = paste0("cbox",j)
      updatePrettyCheckbox(session,paste0(col,i),paste(j+8*(i-1)),value)}
})
observeEvent(input[['cbox92']],{

  i=2
  value <- shinyValue(paste0("cbox",9),4)[i]==TRUE
  for (j in seq_len(8)){
    col = paste0("cbox",j)
    updatePrettyCheckbox(session,paste0(col,i),paste(j+8*(i-1)),value)}
})
observeEvent(input[['cbox93']],{

  i=3
  value <- shinyValue(paste0("cbox",9),4)[i]==TRUE
  for (j in seq_len(8)){
    col = paste0("cbox",j)
    updatePrettyCheckbox(session,paste0(col,i),paste(j+8*(i-1)),value)}
})
observeEvent(input[['cbox94']],{

  i=4
  value <- shinyValue(paste0("cbox",9),4)[i]==TRUE
  for (j in seq_len(8)){
    col = paste0("cbox",j)
    updatePrettyCheckbox(session,paste0(col,i),paste(j+8*(i-1)),value)}
})


################################# METADATA ##################################

#Create metadata from conditions added and update conditions addition UI
observeEvent(input$addcondition,{
  
  if (is.null(Directory())){
    showNotification("No folder selected",type  ="error", duration = 5)}
  
  validate (
    need(!is.null(Directory()),"")
  )
  
  n<-getFileNr()
  
  #Check if any name was inputed for the condition
  if (input$condition==""){
    showNotification("No name selected for the condition chosen", type = "error", duration = 5)
  }
  validate(
    need(input$condition!="","No name selected for the condition chosen")
  )
  
  #Check if any channel was selected
  region_id <- vector()
  for (j in 1:4){
    for (i in 1:8){
      if(shinyValue(paste0("cbox",i),4)[j])
      { 
        col = paste0("cbox",i)
        shinyjs::disable(paste0(col,j))
        shinyjs::disable(paste0(paste0("cbox",9),j))
        updatePrettyCheckbox(session,paste0(col,j),paste(i+8*(j-1)),FALSE)
        region_id <- c(region_id,i+8*(j-1))}
    }
    updatePrettyCheckbox(session,paste0(paste0("cbox",9),j)," ",FALSE)}
  
  
  if (length(region_id)<1){
    showNotification("No channel selected", type = "error", duration = 5)
  }
  validate(
    need(length(region_id)>0,"No channel selected")
  )
  
  #Reset conditions text field
  updateTextInput(session,"condition",value = "")
  
  ### Order variable
  value <- 1
  if (!is.null(Conditions$df)){
    if (nrow(Conditions$df)>0){
      for (i in 1:length(Conditions$df[,6])){
        if (input$condition == Conditions$df[i,5]){
          value <- Conditions$df[i,6]
          break
        }
        if (i == length(Conditions$df[,6]) && length(Conditions$df[,6]) > 0){
          value <- max(Conditions$df[,6])+1
        }
      }
    }
  }
  
  #Metadata variables
  file <- ImportedFiles()[n]
  start_datetime <- dates$startDateTime[n]
  stop_datetime <- dates$stopDateTime[n]
  
  labels <- input$condition
  order <- value
  
  File <- file
  result <- data.frame(file, start_datetime, stop_datetime,region_id, labels, order, File)
  
  Conditions$df <- rbind(Conditions$df,result)
  
})

################################ DATATABLE ##################################
#Create data table to present to the user
observeEvent(Conditions$df,{

  req(nrow(Conditions$df)>0)

  #Reorder labels and order number
  if (nrow(Conditions$df)>1){
    Conditions$df <- Conditions$df[order(Conditions$df[,'order']),]
  }


  # Separate data according to labels and files
  # Add other variables
  Channels <- vector()
  Labels <- vector()
  File <- vector()
  First_date <- vector()
  End_date <- vector()
  Order <- vector()

  for (i in 1:length(Conditions$df[,5])){
    if (i == 1){
      channelsPerLabel <- as.character(Conditions$df[i,4])
      Labels <- Conditions$df[i,'labels']
      File <- tools::file_path_sans_ext(Conditions$df[i,1])
      First_date <- as.character(Conditions$df[i,2])
      End_date <- as.character(Conditions$df[i,3])
      Order <- as.numeric(Conditions$df[i,6])
    }
    else{
      if (Conditions$df[i,'labels'] == Labels[length(Labels)] & tools::file_path_sans_ext(Conditions$df[i,1]) == File[length(File)]){
        channelsPerLabel <- paste(channelsPerLabel,as.character(Conditions$df[i,4]))
      }
      else{
        Channels <- c(Channels,channelsPerLabel)
        channelsPerLabel <- as.character(Conditions$df[i,4])
        Labels <- c(Labels,Conditions$df[i,5])
        File <- c(File,tools::file_path_sans_ext(Conditions$df[i,1]))
        First_date <- c(First_date,as.character(Conditions$df[i,2]))
        End_date <- c(End_date,as.character(Conditions$df[i,3]))
        Order <- c(Order,Conditions$df[i,6])
      }
    }
  }

  Channels <- c(Channels,channelsPerLabel)


  tableData$df <- data.frame(File,First_date,End_date,Channels,Labels, Order)

})

#Data panel
output$Data <- DT::renderDataTable(
  expr= {deleteButtonColumn(tableData$df, 'delete_button')}, escape = FALSE, selection = 'none', 
  editable  = list(target = 'cell',disable = list(columns = c(1,2,3,4,7))),
  option = list(ordering = TRUE, order = list(list(6,'asc'))))




############################# Start analysis ###################################

#Import and analyse data
observeEvent(input$startanalysis,{
  
  req (nrow(Conditions$df)>0)
  
  #Verify that dates are correctly inputted
  validate(
    need(verifyDates(),""))
  
  
  ##### Import data and create behavr tables
  
  withProgress(message = 'Importing data (1/3)', value = 0, {
    
    #get DAM data
    data <- link_dam_metadata(Conditions$df,Directory()) #linking
    damData$dt <- load_dam(data, FUN = sleep_dam_annotation, min_time_immobile = 60*input$sleepTime) #load dam data
    
    incProgress(0.5) #Increment of progress bar
    MinTime(TRUE)
    
    disable("Death_graphs")
    
    #Only analyse bouts if more the data has more than 60 measurements
    if (nrow(damData$dt)>60){
      enable("boutAnalysis")
    }
    
    #Update sliders accoding to imported data
    if ((max(damData$dt[,'t'])-min(damData$dt[,'t']))/60 < 180){
      updateSliderInput(session,"movingAverage",max = (max(damData$dt[,'t'])-min(damData$dt[,'t']))/60)
      updateSliderInput(session,"sleepTime",max = (max(damData$dt[,'t'])-min(damData$dt[,'t']))/60)
    }
    else{
      updateSliderInput(session,"movingAverage",value = 60, max = 180)
      updateSliderInput(session,"sleepTime",value = 5, max = 60)
    }
    
  })
  
  ##### Calculate parameters 
  
  withProgress(message = 'Computing analysis parameters (2/3)', value = 0, {
    
    #Minimum time counted in the Sensor and update sliders according
    damData$dt[,timeDiff := c(NaN,damData$dt[2:nrow(damData$dt),t]- damData$dt[1:(nrow(damData$dt)-1),t])]
    updateSliderInput(session, "activityValue",min = round(mean(damData$dt[,timeDiff],na.rm=TRUE)/60))
    updateSliderInput(session, "boutValue",min = round(mean(damData$dt[,timeDiff],na.rm=TRUE)/60))
    updateSliderInput(session,"movingAverage", min = ceiling(mean(damData$dt[,timeDiff],na.rm=TRUE)/60), step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)
    updateSliderInput(session,"boutWindow", min = ceiling(mean(damData$dt[,timeDiff],na.rm=TRUE)/60), step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)
    updateRadioButtons(session, "activityBoxTime", selected = "Day")
    updateRadioButtons(session, "boutBoxPlot_time", selected = "Day")
    
    #Periodic time adjustable by the user
    damData$dt[,'periodT' := (damData$dt[,'t'])]
    
    #Files in the metadata
    Files<- damData$dt[,file_info,meta=T]
    
    #Difference between start time and zeitgeber time
    minDiff <- as.numeric(difftime(as.POSIXct(strftime(dates$startDateTime,format="%H:%M:%S"), format="%H:%M:%S"),as.POSIXct(ZT(), format="%H:%M:%S"), units = "mins"))
    
    #Start index
    startIndex <-1
    
    #Create moving and AUC variables
    damData$dt[, moving := activity>0]
    damData$dt[, auc := cumsum(activity)]
    
    #Get final indexes for each animal
    finalIndexes <- c()
    if (sum(damData$dt[,timeDiff]<0,na.rm=TRUE)>0){
      finalIndexes <- which(damData$dt[,timeDiff] %in% damData$dt[,timeDiff][damData$dt[,timeDiff]<0])-1}
    
    finalIndexes <- c(finalIndexes,nrow(damData$dt))
    startIndexes <- c(1,finalIndexes[1:length(finalIndexes)-1]+1)
    
    #Update time according to the zeitgeber time
    startIndex <- 1
    if (MinTime()==TRUE){
      for (i in 1:length(damData$dt[,id,meta=T])){
        n <- match(Files[[i]]$file,ImportedFiles())
        damData$dt[startIndex:finalIndexes[i], t := t+(minDiff[n]*60)]
        startIndex <- finalIndexes[i]+1
      }
    }
    MinTime(FALSE)
    
    #Create labels, file, channels and order 
    damData$dt[,labels := damData$dt[1,labels,meta=T]]
    damData$dt[,file := damData$dt[1,File,meta=T]]
    damData$dt[,channels := damData$dt[1,region_id,meta=T]]
    damData$dt[,order := damData$dt[1,order,meta=T]]
    
    damData$dt[startIndexes,timeDiff := NaN]
    if (length(startIndexes)>1){
      for (i in 1:length(startIndexes)){
        damData$dt[(startIndexes[i]:finalIndexes[i]),auc := cumsum(damData$dt[startIndexes[i]:finalIndexes[i],activity])]
        
        animal <- damData$dt[,,meta=T][i]
        damData$dt[startIndexes[i]:finalIndexes[i],'labels' := rep(animal$labels,(finalIndexes[i]-startIndexes[i])+1)]
        damData$dt[startIndexes[i]:finalIndexes[i],'file' := rep(animal$File,(finalIndexes[i]-startIndexes[i])+1)]
        damData$dt[startIndexes[i]:finalIndexes[i],'channels' := rep(animal$region_id,(finalIndexes[i]-startIndexes[i])+1)]
        damData$dt[startIndexes[i]:finalIndexes[i],'order' := rep(animal$order,(finalIndexes[i]-startIndexes[i])+1)]
      }
    }
  })
  
  #Data labels for graphs
  damData$dt[,Data_labels := interaction(File, labels,region_id, sep=" - "), meta =T]
  damData$dt[,Data := interaction(labels,order,sep=" - "), meta = T]
  
  #Activity and bouts variables for statistics plots
  damData$dt[, experimentDay := floor(periodT/(l_period()*3600))] #Experiment day
  damData$dt[, day_night := ((periodT-experimentDay*l_period()*3600)/3600)<l_hours()] #Day or night time
  damData$dt[, 'activityBoxPlot_time' := floor(damData$dt[,'t']/(input$activityGroupBoxTime * l_period()*3600))]
  damData$dt[, 'boutBoxPlot_time' := floor(damData$dt[,'t']/(input$boutGroupBoxTime * l_period()*3600))]
  
  #Remove last inactivity period from animals
  withProgress(message = 'Analyzing dead animals (3/3)', value = 0, {
    cleanData(curate_dead_animals(damData$dt,prop_immobile = 0))
    # cleanData(curate_dead_animals(damData$dt[t %between% c(finalTimes[i]-hours(input$deadTime), finalTimes[i])], prop_immobile = 0, time_window= hours(input$deadTime))
    
    deadTable(NULL)
    
    enable("deleteInactivity")
  })
})

###################### UPDATE DATA BY CHANGES IN TABLE #######################

#Update metadata and labels from cell edit
observeEvent(input$Data_cell_edit, {
  info = input$Data_cell_edit
  str(info)
  i = info$row
  j = info$col
  v = info$value
  
  #Change metadata value
  if (j==5){
    for (k in 1:length(Conditions$df[,5])){
      if (Conditions$df[k,5] == tableData$df[i,j]){
        Conditions$df[k,5] <- v
      }
    }
  }
  if (j==6){
    for (k in 1:length(Conditions$df[,6])){
      if (Conditions$df[k,6] == tableData$df[i,j]){
        Conditions$df[k,6] <- as.numeric(v)
      }
      else{
        if (Conditions$df[k,6] == v){
          Conditions$df[k,6] <- as.numeric(tableData$df[i,j])
        }
      }
    }
  }
  
})

#Delete row and from metadata or from dead larvae selection
observeEvent(input$deletePressed, {
  rowNum <- parseDeleteEvent(input$deletePressed)
  
  if(input$animals == "Add Conditions"){
    
    req(nrow(Conditions$df)>0)
    
    withProgress(message = 'Update data', value = 0, {
      for (k in seq(length(Conditions$df[,5]),1,-1)){
        if (Conditions$df[k,5] == tableData$df[rowNum,5] & tools::file_path_sans_ext(Conditions$df[k,1]) == tableData$df[rowNum,1]){
          
          incProgress(1/nrow(Conditions$df)) #Increment of progress bar
          ch <- Conditions$df[k,4]
          row <- ceiling(ch/8)
          col <- ch-8*(row-1)
          shinyjs::enable(paste0(paste0("cbox",col),row))
          
          #Delete value from metadata
          Conditions$df <- Conditions$df[-k,]
          
          if (ch<10)
            paste_channels <- paste0('0',ch)
          else
            paste_channels <- ch
          
          ids <- paste(Conditions$df[row,'start_datetime'],paste0(Conditions$df[row,'file']),paste_channels,sep="|")
          
        }
      }
    })
  }
  
  else{
    req(nrow(Conditions$df)>0)
    req(length(channelsToRemove())>0)
    
    for (k in seq(nrow(Conditions$df),1,-1)){
      if (Conditions$df[k,4] == deadTable()[rowNum,2] & tools::file_path_sans_ext(Conditions$df[k,1]) == deadTable()[rowNum,1]){
        
        ch <- Conditions$df[k,4]
        row <- ceiling(ch/8)
        col <- ch-8*(row-1)
        shinyjs::enable(paste0(paste0("cbox",col),row))
        

        if (ch<10)
          paste_channels <- paste0('0',ch)
        else
          paste_channels <- ch

        ids <- paste(Conditions$df[row,'start_datetime'],paste0(Conditions$df[row,'file']),paste_channels,sep="|")
        
        indexesToDelete <- channelsToRemove()
        subtract1 <- FALSE
        for (i in seq(from = length(indexesToDelete),to = 1,by = -1)){
          if (subtract1 == TRUE){
            indexesToDelete[i] <- indexesToDelete[i]-1
          }
          if (indexesToDelete[i]==k){
            indexesToDelete <- indexesToDelete[-i]
            subtract1 <- TRUE
          }
        }
        channelsToRemove(indexesToDelete)

        #Delete value from metadata
        Conditions$df <- Conditions$df[-k,]
        
        deadTable(deadTable()[-rowNum,])
        break
      }
    }
  }
  
  MinTime(TRUE)
  
  if (nrow(Conditions$df)==0){
    damData$dt <- NULL
    cleanData(NULL)
  }
  else{
    #Dead animals
    damData$dt<-damData$dt[which(damData$dt[,'id']!=ids),]
    cleanData(cleanData()[which(cleanData()[,'id']!=ids),])
  }
  
  
  #Select all ticks
  shinyjs::enable(paste0(paste0("cbox",9),1))
  shinyjs::enable(paste0(paste0("cbox",9),2))
  shinyjs::enable(paste0(paste0("cbox",9),3))
  shinyjs::enable(paste0(paste0("cbox",9),4))
  
  n<- getFileNr()
  
  if(length(Conditions$df[,1]>0)){
    for (k in seq(length(Conditions$df[,1]))){
      if (Conditions$df[k,1] == ImportedFiles()[n]){
        
        ch <- Conditions$df[k,4]
        if (ch>=1 & ch<=8){
          shinyjs::disable(paste0(paste0("cbox",9),1))}
        if (ch>=9 & ch<=16){
          shinyjs::disable(paste0(paste0("cbox",9),2))}
        if (ch>=17 & ch<=24){
          shinyjs::disable(paste0(paste0("cbox",9),3))}
        if (ch>=25 & ch<=32){
          shinyjs::disable(paste0(paste0("cbox",9),4))}
      }}
  }
  
  # Delete the row from the data frame
  tableData$df <- tableData$df[-rowNum,]
})

######################### DOWNLOAD AND IMPORT METADATA #########################

#Download metadata

observeEvent(Conditions$df,{
  
  if (nrow(Conditions$df)>0){
    shinyjs::enable("saveMetadata")
  }
  else{
    shinyjs::disable("saveMetadata")
  }
  output$saveMetadata <- downloadHandler(
    
    filename = function(){
      paste0('metadata','.xlsx')
    },
    content = function(file){
      
      req(nrow(Conditions$df)>0)
    
      data <- Conditions$df[,1:6]
      
      data[,2] <- as.character(data[,2])
      data[,3] <- as.character(data[,3])
    
    #Create xlsx workbook of conditions and zeitgeber table
    wb<-createWorkbook(type="xlsx")
    sheet <- createSheet(wb,"Conditions")
    addDataFrame(data, sheet=sheet, startColumn=1, row.names=FALSE)
    sheet <- createSheet(wb, "ZT")
    addDataFrame(zt_table(), sheet=sheet, startColumn=1, row.names=FALSE)
  
    saveWorkbook(wb, file=file)
  })
})

########################## REMOVE DEAD ANIMALS #################################

#Find dead larvae
observeEvent(input$remove,{
  
  #Disable delete larvae button while the program finds dead larvae
  shinyjs::disable('deleteAnimals')
  shinyjs::disable('deleteInactivity')
  
  validate(
    need(nrow(damData$dt)>0,"")
  )
  
  # Get the final indexes of each replica
  if (nrow(tableData$df)>1){
    finalIndexes <- which(is.nan(damData$dt[,timeDiff]))-1
    finalIndexes <- c(finalIndexes[2:length(finalIndexes)],nrow(damData$dt))
  }
  else{
    finalIndexes <- nrow(damData$dt)}
  
  #Final times from the indexes
  finalTimes <- unique(damData$dt[finalIndexes,t])
  
  #Detect dead larvae according to the user selected criterion
  differences <-c()
  for (i in 1:length(finalTimes)){
    
    dt_curated <- curate_dead_animals(damData$dt[t %between% c(finalTimes[i]-hours(input$deadTime), finalTimes[i])], prop_immobile = 0)
    segmentedData <- damData$dt[t %between% c(finalTimes[i]-hours(input$deadTime), finalTimes[i])]
    differences <- c(differences,setdiff(segmentedData[, id, meta=T], dt_curated[, id, meta=T]))
  }
  
  #Get indexes of animals to remove
  differences <- unique(differences)
  
  Files <- damData$dt[,file_info, meta=T]
  Channels <- damData$dt[,region_id, meta=T]
  
  indexesToDelete <- c()
  for (i in seq_len(length(differences))){
    index <- match(differences[i],damData$dt[,id, meta=T])
    fileName <- Files[index][[1]]$file
    channel <- Channels[index]
    
    conditionsIndex <- which(Conditions$df[,4]==channel)
    for (j in seq_len(length(conditionsIndex))){
      if (Conditions$df[conditionsIndex[j],1]==fileName){
        indexesToDelete <- c(indexesToDelete,conditionsIndex[j])
        break
      }
    }
  }
  
  
  #Present animals to remove to the user
  indexesToDelete <- sort(indexesToDelete, decreasing = TRUE)
  channelsToRemove(indexesToDelete)
  
  File <- tools::file_path_sans_ext(Conditions$df[indexesToDelete,1])
  Channels <- Conditions$df[indexesToDelete,4]
  Labels <- Conditions$df[indexesToDelete,5]
  table <- data.frame(File,Channels,Labels)
  
  #Table of dead larvae
  deadTable(table)

  #Dead and live animal variables
  if (length(indexesToDelete>0)){
    
    channels <- Conditions$df[indexesToDelete,'region_id']
    paste_channels <- c()
    for (i in 1:length(channels)){
      if (channels[i]<10)
        paste_channels <- c(paste_channels,paste0('0',channels[i]))
      else
        paste_channels <- c(paste_channels,channels[i])
    }
    ids <- paste(Conditions$df[indexesToDelete,'start_datetime'],paste0(Conditions$df[indexesToDelete,'file']),paste_channels,sep="|")
    
    #Dead animals
    Dead_animal(damData$dt[which(damData$dt[,'id']==ids),])
    Dead_noInactivity(cleanData()[which(cleanData()[,'id']==ids),])
    
    #Live animals
    Live_animal(damData$dt[which(damData$dt[,'id']!=ids),])
    Live_noInactivity(cleanData()[which(cleanData()[,'id']!=ids),])
    
    shinyjs::enable("Death_graphs")
    shinyjs::enable('deleteInactivity')
  }
  
  shinyjs::enable('deleteAnimals')
  shinyjs::enable('deleteInactivity')
  
})

### Table of dead animals
observeEvent(deadTable(),{
  output$DeadTubes <- DT::renderDataTable(
    data.frame(deleteButtonColumn(deadTable(), 'delete_button')), escape = FALSE, selection = 'none',editable  = FALSE)
})

##### Remove all dead animals #####
observeEvent(input$deleteAnimals,{
  
  validate(
    need(length(channelsToRemove())>0,"")
  )
  
  withProgress(message = 'Removing dead animals', value = 0, {
    indexesToDelete <- channelsToRemove()
    
    #Delete dead larvae
    paste_channels <- c()
    for (i in 1:length(indexesToDelete)){
      if (as.numeric(Conditions$df[indexesToDelete[i],'region_id'])<10)
        paste_channels <- c(paste_channels,paste0('0',Conditions$df[indexesToDelete[i],'region_id']))
      else
        paste_channels <- c(paste_channels,Conditions$df[indexesToDelete[i],'region_id'])
    }
  
    ids <- paste(Conditions$df[indexesToDelete,'start_datetime'],paste0(Conditions$df[indexesToDelete,'file']),paste_channels,sep="|")
    # print(ids)
    
    #Dead animals
    for (i in 1:length(ids)){
      damData$dt<-damData$dt[which(damData$dt[,'id']!=ids[i]),]
      cleanData(cleanData()[which(cleanData()[,'id']!=ids[i]),])
    }
    
    
    updateChannels()
    
    output$DeadTubes <- DT::renderDataTable(
      NULL, escape = FALSE, selection = 'none',editable  = FALSE)
    
    
    for (k in 1:length(indexesToDelete)){
      ch <- Conditions$df[indexesToDelete[k],4]
      row <- ceiling(ch/8)
      col <- ch-8*(row-1)
      shinyjs::enable(paste0(paste0("cbox",col),row))
      
      Conditions$df <- Conditions$df[-indexesToDelete[k],]
    }
    
    channelsToRemove(c())
    
  })
  graphsAestethics$df <- graphsAestethics$df[-nrow(graphsAestethics$df),]
  MinTime(TRUE)
  updateSliderInput(session,"movingAverage",value = 60)
  
  shinyjs::disable('deleteInactivity')
  disable("deleteAnimals")
  
})

##### REMOVE INACTIVITY OF ANIMALS #####
observeEvent(input$deleteInactivity,{
  req(damData$dt)

  damData$dt <- cleanData()
  
  disable("clean_data")
  disable("deleteInactivity")
  shinyjs::disable('deleteAnimals')
})

output$checkChannels <- renderPlot({
  
  validate(
    need (nrow(Conditions$df)>0,"No data selected")
  )
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  if (input$Death_graphs == "all"){
    if(input$clean_data==FALSE){
      data <- damData$dt
    }
    else{
      data <- cleanData()
    }
  }
  else{
    if (input$Death_graphs == "dead"){
      if(input$clean_data==FALSE){
        data <- Dead_animal()}
      else{
        data <- Dead_noInactivity()
      }
    }
    else{
      if(input$clean_data==FALSE){
        data <- Live_animal()}
      else{
        data <- Live_noInactivity()
      }
    }
  }
    
  fig <- ggetho(data,aes(x = t, y = interaction(File,labels,region_id,order,sep=" - "),
                         z = activity),summary_FUN = sum,summary_time_window = mins(30)) +
    stat_tile_etho() + 
    labs(title=(paste("Mean activity in 30 minutes")),y = "Channels")
    
  fig <- whiteBackground(fig)
    
  return(fig)
    
}, height = function() {req(damData$dt)
    
    # Height of the graph
  if (input$Death_graphs == "all"){
    nrow(damData$dt[,,meta=T])*25+200}
  else{
    if (input$Death_graphs == "dead"){
      nrow(Dead_animal()[,,meta=T])*25+200
    }
    else{
      nrow(Live_animal()[,,meta=T])*25+200
    }
  }
},
res = 96)

