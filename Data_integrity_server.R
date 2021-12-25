
########################## VARIABLES #################################
DataFile <- reactiveVal() #Files of data
DataDir <- reactiveVal() #Directory of data
inputtedData <- reactiveVal() 
importedData <- reactiveVal()

######################## EVALUATE DATA ###############################
disable("evaluateData")
disable("saveInputtedData")
### Input missing data
observeEvent(input$FileToEvaluate,{
  
  #Get file
  validate(
    need(nrow(input$FileToEvaluate)>0,""))
  
  #File path
  base_path <- tools::file_path_as_absolute(input$FileToEvaluate$datapath[1])
  
  
  #If get path worked rename the temporary file
  validate(
    need(length(path)>0,"")
  )
  
  if (nrow(input$FileToEvaluate)>0){
    split_path <- str_split(base_path, '/')[[1]]
    path <- ""
    for (i in 1:(length(split_path)-1)){
      path <- paste0(path,split_path[i],sep="/")
    }
  }
  
  #File rename
  file.rename(input$FileToEvaluate$datapath, paste(path,input$FileToEvaluate$name,sep="/"))
  
  #Check if file is text file
  if (file_ext(input$FileToEvaluate$name)!='txt'){
    showNotification(paste(missingDataFile(), "is not a text file"), type = "error", duration = 5)
  }
  validate(
    need(tools::file_ext(input$FileToEvaluate$name)=='txt',"")
  )
  
  DataFile(input$FileToEvaluate$name) #Filename variable
  DataDir(path) #Path variable
  
  
  #Verify files found. Only text files without header and with 44 columns are counted as valid
  
  data <- data.frame()
  datapath <- paste(DataDir(),DataFile(),sep = "/" )
  tryCatch({data <- read.table(datapath, sep = "\t")},
           warning = function(w) {
             showNotification(paste(missingDataFile(), "is not in a table format"), type = "error", duration = 5)
             data <- data.frame()
           }, error = function(e) {
             showNotification(paste(missingDataFile(), "is not in a table format"), type = "error", duration = 5)
             data <- data.frame()
           }, finally = {
           }
  )
  validate(
    need(!is.null(data),"")
  )
  
  #Verify data is in the correct format
  #Verify if data is in 42 column format
  if (ncol(data)!= 42){
    showNotification(paste(DataFile(), "is not in 42 column format"), type = "error", duration = 5)
  }
  validate(
    need(ncol(data)==42,"")
  )
  
  #Verify if first 10 columns are in he correct format
  colTypes <- sapply(data,class)
  checkColType <- rep("integer",42)
  checkColType[2] <- "character"
  checkColType[3] <- "character"
  checkColType[8] <- "character"
  
  for (i in 1:10){
    if (colTypes[i] != checkColType[i]){
      showNotification(paste("Column", as.character(i), "of", DataFile(), "should be",checkColType[i]), type = "error", duration = 5)
      validate(
        need(colTypes[i] == checkColType[i],"")
      )
    }
  }
  
  importedData(data) #Imported file
  
  output$fileCheck <- renderText({
    paste("Sucessfull text file upload.",
          " ",
          "File is in the correct 42 column table format.",
          sep = "\r")
  })
  enable("evaluateData")
})

#Analyze missing data
observeEvent(input$evaluateData,{
  
  #Verify if there is missing data
  
  #### Input missing data
  activityData <- importedData()[,11:42]
  
  for(i in 1:ncol(activityData)){
    activityData[,i] <- as.numeric(activityData[,i])
  }
  
  if (any(is.na(activityData))){
    NAData <- which(is.na(activityData), arr.ind = TRUE)
    NAimputation <- round(imputation(data.matrix(activityData),"copyMean.locf"))
    OutputData <- cbind(importedData()[1:12],NAimputation)
    
    inputtedData(OutputData)
    
    NAchannels <- unique(NAData[,'col'])
    print(NAchannels)
    output$fileCheck <- renderText({
      
      ch <- NAchannels[1]

      for(i in 2:length(NAchannels)){
        ch <- paste(ch, " ,",NAchannels[i], sep = "")
      }
      
      paste("File has missing data in channels", ch )
    })
  }
  else{
    output$fileCheck <- renderText({
      paste("No missing data")
    })
  }
  enable("saveInputtedData")
  
 

})

observeEvent(inputtedData(),{
  
  req(!is.null(inputtedData()))
  shinySaveButton("save", "Save file", "Save file as ...", filetype=list(xlsx="txt"))
  
  output$saveInputtedData <- downloadHandler(
    
    filename = function(){
      paste0(DataFile(),'.txt')
    },
    content = function(file){
      
      write.table(inputtedData(),file, quote = FALSE, sep = "\t",col.names = FALSE, row.names = FALSE)
    })
})
