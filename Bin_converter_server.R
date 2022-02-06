
########################## VARIABLES #################################
BinFile <- reactiveVal() #Files of data
BinDir <- reactiveVal() #Directory of data
BinData <- reactiveVal() #Imported Data

ConvertedData <- reactiveVal()

bins <- reactiveVal(c(1,2,3,4,5,6,10,12,15,20,30,60,120,180,240,300,360,600,720,900,1200,1800,3600,7200,10800))

######################## EVALUATE DATA ###############################
disable("evaluateBins")
### Input missing data
observeEvent(input$FileToConvert,{
  
  #Get file
  validate(
    need(nrow(input$FileToConvert)>0,""))
  
  #File path
  base_path <- tools::file_path_as_absolute(input$FileToConvert$datapath[1])
  
  
  #If get path worked rename the temporary file
  validate(
    need(length(path)>0,"")
  )
  
  if (nrow(input$FileToConvert)>0){
    split_path <- str_split(base_path, '/')[[1]]
    path <- ""
    for (i in 1:(length(split_path)-1)){
      path <- paste0(path,split_path[i],sep="/")
    }
  }
  
  #File rename
  file.rename(input$FileToConvert$datapath, paste(path,input$FileToConvert$name,sep="/"))
  
  #Check if file is text file
  if (file_ext(input$FileToConvert$name)!='txt'){
    showNotification(paste(input$FileToConvert$name, "is not a text file"), type = "error", duration = 5)
  }
  validate(
    need(tools::file_ext(input$FileToConvert$name)=='txt',"")
  )
  
  BinFile(input$FileToConvert$name) #Filename variable
  BinDir(path) #Path variable
  
  
  #Verify files found. Only text files without header and with 44 columns are counted as valid
  
  data <- data.frame()
  datapath <- paste(BinDir(),BinFile(),sep = "/" )
  tryCatch({data <- read.table(datapath, sep = "\t")},
           warning = function(w) {
             showNotification(paste(BinFile(), "is not in a table format"), type = "error", duration = 5)
             data <- data.frame()
           }, error = function(e) {
             showNotification(paste(BinFile(), "is not in a table format"), type = "error", duration = 5)
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
    showNotification(paste(BinFile(), "is not in 42 column format"), type = "error", duration = 5)
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
  
  BinData(data) #Imported file
  
  output$fileCheck <- renderText({
    paste("Sucessfull text file upload.",
          " ",
          "File is in the correct 42 column table format.",
          sep = "\r")
  })
  enable("evaluateBins")
})

#Analyze missing data
observeEvent(input$evaluateBins,{
  
  #Verify if there is missing data
  
  #### Input missing data
  FileDates <- paste(str_replace_all(BinData()[,2], " ","-") ,BinData()[,3])
  
  dateTimes <- as.POSIXlt(FileDates, tz = "", format = "%d-%b-%y %H:%M:%OS",
             optional = FALSE)

  Time_differences <- difftime(dateTimes[2:length(dateTimes)],dateTimes[1:(length(dateTimes)-1)],units="secs")
  
  dataBinSec <- unique(Time_differences)
  
  if (length(dataBinSec)>1){
    showNotification(paste(BinFile(), "has more than one bin size"), type = "error", duration = 5)
  }
  
  output$DataInfo <- renderText({
    
    if (length(dataBinSec)==1){
      if(dataBinSec<=60){
        dataMessage <- paste("Data was collected in", dataBinSec,"seconds")}
      else{
        dataMessage <- paste("Data was collected in", round(dataBinSec/60,2),"minutes")
      }
    }
    else{
      dataBinSec <- dataBinSec[order(dataBinSec)]
      dataMessage <- "Data was collected in"
      for (i in 1:(length(dataBinSec)-1)){
        if(dataBinSec[i]<=60){
          dataMessage <- paste(dataMessage, dataBinSec[i],"seconds;")}
        else{
          dataMessage <- paste(dataMessage, round(dataBinSec[i]/60,2),"minutes;")
        }
      }
      if(dataBinSec[length(dataBinSec)]<=60){
        dataMessage <- paste(dataMessage, "and", dataBinSec[length(dataBinSec)],"seconds")}
      else{
        dataMessage <- paste(dataMessage, "and", round(dataBinSec[length(dataBinSec)]/60,2),"minutes")
      }
    }
    
    return(dataMessage)
    
  })
  
  validate(
    need(length(dataBinSec)==1,"")
  )

    
  availableConversions <- which(bins()%%dataBinSec==0 & bins()/dataBinSec >1)
  
  windows <- floor(bins()[availableConversions]/dataBinSec)
  
  conversions <- c()
  names <- c()
  values <- c()
  for(i in 1:length(availableConversions)){
    value <- bins()[availableConversions[i]]/3600
    if (value<1){
      value <- bins()[availableConversions[i]]/60
      names <- c(names,paste(value,"mins"))
      values <- c(values,windows[i])
    }
    else{
      names <- c(names,paste(value,"hours"))
      values <- c(values,windows[i])
    }
  }
  
  output$binsButtons <- renderUI({
    radioButtons("selectedBins", "Choose bin to convert", choiceNames = names, choiceValues = values,inline=TRUE)
  })
  output$ConvertButton <- renderUI({
    downloadButton("Convert", "Convert data")
  })
  
})

output$Convert <- downloadHandler(
  
  filename = function(){
    paste0(BinFile(),'.txt')
  },
  content = function(file){
  
    print(BinFile())
    activity <- BinData()[,11:42]
    window <- as.numeric(input$selectedBins)
    
    ConvertedActivity <- rollapply(activity[1], width = window, by = window, FUN = sum)
    
    for (i in 2:ncol(activity)){
      cAct = rollapply(activity[i], width = window, by = window, FUN = sum)
      ConvertedActivity <- cbind(ConvertedActivity,cAct)
    }
    
    indexes <- seq(window, nrow(activity), by = window)
    
    result <- cbind(BinData()[indexes,1:10],ConvertedActivity)
    
    write.table(result, file, quote = FALSE, sep = "\t",col.names = FALSE, row.names = FALSE)
  }
  
)
