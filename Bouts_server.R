############################# VARIABLES ##################################

### Activity data and figure variables
BoutActivityData <- reactiveVal()
BoutActivityFig <- reactiveVal()

### Activity time and figure variables
BoutTimeData <- reactiveVal()
BoutTimeFig <- reactiveVal()

### Sleep latency variables
BoutSleepLatencyData <- reactiveVal()
BoutSleepLatencyFig <- reactiveVal()

### Sleep bout duration variables
BoutSleepTimeData <- reactiveVal()
BoutSleepTimeFig <- reactiveVal()

############################ Data to plot ################################

#Activity bouts to plot
boutActivitySummary <- function(){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ##### Create data for statistic plots #####
  
  if(input$boutActivityPlotsTabs == "Mean activity bouts"){
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(Activity_Light = mean(boutActivity[day_night == "TRUE" & activityBout == TRUE & boutActivity > 0],na.rm=TRUE)), by = id])
    summaryNight <- rejoin(damData$dt[, .(Activity_Dark = mean(boutActivity[day_night == "FALSE" & activityBout == TRUE & boutActivity > 0],na.rm = TRUE)), by = id])
    
    summaryDT <- cbind(summaryLight, summaryNight[,Activity_Dark])
    names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Dark")
    
    #Get summary by light and dark phase  
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Activity_"),variable.name = "xPlot", value.name = "yPlot")
    
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "Activity_Dark", "Dark")]
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "Activity_Light", "Light")]
  }
  
  if(input$boutActivityPlotsTabs == "Mean activity bouts per day"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(Activity = mean(boutActivity[experimentDay==startDay & boutActivity > 0 & activityBout == TRUE],na.rm=TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryDT)[names(summaryDT) == "Activity"] <- paste0("Activity_Day_",startDay)
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(Activity = mean(boutActivity[experimentDay==n & boutActivity > 0 & activityBout == TRUE], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,Activity])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",n)
      }
    }
    
    #Rename files as Day0, Day1...
    names(summaryDT) <- str_replace_all(names(summaryDT), "Activity_", "")
    
    #Get summary by days
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  if(input$boutActivityPlotsTabs == "Mean activity bouts daytime vs nighttime"){
    
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(activity_Day = mean(boutActivity[experimentDay==startDay & day_night == "TRUE" & boutActivity > 0 & activityBout == TRUE],na.rm=TRUE)), by = id])
    summaryNight <- rejoin(damData$dt[, .(activity_Night = mean(boutActivity[experimentDay==startDay & day_night == "FALSE" & boutActivity > 0 & activityBout == TRUE],na.rm = TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryLight)[names(summaryLight) == "activity_Day"] <- paste0("Activity_Day_",startDay,"_Light")
    
    summaryDT <- cbind(summaryLight, summaryNight[,activity_Night])
    names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",startDay,"_Dark")
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(activity_Day = mean(boutActivity[experimentDay==n & day_night == "TRUE" & boutActivity > 0 & activityBout == TRUE], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,activity_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",n,"_Light")
        
        summary <- rejoin(damData$dt[, .(activity_Night = mean(boutActivity[experimentDay==n & day_night == "FALSE" & boutActivity > 0 & activityBout == TRUE], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,activity_Night])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",n,"_Dark")
      }
    }
    
    #Rename by day and light phase
    names(summaryDT) <- str_replace_all(names(summaryDT), "Activity_", "")
    
    #Get summary by light and dark phase and by day
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  if(input$boutActivityPlotsTabs == "Mean activity bouts custom time"){
    
    #Separate by customized activity
    startTime <- min(damData$dt[,boutBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(activity_Day = mean(boutActivity[boutBoxPlot_time==startTime & boutActivity > 0 & activityBout == TRUE],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "activity_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,boutBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(activity_Day = mean(boutActivity[boutBoxPlot_time==n & boutActivity > 0 & activityBout == TRUE],na.rm=TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,activity_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Group",n)
      }
    }
    
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Group"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  return(summaryDT_melted)
  
}
boutTimeSummary <- function(){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ##### Create data for statistic plots #####
  
  if(input$boutTimePlotsTabs == "Mean bouts time"){
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(boutTime_Light = mean(activityBoutTime[day_night == "TRUE" & activityBoutTime > 0],na.rm=TRUE)), by = id])
    summaryNight <- rejoin(damData$dt[, .(boutTime_Dark = mean(activityBoutTime[day_night == "FALSE" & activityBoutTime > 0],na.rm = TRUE)), by = id])
    
    summaryDT <- cbind(summaryLight, summaryNight[,boutTime_Dark])
    names(summaryDT)[length(names(summaryDT))] <- paste0("boutTime_Dark")
    
    #Get summary by light and dark phase  
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("boutTime_"),variable.name = "xPlot", value.name = "yPlot")
    
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "boutTime_Dark", "Dark")]
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "boutTime_Light", "Light")]
  }
  
  if(input$boutTimePlotsTabs == "Mean bouts time per day"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(boutTime = mean(activityBoutTime[experimentDay==startDay & activityBoutTime > 0],na.rm=TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryDT)[names(summaryDT) == "boutTime"] <- paste0("boutTime_Day_",startDay)
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(boutTime = mean(activityBoutTime[experimentDay==n & activityBoutTime > 0 ], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,boutTime])
        names(summaryDT)[length(names(summaryDT))] <- paste0("boutTime_Day_",n)
      }
    }
    
    #Rename files as Day0, Day1...
    names(summaryDT) <- str_replace_all(names(summaryDT), "boutTime_", "")
    
    #Get summary by days
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  if(input$boutTimePlotsTabs == "Mean bouts time daytime vs nighttime"){
    
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(boutTime_Day = mean(activityBoutTime[experimentDay==startDay & day_night == "TRUE" & activityBoutTime> 0 ],na.rm=TRUE)), by = id])
    summaryNight <- rejoin(damData$dt[, .(boutTime_Night = mean(activityBoutTime[experimentDay==startDay & day_night == "FALSE" & activityBoutTime > 0 ],na.rm = TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryLight)[names(summaryLight) == "boutTime_Day"] <- paste0("boutTime_Day_",startDay,"_Light")
    
    summaryDT <- cbind(summaryLight, summaryNight[,boutTime_Night])
    names(summaryDT)[length(names(summaryDT))] <- paste0("boutTime_Day_",startDay,"_Dark")
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(boutTime_Day = mean(activityBoutTime[experimentDay==n & day_night == "TRUE" & activityBoutTime > 0 ], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,boutTime_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("boutTime_Day_",n,"_Light")
        
        summary <- rejoin(damData$dt[, .(boutTime_Night = mean(activityBoutTime[experimentDay==n & day_night == "FALSE" & activityBoutTime > 0], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,boutTime_Night])
        names(summaryDT)[length(names(summaryDT))] <- paste0("boutTime_Day_",n,"_Dark")
      }
    }
    
    #Rename by day and light phase
    names(summaryDT) <- str_replace_all(names(summaryDT), "boutTime_", "")
    
    #Get summary by light and dark phase and by day
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
    
  }
  
  if(input$boutTimePlotsTabs == "Mean bouts time custom time"){
    
    #Separate by customized activity
    startTime <- min(damData$dt[,boutBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(boutTime_Day = mean(activityBoutTime[boutBoxPlot_time==startTime & activityBoutTime > 0 ],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "boutTime_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,boutBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(boutTime_Day = mean(activityBoutTime[boutBoxPlot_time==n & activityBoutTime > 0 ],na.rm=TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,boutTime_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Group",n)
      }
    }
    
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Group"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")

  }
  
  return(summaryDT_melted)
}

#Sleep bouts to plot
boutSleepLatencySummary <- function(){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ##### Create data for statistic plots #####
  
  if(input$SleepLatencyPlotsTabs == "Sleep latency"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(SleepLatency = mean(sleepLatency[sleepLatency > 0],na.rm=TRUE)), by = id])
    
    #Get summary by light and dark phase  
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("SleepLatency"),variable.name = "xPlot", value.name = "yPlot")
    
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "SleepLatency", "Sleep_latency")]
  }
  
  if(input$SleepLatencyPlotsTabs == "Sleep latency per day"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(SleepLatency = mean(sleepLatency[experimentDay==startDay & sleepLatency > 0 ],na.rm=TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryDT)[names(summaryDT) == "SleepLatency"] <- paste0("SleepLatency_Day_",startDay)
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(SleepLatency = mean(sleepLatency[experimentDay==n & sleepLatency > 0], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,SleepLatency])
        
        names(summaryDT)[length(names(summaryDT))] <- paste0("SleepLatency_Day_",n)
      }
    }
    
    
    #Rename files as Day0, Day1...
    names(summaryDT) <- str_replace_all(names(summaryDT), "SleepLatency_", "")
    
    
    
    #Get summary by days
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  if(input$SleepLatencyPlotsTabs == "Sleep latency custom time"){
    
    #Separate by customized activity
    startTime <- min(damData$dt[,boutBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(SleepLatency_Day = mean(sleepLatency[boutBoxPlot_time==startTime & sleepLatency > 0],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "SleepLatency_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,boutBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(SleepLatency_Day = mean(sleepLatency[boutBoxPlot_time==n & sleepLatency > 0],na.rm=TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,SleepLatency_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Group",n)
      }
    }
    
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Group"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
    
  }
  
  if (any(is.nan(summaryDT_melted[,yPlot]))){
    summaryDT_melted[which(is.nan(summaryDT_melted[,yPlot])),yPlot:=0]
  }
  return(summaryDT_melted)
}
boutSleepTimeSummary <- function(){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ##### Create data for statistic plots #####
  
  if(input$SleepTimePlotsTabs == "Sleep bout time"){
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(Activity_Light = mean(sleepBoutTime[day_night == "TRUE" & sleepBoutTime>0],na.rm=TRUE)), by = id])
    summaryNight <- rejoin(damData$dt[, .(Activity_Dark = mean(sleepBoutTime[day_night == "FALSE" & sleepBoutTime>0],na.rm = TRUE)), by = id])
    
    summaryDT <- cbind(summaryLight, summaryNight[,Activity_Dark])
    names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Dark")
    
    #Get summary by light and dark phase  
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Activity_"),variable.name = "xPlot", value.name = "yPlot")
    
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "Activity_Dark", "Dark")]
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "Activity_Light", "Light")]
  }
  
  if(input$SleepTimePlotsTabs == "Sleep bout time per day"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(Activity = mean(sleepBoutTime[experimentDay==startDay & sleepBoutTime>0],na.rm=TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryDT)[names(summaryDT) == "Activity"] <- paste0("Activity_Day_",startDay)
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(Activity = mean(sleepBoutTime[experimentDay==n & sleepBoutTime>0], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,Activity])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",n)
      }
    }
    
    #Rename files as Day0, Day1...
    names(summaryDT) <- str_replace_all(names(summaryDT), "Activity_", "")
    
    #Get summary by days
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  if(input$SleepTimePlotsTabs == "Sleep bout time daytime vs nighttime"){
    
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(activity_Day = mean(sleepBoutTime[experimentDay==startDay & day_night == "TRUE" & sleepBoutTime>0],na.rm=TRUE)), by = id])
    summaryNight <- rejoin(damData$dt[, .(activity_Night = mean(sleepBoutTime[experimentDay==startDay & day_night == "FALSE" & sleepBoutTime>0],na.rm = TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryLight)[names(summaryLight) == "activity_Day"] <- paste0("Activity_Day_",startDay,"_Light")
    
    summaryDT <- cbind(summaryLight, summaryNight[,activity_Night])
    names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",startDay,"_Dark")
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(activity_Day = mean(sleepBoutTime[experimentDay==n & day_night == "TRUE" & sleepBoutTime>0], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,activity_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",n,"_Light")
        
        summary <- rejoin(damData$dt[, .(activity_Night = mean(sleepBoutTime[experimentDay==n & day_night == "FALSE" & sleepBoutTime>0], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,activity_Night])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",n,"_Dark")
      }
    }
    
    #Rename by day and light phase
    names(summaryDT) <- str_replace_all(names(summaryDT), "Activity_", "")
    
    #Get summary by light and dark phase and by day
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  if(input$SleepTimePlotsTabs == "Sleep bout time custom time"){
    
    #Separate by customized activity
    startTime <- min(damData$dt[,boutBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(activity_Day = mean(sleepBoutTime[boutBoxPlot_time==startTime & sleepBoutTime>0],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "activity_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,boutBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(activity_Day = mean(sleepBoutTime[boutBoxPlot_time==n & sleepBoutTime>0],na.rm=TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,activity_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Group",n)
      }
    }
    
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Group"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  return(summaryDT_melted)
  
}


observe({
  req(damData$dt)
  req(input$boutAnalysis)
  req('boutActivity' %in% colnames(damData$dt))
  
  req(input$boutGroupBoxTime)
  req(input$boutBoxTime)
  
  BoutActivityData(boutActivitySummary())
  BoutTimeData(boutTimeSummary())
})

observe({
  
  req(damData$dt)
  req(input$boutAnalysis)
  req('sleepLatency' %in% colnames(damData$dt))
  
  req(input$boutGroupBoxTime)
  req(input$boutBoxTime)
  
  BoutSleepTimeData(boutSleepTimeSummary())
  BoutSleepLatencyData(boutSleepLatencySummary())
  
})

observeEvent(input$startanalysis,{
  req(damData$dt)
  req("timeDiff" %in% colnames(damData$dt))
  
  #Update bout analysis bin size
  if ((max(damData$dt[,'t'])-min(damData$dt[,'t']))/60 < 180){
    updateSliderInput(session,"boutWindow",max = (max(damData$dt[,'t'])-min(damData$dt[,'t']))/60,
                      min = max(damData$dt[,timeDiff],na.rm=TRUE)/60, step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)
  }
  else{
    updateSliderInput(session,"boutWindow",value = 60, max = 180,
                      min = max(damData$dt[,timeDiff],na.rm=TRUE)/60, step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)}
})

#Bout analysis
observeEvent(input$boutAnalysis,{
  
  req(damData$dt)
  
  # Analyze bouts
  withProgress(message = 'Analyzing bouts', value = 0, {
    
    #Calculus of the time difference between measurements
    damData$dt[,timeDiff := c(NaN,damData$dt[2:nrow(damData$dt),t]- damData$dt[1:(nrow(damData$dt)-1),t])]
    meanTimeDiff <- mean(damData$dt[damData$dt[,timeDiff]>0,timeDiff]) #Minimum bin size
    step <- round(mins(input$boutWindow)/meanTimeDiff) #Step for analysis
    
    #Calculate the start and final indexes for each sensor channel
    finalIndexes <- c()
    startIndexes <- 1
    if (sum(damData$dt[,timeDiff]<0,na.rm=TRUE)>0){
      finalIndexes <- which(damData$dt[,timeDiff] %in% damData$dt[,timeDiff][damData$dt[,timeDiff]<0])-1}
    
    finalIndexes <- c(finalIndexes,nrow(damData$dt))
    if (length(finalIndexes)>1){
      startIndexes <- c(startIndexes,(finalIndexes[1:(length(finalIndexes)-1)]+1))
    }
    
    #Introduce the variables bouts, boutTime and boutActivity
    damData$dt[,sleepBout:= FALSE]
    damData$dt[,activityBout:= FALSE]
    damData$dt[,activityBoutTime := 0]
    damData$dt[,sleepBoutTime := 0]
    damData$dt[,sleepLatency := 0]
    damData$dt[,boutActivity := 0]
    
    validate(
      need(!is.null(startIndexes),"")
    )
    
    start_bouts <- c()
    end_bouts <- c()
    
    activity <- c()
    
    data <- link_dam_metadata(Conditions$df,Directory()) #linking
    bout_data <- load_dam(data, FUN = sleep_dam_annotation, min_time_immobile = 60*step) #load dam data
    
    bout_dt <- bout_analysis(asleep, damData$dt)
    
    #Calculate bouts
    for (j in 1:length(startIndexes)){
      End <- FALSE
      incProgress(1/length(startIndexes))
      
      activityBout_dt <- bout_analysis(asleep, bout_data[startIndexes[j]:finalIndexes[j],])
      sleepBout_dt <- bout_analysis(asleep, damData$dt[startIndexes[j]:finalIndexes[j],])
      t <- damData$dt[startIndexes[j]:finalIndexes[j],t]
      t<- t-min(t)
      t<- t + t[2]
      
      sleepBouts <- sleepBout_dt[asleep == TRUE, -"asleep"]
      activityBouts <- activityBout_dt[asleep == FALSE, -"asleep"]
      
      startRowInactivity <- which(t %in% sleepBouts[,t])
      startRowActivity <- which(t %in% activityBouts[,t])
      finishRowInactivity <- which(t %in% (sleepBouts[,t]+sleepBouts[,duration]))
      finishRowActivity <- which(t %in% (activityBouts[,t]+activityBouts[,duration]))
      
      latency <- TRUE
      for (k in 1:length(startRowInactivity)){
        
        damData$dt[(startIndexes[j]+startRowInactivity[k]-1),sleepBout := TRUE]
        damData$dt[(startIndexes[j]+startRowInactivity[k]-1),sleepBoutTime := sleepBouts[k,duration]]
      }
      
      periodicTime <- damData$dt[startIndexes[j]:finalIndexes[j],periodT]
      replica_data <- damData$dt[startIndexes[j]:finalIndexes[j],]
      threshold_hours <- hours(l_hours())
      
      for (k in min(damData$dt[,experimentDay]):max(damData$dt[,experimentDay])){
        
        Time <- periodicTime - hours(l_period())*k - threshold_hours
        
        if(max(Time)>0){
          n <- which(abs(Time) == min(abs(Time)))
          # if (damData$dt[(n+startIndexes[j]),asleep]==FALSE){
            m <- which(replica_data[Time>0,sleepBout]==TRUE)
            
            if(length(m)>0){
              damData$dt[min(m)+n+startIndexes[j]-1,sleepLatency := (Time[min(m)+n])]}
          # }
        }
      }
      
      for (k in 1:length(startRowActivity)){
        damData$dt[(startIndexes[j]+startRowActivity[k]-1),activityBout := TRUE]
        damData$dt[(startIndexes[j]+startRowActivity[k]-1),activityBoutTime := activityBouts[k,duration]]
        
        bout_activity <- cumsum(damData$dt[(startIndexes[j]+startRowActivity[k]-1):(startIndexes[j]+finishRowActivity[k]-1),activity])
        damData$dt[(startIndexes[j]+startRowActivity[k]-1),boutActivity := bout_activity[length(bout_activity)]]
      }
    }

  })
})

########################### Change labels ###############################

#Bout Activity
observeEvent(input$BoutActivitySummary_cell_edit,{
  
  info <- input$BoutActivitySummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  boutActivity <- BoutActivityData()
  conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=boutActivity, FUN=mean)[,1]
  Label <-  str_split_fixed(conditions, " - ",2)[,2]
  
  for (k in 1:nrow(boutActivity)){
    if (boutActivity[k,'xPlot']==Label[i]){
      boutActivity[k,'xPlot'] <- v
    }
  }
  BoutActivityData(boutActivity)
  
})
observeEvent(input$boutActivityPlotsTabs,{
  
  req(damData$dt)
  req(BoutActivityData())
  summaryDT_melted <- boutActivitySummary()
  
  BoutActivityData(summaryDT_melted)
})

#Bout Time
observeEvent(input$BoutTimeSummary_cell_edit,{
  
  info <- input$BoutTimeSummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  boutTime <- BoutTimeData()
  conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=boutTime, FUN=mean)[,1]
  Label <-  str_split_fixed(conditions, " - ",2)[,2]
  
  for (k in 1: nrow(boutTime)){
    if (boutTime[k,'xPlot']==Label[i]){
      boutTime[k,'xPlot'] <- v
    }
  }
  
  BoutTimeData(boutTime)
  
})
observeEvent(input$boutTimePlotsTabs,{
  
  req(damData$dt)
  req(BoutTimeData())
  summaryDT_melted <- boutTimeSummary()
  
  BoutTimeData(summaryDT_melted)
})

#Sleep latency
observeEvent(input$SleepLatencySummary_cell_edit,{
  
  info <- input$SleepLatencySummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  sleepLatency <- BoutSleepLatencyData()
  conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=sleepLatency, FUN=mean)[,1]
  Label <-  str_split_fixed(conditions, " - ",2)[,2]
  
  for (k in 1:nrow(sleepLatency)){
    if (sleepLatency[k,'xPlot']==Label[i]){
      sleepLatency[k,'xPlot'] <- v
    }
  }
  BoutSleepLatencyData(sleepLatency)
  
})
observeEvent(input$SleepLatencyPlotsTabs,{
  
  req(damData$dt)
  req(BoutSleepLatencyData())
  summaryDT_melted <- boutSleepLatencySummary()
  
  BoutSleepLatencyData(summaryDT_melted)
})

#Bout sleep time
observeEvent(input$SleepTimeSummary_cell_edit,{
  
  info <- input$SleepTimeSummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  sleepTime <- BoutSleepTimeData()
  conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=sleepTime, FUN=mean)[,1]
  Label <-  str_split_fixed(conditions, " - ",2)[,2]
  
  for (k in 1:nrow(sleepTime)){
    if (sleepTime[k,'xPlot']==Label[i]){
      sleepTime[k,'xPlot'] <- v
    }
  }
  BoutSleepTimeData(sleepTime)
  
})
observeEvent(input$SleepTimePlotsTabs,{
  
  req(damData$dt)
  req(BoutSleepTimeData())
  summaryDT_melted <- boutSleepTimeSummary()
  
  BoutSleepLatencyData(summaryDT_melted)
})

#Update data presented
observe({
  req(nrow(damData$dt)>0)
  req(BoutActivityData())
  req(BoutTimeData())
  
  
  output$BoutActivitySummary <- DT::renderDataTable(dataReport(BoutActivityData()),
                                                escape = FALSE, selection = 'none', 
                                                editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
  output$BoutTimeSummary <- DT::renderDataTable(dataReport(BoutTimeData()),
                                             escape = FALSE, selection = 'none', 
                                             editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
  output$SleepTimeSummary <- DT::renderDataTable(dataReport(BoutSleepTimeData()),
                                                 escape = FALSE, selection = 'none', 
                                                 editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
  output$SleepLatencySummary <- DT::renderDataTable(dataReport(BoutSleepLatencyData()),
                                                      escape = FALSE, selection = 'none', 
                                                      editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
})

observe({
  
  if(is.null(damData$dt)){
    BoutActivityData(NULL)
    BoutTimeData(NULL)
    BoutSleepTimeData(NULL)
    BoutSleepLatencyData(NULL)}
})


############################### PLOTS ####################################

#Bout activity box plots
output$boutActivityDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData())>0, "")
  )
  
  fig <- statisticPlots(BoutActivityData() ,input$boutPlot,input$error, graph = "boutActivity")
  
  if (input$BoutActivityBoxTitle == ""){
    fig <- fig+ggtitle(paste("Total bout activity during Day and Night times"))}
  else{
    fig <- fig+ggtitle(input$BoutActivityBoxTitle)
  }
  
  BoutActivityFig(fig)
  
  return(fig)
  
  
  
},width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$boutActivityPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData())>0, "")
  )

  fig <- statisticPlots(BoutActivityData() ,input$boutPlot,input$error, graph = "boutActivity")
  
  if (input$BoutActivityBoxTitle == ""){
    fig <- fig+ggtitle(paste("Total bout activity per day"))}
  else{
    fig <- fig+ggtitle(input$BoutActivityBoxTitle)
  }
  
  BoutActivityFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$boutActivityDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData())>0, "")
  )

  fig <- statisticPlots(BoutActivityData() ,input$boutPlot,input$error, graph = "boutActivity")
  
  if (input$BoutActivityBoxTitle == ""){
    fig <- fig+ggtitle(paste("Total bout activity per light and dark phases"))}
  else{
    fig <- fig+ggtitle(input$BoutActivityBoxTitle)
  }
  
  BoutActivityFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$boutActivityCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData())>0, "")
  )
  
  req(input$boutGroupBoxTime)
  req(input$boutBoxTime)
  
  fig <- statisticPlots(BoutActivityData() ,input$boutPlot,input$error, graph = "boutActivity")
  
  if (input$ActivityBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean bout activity"))}
  else{
    fig <- fig+ggtitle(input$BoutActivityBoxTitle)
  }
  
  BoutActivityFig(fig)
  
  
  return(fig)
  
},width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)


#Bout time box plots
output$boutTimeDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData())>0, "")
  )
  
  fig <- statisticPlots(BoutTimeData() ,input$boutPlot,input$error, graph = "boutTime")
  
  if (input$BoutTimeBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean bouts time ratio during Day and Night times"))}
  else{
    fig <- fig+ggtitle(input$SleepBoxTitle)
  }
  
  BoutTimeFig(fig)
  
  
  return(fig)
  
  
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$boutTimePerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData())>0, "")
  )
  
  fig <- statisticPlots(BoutTimeData() ,input$boutPlot,input$error, graph = "boutTime")
  
  if (input$BoutTimeBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean bouts time ratio per day"))}
  else{
    fig <- fig+ggtitle(input$BoutTimeBoxTitle)
  }
  
  BoutTimeFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$boutTimeDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData())>0, "")
  )
  
  fig <- statisticPlots(BoutTimeData() ,input$boutPlot,input$error, graph = "boutTime")
  
  if (input$BoutTimeBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean bouts time ratio per light and dark phases"))}
  else{
    fig <- fig+ggtitle(input$BoutTimeBoxTitle)
  }
  
  BoutTimeFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$boutTimeCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData())>0, "")
  )
  req(input$boutGroupBoxTime)
  req(input$boutBoxTime)
  
  fig <- statisticPlots(BoutTimeData() ,input$boutPlot,input$error, graph = "boutTime")
  
  if (input$BoutTimeBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean bouts time"))}
  else{
    fig <- fig+ggtitle(input$BoutTimeBoxTitle)
  }
  
  BoutTimeFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)


#Sleep latency box plots
output$SleepLatencyDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepLatencyData())>0, "")
  )
  
  fig <- statisticPlots(BoutSleepLatencyData() ,input$boutPlot,input$error, graph = "sleepLatency")
  
  if (input$SleepLatencyBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep latency"))}
  else{
    fig <- fig+ggtitle(input$SleepLatencyBoxTitle)
  }
  
  BoutSleepLatencyFig(fig)
  
  
  return(fig)
  
  
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$SleepLatencyPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepLatencyData())>0, "")
  )
  
  fig <- statisticPlots(BoutSleepLatencyData() ,input$boutPlot,input$error, graph = "sleepLatency")
  
  if (input$SleepLatencyBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep latency per day"))}
  else{
    fig <- fig+ggtitle(input$SleepLatencyBoxTitle)
  }
  
  BoutSleepLatencyFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$SleepLatencyCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepLatencyData())>0, "")
  )
  req(input$boutGroupBoxTime)
  req(input$boutBoxTime)
  
  fig <- statisticPlots(BoutSleepLatencyData() ,input$boutPlot,input$error, graph = "sleepLatency")
  
  if (input$SleepLatencyBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep latency"))}
  else{
    fig <- fig+ggtitle(input$SleepLatencyBoxTitle)
  }
  
  BoutSleepLatencyFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)


#Sleep time box plots
output$SleepTimeDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData())>0, "")
  )
  
  fig <- statisticPlots(BoutSleepTimeData() ,input$boutPlot,input$error, graph = "sleepTime")
  
  if (input$SleepTimeBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep bout time during Day and Night times"))}
  else{
    fig <- fig+ggtitle(input$SleepTimeBoxTitle)
  }
  
  BoutSleepTimeFig(fig)
  
  
  return(fig)
  
  
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$SleepTimePerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData())>0, "")
  )
  
  fig <- statisticPlots(BoutSleepTimeData() ,input$boutPlot,input$error, graph = "sleepLatency")
  
  if (input$SleepTimeBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep bout time per day"))}
  else{
    fig <- fig+ggtitle(input$SleepTimeBoxTitle)
  }
  
  BoutSleepTimeFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$SleepTimeDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData())>0, "")
  )
  
  fig <- statisticPlots(BoutSleepTimeData() ,input$boutPlot,input$error, graph = "sleepLatency")
  
  if (input$SleepTimeBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep bouts time per light and dark phases"))}
  else{
    fig <- fig+ggtitle(input$SleepTimeBoxTitle)
  }
  
  BoutSleepTimeFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$SleepTimeCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData())>0, "")
  )
  req(input$boutGroupBoxTime)
  req(input$boutBoxTime)
  
  fig <- statisticPlots(BoutSleepTimeData() ,input$boutPlot,input$error, graph = "sleepLatency")
  
  if (input$SleepTimeBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep bouts time"))}
  else{
    fig <- fig+ggtitle(input$SleepTimeBoxTitle)
  }
  
  BoutSleepTimeFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)

######################## SAVE FIGS AND DATA #############################


#save Figures
observe({
  req(BoutActivityFig())
  
  output$saveBoutActivityFig <- downloadHandler(
    
    filename = function(){
      paste0(input$boutActivityPlotsTabs,input$BoutActivityFig)
    },
    content = function(file){
      
      ggsave(filename = file, plot = BoutActivityFig(),
         width = round(input$periodicWidth/97), height= round(input$periodicHeight/97))
    })
})
observe({
  req(BoutTimeFig())
  
  output$saveBoutTimeFig <- downloadHandler(
    
    filename = function(){
      paste0(input$boutTimePlotsTabs,input$BoutTimeFig)
    },
    content = function(file){
  
  ggsave(filename = file, plot = BoutTimeFig(),
         width = round(input$periodicWidth/97), height= round(input$periodicHeight/97))
    })
})
observe({
  req(BoutSleepLatencyFig())
  
  output$saveSleepLatencyFig <- downloadHandler(
    
    filename = function(){
      paste0(input$SleepLatencyPlotsTabs,input$SleepLatencyFig)
    },
    content = function(file){
      
      ggsave(filename = file, plot = BoutSleepLatencyFig(),
             width = round(input$periodicWidth/97), height= round(input$periodicHeight/97))
    })
})
observe({
  req(BoutSleepTimeFig())
  
  output$saveSleepTimeFig <- downloadHandler(
    
    filename = function(){
      paste0(input$SleepTimePlotsTabs,input$SleepTimeFig)
    },
    content = function(file){
      
      ggsave(filename = file, plot = BoutSleepTimeFig(),
             width = round(input$periodicWidth/97), height= round(input$periodicHeight/97))
    })
})


#Save graph data
observe({
  req(damData$dt)
  req(BoutActivityData())
  
  output$saveBoutActivityReport <- downloadHandler(
    
    filename = function(){
      paste0(input$boutActivityPlotsTabs,".xlsx")
    },
    content = function(file){
  
      #Create xlsx workbook of conditions and zeitgeber table
      wb<-createWorkbook(type="xlsx")
      sheet <- createSheet(wb,"Replicates")
      addDataFrame(saveReport(BoutActivityData()), sheet=sheet, startColumn=1, row.names=FALSE)
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(dataReport(BoutActivityData()), sheet=sheet, startColumn=1, row.names=FALSE)
      
      saveWorkbook(wb, file = file)
    })
})
observe({
  req(BoutTimeData())
  
  output$saveBoutTimeReport <- downloadHandler(
    
    filename = function(){
      paste0(input$boutTimePlotsTabs,".xlsx")
    },
    content = function(file){
  
      #Create xlsx workbook of conditions and zeitgeber table
      wb<-createWorkbook(type="xlsx")
      sheet <- createSheet(wb,"Replicates")
      addDataFrame(saveReport(BoutTimeData()), sheet=sheet, startColumn=1, row.names=FALSE)
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(dataReport(BoutTimeData()), sheet=sheet, startColumn=1, row.names=FALSE)
      
      saveWorkbook(wb, file = file)
    })
})
observe({
  req(BoutSleepLatencyData())
  
  output$saveSleepLatencyReport <- downloadHandler(
    
    filename = function(){
      paste0(input$SleepLatencyPlotsTabs,".xlsx")
    },
    content = function(file){
      
      #Create xlsx workbook of conditions and zeitgeber table
      wb<-createWorkbook(type="xlsx")
      sheet <- createSheet(wb,"Replicates")
      addDataFrame(saveReport(BoutSleepLatencyData()), sheet=sheet, startColumn=1, row.names=FALSE)
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(dataReport(BoutSleepLatencyData()), sheet=sheet, startColumn=1, row.names=FALSE)
      
      saveWorkbook(wb, file = file)
    })
})
observe({
  req(BoutSleepTimeData())
  
  output$saveSleepTimeReport <- downloadHandler(
    
    filename = function(){
      paste0(input$SleepTimePlotsTabs,".xlsx")
    },
    content = function(file){
      
      #Create xlsx workbook of conditions and zeitgeber table
      wb<-createWorkbook(type="xlsx")
      sheet <- createSheet(wb,"Replicates")
      addDataFrame(saveReport(BoutSleepTimeData()), sheet=sheet, startColumn=1, row.names=FALSE)
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(dataReport(BoutSleepTimeData()), sheet=sheet, startColumn=1, row.names=FALSE)
      
      saveWorkbook(wb, file = file)
    })
})