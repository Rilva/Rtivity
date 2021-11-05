############################# VARIABLES ##################################

### Activity per bout data and figure variables
BoutActivityData <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
BoutActivityFigures <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
BoutActivityFiguresTitles <- reactiveValues(lightDark = "Mean activity per bout per light phase", day = "Mean activity per bout per day", 
                                        dayLightDark = "Mean activity per bout day and per light phase", custom = "Mean activity per bout")
BoutActivityFiguresXLabel <- reactiveVal("")
BoutActivityFiguresYLabel <- reactiveVal("Mean activity per bout")

### Time per bout data and figure variables
BoutTimeData <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
BoutTimeFigures <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
BoutTimeFiguresTitles <- reactiveValues(lightDark = "Mean bout duration per light phase", day = "Mean bout duration per day", 
                                            dayLightDark = "Mean bout duration day and per light phase", custom = "Mean duration per bout")
BoutTimeFiguresXLabel <- reactiveVal("")
BoutTimeFiguresYLabel <- reactiveVal("Mean bout duration")

### Time per sleep bout data and figure variables
BoutSleepTimeData <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
BoutSleepTimeFigures <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
BoutSleepTimeFiguresTitles <- reactiveValues(lightDark = "Mean sleep bout duration per light phase", day = "Mean sleep bout duration per day", 
                                     dayLightDark = "Mean sleep bout duration per day and per light phase", custom = "Mean sleep bout duration")
BoutSleepTimeFiguresXLabel <- reactiveVal("")
BoutSleepTimeFiguresYLabel <- reactiveVal("Mean sleep bout duration")

### Sleep Latency data and figure variables
BoutSleepLatencyData <- reactiveValues(all = NULL, day = NULL, custom = NULL)
BoutSleepLatencyFigures <- reactiveValues(all = NULL, day = NULL, custom = NULL)
BoutSleepLatencyFiguresTitles <- reactiveValues(all = "Mean sleep latency", day = "Mean sleep latency per day", 
                                             custom = "Mean sleep latency")
BoutSleepLatencyFiguresXLabel <- reactiveVal("")
BoutSleepLatencyFiguresYLabel <- reactiveVal("Mean sleep latency")

##################### Restart variables #####################
observeEvent(input$files,{
  
  #Get directory
  validate(
    need(nrow(input$files)>0,""))
  
  ### Bouts statistics
  #Bout activity
  BoutActivityData$lightDark <- NULL
  BoutActivityData$day <- NULL
  BoutActivityData$dayLightDark <- NULL
  BoutActivityData$custom <- NULL
  
  BoutActivityFigures$lightDark <- NULL
  BoutActivityFigures$day <- NULL
  BoutActivityFigures$dayLightDark <- NULL
  BoutActivityFigures$custom <- NULL
  
  BoutActivityFiguresTitles$lightDark <- "Mean activity per bout per light phase"
  BoutActivityFiguresTitles$day <- "Mean activity per bout per day"
  BoutActivityFiguresTitles$dayLightDark <- "Mean activity per bout day and per light phase"
  BoutActivityFiguresTitles$custom <- "Mean activity per bout"
  BoutActivityFiguresXLabel("")
  BoutActivityFiguresYLabel("Mean activity per bout")
  
  #Bout time
  BoutTimeData$lightDark <- NULL
  BoutTimeData$day <- NULL
  BoutTimeData$dayLightDark <- NULL
  BoutTimeData$custom <- NULL
  
  BoutTimeFigures$lightDark <- NULL
  BoutTimeFigures$day <- NULL
  BoutTimeFigures$dayLightDark <- NULL
  BoutTimeFigures$custom <- NULL
  
  BoutTimeFiguresTitles$lightDark <- "Mean bout duration per light phase"
  BoutTimeFiguresTitles$day <- "Mean bout duration per day"
  BoutTimeFiguresTitles$dayLightDark <- "Mean bout duration day and per light phase"
  BoutTimeFiguresTitles$custom <- "Mean bout duration"
  BoutTimeFiguresXLabel("")
  BoutTimeFiguresYLabel("Mean bout duration")
  
  #Sleep bout time
  BoutSleepTimeData$lightDark <- NULL
  BoutSleepTimeData$day <- NULL
  BoutSleepTimeData$dayLightDark <- NULL
  BoutSleepTimeData$custom <- NULL
  
  BoutSleepTimeFigures$lightDark <- NULL
  BoutSleepTimeFigures$day <- NULL
  BoutSleepTimeFigures$dayLightDark <- NULL
  BoutSleepTimeFigures$custom <- NULL
  
  BoutSleepTimeFiguresTitles$lightDark <- "Mean sleep bout duration per light phase"
  BoutSleepTimeFiguresTitles$day <- "Mean sleep bout duration per day"
  BoutSleepTimeFiguresTitles$dayLightDark <- "Mean sleep bout duration per day and per light phase"
  BoutSleepTimeFiguresTitles$custom <- "Mean sleep bout duration"
  BoutSleepTimeFiguresXLabel("")
  BoutSleepTimeFiguresYLabel("Mean sleep bout duration")
  
  #Sleep latency
  
  BoutSleepLatencyData$all <- NULL
  BoutSleepLatencyData$dayLightDark <- NULL
  BoutSleepLatencyData$custom <- NULL
  
  BoutSleepLatencyFigures$all <- NULL
  BoutSleepLatencyFigures$dayLightDark <- NULL
  BoutSleepLatencyFigures$custom <- NULL
  
  BoutSleepLatencyFiguresTitles$all <- "Mean sleep latency"
  BoutSleepLatencyFiguresTitles$dayLightDark <- "Mean sleep latency per day"
  BoutSleepLatencyFiguresTitles$custom <- "Mean sleep latency"
  BoutSleepLatencyFiguresXLabel("")
  BoutSleepLatencyFiguresYLabel("Mean sleep latency")
  
  #Clean data presented
  output$BoutActivitySummary <- NULL
  output$BoutTimeSummary <- NULL
  output$SleepTimeSummary <- NULL
  output$SleepLatencySummary <- NULL
})

############################ Data to plot ################################

##### Data 
#Activity bouts to plot
boutActivitySummary <- function(graphs){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ##### Create data for statistic plots #####
  if(graphs == "lightDark"){
    
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
  
  if(graphs == "day"){
    
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
  
  if(graphs == "dayLightDark"){
    
    
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
  
  if(graphs == "custom"){
    
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
boutTimeSummary <- function(graphs){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ##### Create data for statistic plots #####
  
  if(graphs == "lightDark"){
    
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
  
  if(graphs == "day"){
    
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
  
  if(graphs == "dayLightDark"){
    
    
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
  
  if(graphs == "custom"){
    
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
  
  summaryDT_melted[,'yPlot':= summaryDT_melted[,'yPlot']/60]
  
  return(summaryDT_melted)
}

#Sleep bouts to plot
boutSleepLatencySummary <- function(graphs){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ##### Create data for statistic plots #####
  
  if(graphs == "all"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(SleepLatency = mean(sleepLatency[sleepLatency > 0],na.rm=TRUE)), by = id])
    
    #Get summary by light and dark phase  
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("SleepLatency"),variable.name = "xPlot", value.name = "yPlot")
    
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "SleepLatency", "Sleep_latency")]
  }
  
  if(graphs == "day"){
    
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
  
  if(graphs == "custom"){
    
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
  
  # if (any(is.nan(summaryDT_melted[,yPlot]))){
  #   summaryDT_melted[which(is.nan(summaryDT_melted[,yPlot])),yPlot:=0]
  # }
  
  summaryDT_melted[,'yPlot':= summaryDT_melted[,'yPlot']/60]
  
  return(summaryDT_melted)
}
boutSleepTimeSummary <- function(graphs){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ##### Create data for statistic plots #####
  
  if(graphs == "lightDark"){
    
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
  
  if(graphs == "day"){
    
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
  
  if(graphs == "dayLightDark"){
    
    
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
  
  if(graphs == "custom"){
    
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
  
  summaryDT_melted[,'yPlot':= summaryDT_melted[,'yPlot']/60]
  
  return(summaryDT_melted)
  
}

#Attribute data to variables
ActivityBoutsData <- function(graph = NaN, all=TRUE){
  
  if (all == TRUE){
    if (is.nan(graph) | graph == "BoutActivity"){
      withProgress(message = 'Compute activity per bout statistics', value = 0, {
      BoutActivityData$lightDark <- boutActivitySummary("lightDark")
      incProgress(1/3)
      BoutActivityData$day <- boutActivitySummary("day")
      incProgress(1/3)
      BoutActivityData$dayLightDark <- boutActivitySummary("dayLightDark")
      incProgress(1/3)})
    }
    
    if (is.nan(graph) | graph == "BoutTime"){
      withProgress(message = 'Compute bout duration statistics', value = 0, {
      BoutTimeData$lightDark <- boutTimeSummary("lightDark")
      incProgress(1/3)
      BoutTimeData$day <- boutTimeSummary("day")
      incProgress(1/3)
      BoutTimeData$dayLightDark <- boutTimeSummary("dayLightDark")
      incProgress(1/3)})
      }}
  
  if (is.nan(graph) | graph == "BoutActivity"){
    BoutActivityData$custom <- boutActivitySummary("custom")}
  if (is.nan(graph) | graph == "BoutTime"){
    BoutTimeData$custom <- boutTimeSummary("custom")}
}
SleepBoutsData <- function(graph = NaN, all=TRUE){
  
  if (all == TRUE){
    if (is.nan(graph) | graph == "BoutActivity"){
      withProgress(message = 'Compute sleep bout duration statistics', value = 0, {
      BoutSleepTimeData$lightDark <- boutSleepTimeSummary("lightDark")
      incProgress(1/3)
      BoutSleepTimeData$day <- boutSleepTimeSummary("day")
      incProgress(1/3)
      BoutSleepTimeData$dayLightDark <- boutSleepTimeSummary("dayLightDark")
      incProgress(1/3)})
      }
    
    if (is.nan(graph) | graph == "BoutTime"){
      withProgress(message = 'Compute sleep latency statistics', value = 0, {
      BoutSleepLatencyData$all <- boutSleepLatencySummary("all")
      incProgress(1/2)
      BoutSleepLatencyData$day <- boutSleepLatencySummary("day")
      incProgress(1/2)})
      }}
  
  if (is.nan(graph) | graph == "BoutActivity"){
    BoutSleepTimeData$custom <- boutSleepTimeSummary("custom")}
  if (is.nan(graph) | graph == "BoutTime"){
    BoutSleepLatencyData$custom <- boutSleepLatencySummary("custom")}
}

##### Update Y axis range #####
#Activity bouts
updateYBoutActivity <- function() {
  
  if (input$boutActivityPlotsTabs == "Activity per bout per light phase"){
    BoutActivityFigures$lightDark <- BoutActivityFigures$lightDark + coord_cartesian(ylim = input$yLimitsBoutActivity)
  }
  else{
    if (input$boutActivityPlotsTabs == "Activity per bout per day"){
      BoutActivityFigures$day <- BoutActivityFigures$day + coord_cartesian(ylim = input$yLimitsBoutActivity)
    }
    else{
      if(input$boutActivityPlotsTabs == "Activity per bout per day and light phase"){
        BoutActivityFigures$dayLightDark <- BoutActivityFigures$dayLightDark + coord_cartesian(ylim = input$yLimitsBoutActivity)
      }
      else{
        BoutActivityFigures$custom <- BoutActivityFigures$custom + coord_cartesian(ylim = input$yLimitsBoutActivity)
      }
    }
  }
}
updateYBoutTime<- function() {
  
  if (input$boutTimePlotsTabs == "Bout duration per light phase"){
    BoutTimeFigures$lightDark <- BoutTimeFigures$lightDark + coord_cartesian(ylim = input$yLimitsBoutTime)
  }
  else{
    if (input$boutTimePlotsTabs == "Bout duration per day"){
      BoutTimeFigures$day <- BoutTimeFigures$day + coord_cartesian(ylim = input$yLimitsBoutTime)
    }
    else{
      if(input$boutTimePlotsTabs == "Bout duration per day and light phase"){
        BoutTimeFigures$dayLightDark <- BoutTimeFigures$dayLightDark + coord_cartesian(ylim = input$yLimitsBoutTime)
      }
      else{
        BoutTimeFigures$custom <- BoutTimeFigures$custom + coord_cartesian(ylim = input$yLimitsBoutTime)
      }
    }
  }
}

#Sleep bouts
updateYSleepTime <- function() {
  
  if (input$SleepTimePlotsTabs == "Sleep bout duration per light phase"){
    BoutSleepTimeFigures$lightDark <- BoutSleepTimeFigures$lightDark + coord_cartesian(ylim = input$yLimitsBoutSleepTime)
  }
  else{
    if (input$SleepTimePlotsTabs == "Sleep bout duration per day"){
      BoutSleepTimeFigures$day <- BoutSleepTimeFigures$day + coord_cartesian(ylim = input$yLimitsBoutSleepTime)
    }
    else{
      if(input$SleepTimePlotsTabs == "Sleep bout duration per day and light phase"){
        BoutSleepTimeFigures$dayLightDark <- BoutSleepTimeFigures$dayLightDark + coord_cartesian(ylim = input$yLimitsBoutSleepTime)
      }
      else{
        BoutSleepTimeFigures$custom <- BoutSleepTimeFigures$custom + coord_cartesian(ylim = input$yLimitsBoutSleepTime)
      }
    }
  }
}
updateYSleepLatency <- function() {
  
  if (input$SleepLatencyPlotsTabs == "Sleep latency"){
    BoutSleepLatencyFigures$lightDark <- BoutSleepLatencyFigures$lightDark + coord_cartesian(ylim = input$yLimitsSleepLatency)
  }
  else{
    if (input$SleepLatencyPlotsTabs == "Sleep latency per day"){
      BoutSleepLatencyFigures$day <- BoutSleepLatencyFigures$day + coord_cartesian(ylim = input$yLimitsSleepLatency)
    }
    else{
      BoutSleepLatencyFigures$custom <- BoutSleepLatencyFigures$custom + coord_cartesian(ylim = input$yLimitsSleepLatency)
    }
  }
}

##### Update Labels #####
updateActivityBoutsLabels <- function(){
  
  BoutActivityFiguresXLabel(input$BoutActivityBoxXLabel)
  BoutActivityFiguresYLabel(input$BoutActivityBoxYLabel)
  
  if (input$boutActivityPlotsTabs == "Activity per bout per light phase"){
    BoutActivityFiguresTitles$lightDark <- input$BoutActivityBoxTitle
  }
  else{
    if (input$boutActivityPlotsTabs == "Activity per bout per day"){
      BoutActivityFiguresTitles$day <- input$BoutActivityBoxTitle
    }
    else{
      if(input$boutActivityPlotsTabs == "Activity per bout per day and light phase"){
        BoutActivityFiguresTitles$dayLightDark <- input$BoutActivityBoxTitle
      }
      else{
        BoutActivityFiguresTitles$custom <- input$BoutActivityBoxTitle
      }
    }
  }
  
  BoutTimeFiguresXLabel(input$BoutTimeBoxXLabel)
  BoutTimeFiguresYLabel(input$BoutTimeBoxYLabel)
  
  if (input$boutTimePlotsTabs == "Bout duration per light phase"){
    BoutTimeFiguresTitles$lightDark <- input$BoutTimeBoxTitle
  }
  else{
    if (input$boutTimePlotsTabs == "Bout duration per day"){
      BoutTimeFiguresTitles$day <- input$BoutTimeBoxTitle
    }
    else{
      if(input$boutTimePlotsTabs == "Bout duration per day and light phase"){
        BoutTimeFiguresTitles$dayLightDark <- input$BoutTimeBoxTitle
      }
      else{
        BoutTimeFiguresTitles$custom <- input$BoutTimeBoxTitle
      }
    }
  }
}
updateSleepBoutsLabels <- function(){
  
  BoutSleepTimeFiguresXLabel(input$SleepTimeBoxXLabel)
  BoutSleepTimeFiguresYLabel(input$SleepTimeBoxYLabel)
  
  if (input$SleepTimePlotsTabs == "Sleep bout duration per light phase"){
    BoutSleepTimeFiguresTitles$lightDark <- input$SleepTimeBoxTitle
  }
  else{
    if (input$SleepTimePlotsTabs == "Sleep bout duration per day"){
      BoutSleepTimeFiguresTitles$day <- input$SleepTimeBoxTitle
    }
    else{
      if(input$SleepTimePlotsTabs == "Sleep bout duration per day and light phase"){
        BoutSleepTimeFiguresTitles$dayLightDark <- input$SleepTimeBoxTitle
      }
      else{
        BoutSleepTimeFiguresTitles$custom <- input$SleepTimeBoxTitle
      }
    }
  }
  
  BoutSleepLatencyFiguresXLabel(input$SleepLatencyBoxXLabel)
  BoutSleepLatencyFiguresYLabel(input$SleepLatencyBoxYLabel)
  
  if (input$SleepLatencyPlotsTabs == "Sleep latency"){
    BoutSleepLatencyFiguresTitles$lightDark <- input$SleepLatencyBoxTitle
  }
  else{
    if (input$SleepLatencyPlotsTabs == "Sleep latency per day"){
      BoutSleepLatencyFiguresTitles$day <- input$SleepLatencyBoxTitle
    }
    else{
      BoutSleepLatencyFiguresTitles$custom <- input$SleepLatencyBoxTitle
    }
  }
}

##### Create and update Figures #####
# Bout Activity
updateBoutActivityLightDark <- function(){
  ##### Bout activity per light and dark phases figure #####
  fig <- statisticPlots(BoutActivityData$lightDark ,input$boutPlot,input$boutError)+
    labs(title = BoutActivityFiguresTitles$lightDark, x = BoutActivityFiguresXLabel(),
         y = BoutActivityFiguresYLabel())
  
  BoutActivityFigures$lightDark <- fig
}
updateBoutActivityDay <- function(){
  ##### Bout activity per day #####
  fig <- statisticPlots(BoutActivityData$day ,input$boutPlot,input$boutError)+
    labs(title = BoutActivityFiguresTitles$day, x = BoutActivityFiguresXLabel(),
         y = BoutActivityFiguresYLabel())
  
  BoutActivityFigures$day <- fig
}
updateBoutActivityDayLightDark <- function(){
  ##### Bout activity per day per light and dark phases figure 
  fig <- statisticPlots(BoutActivityData$dayLightDark ,input$boutPlot,input$boutError)+
    labs(title = BoutActivityFiguresTitles$dayLightDark, x = BoutActivityFiguresXLabel(),
         y = BoutActivityFiguresYLabel())
  
  BoutActivityFigures$dayLightDark <- fig
}
updateBoutActivityCustom <- function(){
  ##### Bout activity custom figure 
  fig <- statisticPlots(BoutActivityData$custom ,input$boutPlot,input$boutError)+
    labs(title = BoutActivityFiguresTitles$custom, x = BoutActivityFiguresXLabel(),
         y = BoutActivityFiguresYLabel())
  
  BoutActivityFigures$custom <- fig
}

updateBoutActivityFigures <- function (all=TRUE) {
  
  if (all == TRUE){
    withProgress(message = 'Activity per bout figures', value = 0, {
    updateBoutActivityLightDark()
    incProgress(1/3)
    updateBoutActivityDay()
    incProgress(1/3)
    updateBoutActivityDayLightDark()
    incProgress(1/3)})
  }
  updateBoutActivityCustom()
  
}

# Bout Time
updateBoutTimeLightDark <- function(){
  ##### Bout time per light and dark phases figure 
  fig <- statisticPlots(BoutTimeData$lightDark ,input$boutPlot,input$boutError)+
    labs(title = BoutTimeFiguresTitles$lightDark, x = BoutTimeFiguresXLabel(),
         y = BoutTimeFiguresYLabel())
  
  BoutTimeFigures$lightDark <- fig
}
updateBoutTimeDay <- function(){
  ##### Bout time per day 
  fig <- statisticPlots(BoutTimeData$day ,input$boutPlot,input$boutError)+
    labs(title = BoutTimeFiguresTitles$day, x = BoutTimeFiguresXLabel(),
         y = BoutTimeFiguresYLabel())
  
  BoutTimeFigures$day <- fig
}
updateBoutTimeDayLightDark <- function(){
  ##### Bout time per day per light and dark phases figure
  fig <- statisticPlots(BoutTimeData$dayLightDark ,input$boutPlot,input$boutError)+
    labs(title = BoutTimeFiguresTitles$dayLightDark, x = BoutTimeFiguresXLabel(),
         y = BoutTimeFiguresYLabel())
  
  BoutTimeFigures$dayLightDark <- fig
}
updateBoutTimeCustom <- function(){
  ##### Bout time custom figure 
  fig <- statisticPlots(BoutTimeData$custom ,input$boutPlot,input$boutError)+
    labs(title = BoutTimeFiguresTitles$custom, x = BoutTimeFiguresXLabel(),
         y = BoutTimeFiguresYLabel())
  
  BoutTimeFigures$custom <- fig
}

updateBoutTimeFigures <- function (all=TRUE) {
  
  if (all == TRUE){
    withProgress(message = 'Bout duration figures', value = 0, {
    updateBoutTimeLightDark()
    incProgress(1/3)
    updateBoutTimeDay()
    incProgress(1/3)
    updateBoutTimeDayLightDark()
    incProgress(1/3)})
  }
  updateBoutTimeCustom()
  
}

# Sleep Bout Time
updateBoutSleepTimeLightDark <- function(){
  ##### Sleep Bout time per light and dark phases figure #####
  fig <- statisticPlots(BoutSleepTimeData$lightDark ,input$boutPlot,input$boutError)+
    labs(title = BoutSleepTimeFiguresTitles$lightDark, x = BoutSleepTimeFiguresXLabel(),
         y = BoutSleepTimeFiguresYLabel())
  
  BoutSleepTimeFigures$lightDark <- fig
}
updateBoutSleepTimeDay <- function(){
  ##### Sleep Bout time per day #####
  fig <- statisticPlots(BoutSleepTimeData$day ,input$boutPlot,input$boutError)+
    labs(title = BoutSleepTimeFiguresTitles$day, x = BoutSleepTimeFiguresXLabel(),
         y = BoutSleepTimeFiguresYLabel())
  
  BoutSleepTimeFigures$day <- fig
}
updateBoutSleepTimeDayLightDark <- function(){
  ##### SleepBout time per day per light and dark phases figure #####
  fig <- statisticPlots(BoutSleepTimeData$dayLightDark ,input$boutPlot,input$boutError)+
    labs(title = BoutSleepTimeFiguresTitles$dayLightDark, x = BoutSleepTimeFiguresXLabel(),
         y = BoutSleepTimeFiguresYLabel())
  
  BoutSleepTimeFigures$dayLightDark <- fig
}
updateBoutSleepTimeCustom <- function(){
  ##### Sleep bout time custom figure #####
  fig <- statisticPlots(BoutSleepTimeData$custom ,input$boutPlot,input$boutError)+
    labs(title = BoutSleepTimeFiguresTitles$custom, x = BoutSleepTimeFiguresXLabel(),
         y = BoutSleepTimeFiguresYLabel())
  
  BoutSleepTimeFigures$custom <- fig
}

updateBoutSleepTimeFigures <- function (all=TRUE) {
  
  if (all == TRUE){
    withProgress(message = 'Sleep bout duration figures', value = 0, {
    updateBoutSleepTimeLightDark()
    incProgress(1/3)
    updateBoutSleepTimeDay()
    incProgress(1/3)
    updateBoutSleepTimeDayLightDark()
    incProgress(1/3)})
  }
  updateBoutSleepTimeCustom()
  
}

# Sleep Latency
updateBoutSleepLatencyAll <- function(){
  ##### Sleep Latency all figure #####
  fig <- statisticPlots(BoutSleepLatencyData$all ,input$boutPlot,input$boutError)+
    labs(title = BoutSleepLatencyFiguresTitles$all, x = BoutSleepLatencyFiguresXLabel(),
         y = BoutSleepLatencyFiguresYLabel())
  
  BoutSleepLatencyFigures$all <- fig
}
updateBoutSleepLatencyDay <- function(){
  ##### Sleep Latency per day #####
  fig <- statisticPlots(BoutSleepLatencyData$day ,input$boutPlot,input$boutError)+
    labs(title = BoutSleepLatencyFiguresTitles$day, x = BoutSleepLatencyFiguresXLabel(),
         y = BoutSleepLatencyFiguresYLabel())
  
  BoutSleepLatencyFigures$day <- fig
}
updateBoutSleepLatencyCustom <- function(){
  ##### Sleep latency custom figure #####
  fig <- statisticPlots(BoutSleepLatencyData$custom ,input$boutPlot,input$boutError)+
    labs(title = BoutSleepLatencyFiguresTitles$custom, x = BoutSleepLatencyFiguresXLabel(),
         y = BoutSleepLatencyFiguresYLabel())
  
  BoutSleepLatencyFigures$custom <- fig
}

updateBoutSleepLatencyFigures <- function (all=TRUE) {
  
  if (all == TRUE){
    withProgress(message = 'Sleep latency figures', value = 0, {
    updateBoutSleepLatencyAll()
    incProgress(1/2)
    updateBoutSleepLatencyDay()
    incProgress(1/2)})
  }
  updateBoutSleepLatencyCustom()
  
}

####################### Data ################################

observeEvent(input$startanalysis,{
  req(damData$dt)
  req("timeDiff" %in% colnames(damData$dt))
  
  ### Bouts statistics
  #Bout activity
  BoutActivityData$lightDark <- NULL
  BoutActivityData$day <- NULL
  BoutActivityData$dayLightDark <- NULL
  BoutActivityData$custom <- NULL
  
  BoutActivityFigures$lightDark <- NULL
  BoutActivityFigures$day <- NULL
  BoutActivityFigures$dayLightDark <- NULL
  BoutActivityFigures$custom <- NULL
  
  BoutActivityFiguresTitles$lightDark <- "Mean activity per bout per light phase"
  BoutActivityFiguresTitles$day <- "Mean activity per bout per day"
  BoutActivityFiguresTitles$dayLightDark <- "Mean activity per bout day and per light phase"
  BoutActivityFiguresTitles$custom <- "Mean activity per bout"
  BoutActivityFiguresXLabel("")
  BoutActivityFiguresYLabel("Mean activity per bout")
  
  #Bout time
  BoutTimeData$lightDark <- NULL
  BoutTimeData$day <- NULL
  BoutTimeData$dayLightDark <- NULL
  BoutTimeData$custom <- NULL
  
  BoutTimeFigures$lightDark <- NULL
  BoutTimeFigures$day <- NULL
  BoutTimeFigures$dayLightDark <- NULL
  BoutTimeFigures$custom <- NULL
  
  BoutTimeFiguresTitles$lightDark <- "Mean bout duration per light phase"
  BoutTimeFiguresTitles$day <- "Mean bout duration per day"
  BoutTimeFiguresTitles$dayLightDark <- "Mean bout duration day and per light phase"
  BoutTimeFiguresTitles$custom <- "Mean bout duration"
  BoutTimeFiguresXLabel("")
  BoutTimeFiguresYLabel("Mean bout duration")
  
  #Sleep bout time
  BoutSleepTimeData$lightDark <- NULL
  BoutSleepTimeData$day <- NULL
  BoutSleepTimeData$dayLightDark <- NULL
  BoutSleepTimeData$custom <- NULL
  
  BoutSleepTimeFigures$lightDark <- NULL
  BoutSleepTimeFigures$day <- NULL
  BoutSleepTimeFigures$dayLightDark <- NULL
  BoutSleepTimeFigures$custom <- NULL
  
  BoutSleepTimeFiguresTitles$lightDark <- "Mean sleep bout duration per light phase"
  BoutSleepTimeFiguresTitles$day <- "Mean sleep bout duration per day"
  BoutSleepTimeFiguresTitles$dayLightDark <- "Mean sleep bout duration per day and per light phase"
  BoutSleepTimeFiguresTitles$custom <- "Mean sleep bout duration"
  BoutSleepTimeFiguresXLabel("")
  BoutSleepTimeFiguresYLabel("Mean sleep bout duration")
  
  #Sleep latency
  
  BoutSleepLatencyData$all <- NULL
  BoutSleepLatencyData$dayLightDark <- NULL
  BoutSleepLatencyData$custom <- NULL
  
  BoutSleepLatencyFigures$all <- NULL
  BoutSleepLatencyFigures$dayLightDark <- NULL
  BoutSleepLatencyFigures$custom <- NULL
  
  BoutSleepLatencyFiguresTitles$all <- "Mean sleep latency"
  BoutSleepLatencyFiguresTitles$dayLightDark <- "Mean sleep latency per day"
  BoutSleepLatencyFiguresTitles$custom <- "Mean sleep latency"
  BoutSleepLatencyFiguresXLabel("")
  BoutSleepLatencyFiguresYLabel("Mean sleep latency")
  
  
  #Clean data presented
  output$BoutActivitySummary <- NULL
  output$BoutTimeSummary <- NULL
  output$SleepTimeSummary <- NULL
  output$SleepLatencySummary <- NULL
  
  #Update bout analysis bin size
  if ((max(damData$dt[,'t'])-min(damData$dt[,'t']))/60 < 180){
    updateSliderInput(session,"boutWindow",max = (max(damData$dt[,'t'])-min(damData$dt[,'t']))/60,
                      min = max(damData$dt[,timeDiff],na.rm=TRUE)/60, step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)
  }
  else{
    updateSliderInput(session,"boutWindow",value = max(damData$dt[,timeDiff],na.rm=TRUE)/60, max = 60,
                      min = max(damData$dt[,timeDiff],na.rm=TRUE)/60, step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)}
  
})

#Bout analysis
observeEvent(input$boutAnalysis,{
  
  req(damData$dt)
  
  # Analyze bouts
  withProgress(message = 'Analyzing bouts', value = 0, {
    
    #Calculus of the time difference between measurements
    damData$dt[,timeDiff := c(NaN,damData$dt[2:nrow(damData$dt),t]- damData$dt[1:(nrow(damData$dt)-1),t])]
    
    meanTimeDiff <- mean(damData$dt[damData$dt[,timeDiff]>0,timeDiff],na.rm=TRUE) #Minimum bin size
    step <- round(mins(input$boutWindow)/meanTimeDiff) #Step for analysis
    
    #Actiivty bouts
    damData$dt[,"movingBout" := !sleep_dam_annotation(damData$dt[,1:3],min_time_immobile = 60*input$boutWindow)[,'asleep']]
    
    #Calculate the start and final indexes for each sensor channel
    finalIndexes <- c()
    startIndexes <- 1
    if (sum(damData$dt[,timeDiff]<0,na.rm=TRUE)>0){
      finalIndexes <- which(damData$dt[,timeDiff] %in% damData$dt[,timeDiff][damData$dt[,timeDiff]<0])-1}
    
    finalIndexes <- c(finalIndexes,nrow(damData$dt))
    if (length(finalIndexes)>1){
      startIndexes <- c(startIndexes,(finalIndexes[1:(length(finalIndexes)-1)]+1))
    }
    
    damData$dt[startIndexes,timeDiff := NaN]
    
    #Introduce the variables bouts, boutTime and boutActivity
    dataSleepBout <- rep(FALSE,nrow(damData$dt))
    dataSleepBoutTime <- rep(0,nrow(damData$dt))
    dataSleepLatency <- rep(0, nrow(damData$dt))
    
    dataActivityBout <- rep(TRUE,nrow(damData$dt))
    dataActivityBoutTime <- rep(0, nrow(damData$dt))
    dataBoutActivity <- rep(0, nrow(damData$dt))
    
    Activity <- damData$dt[,'activity']
    
    validate(
      need(!is.null(startIndexes),"")
    )
    
    start_bouts <- c()
    end_bouts <- c()
    activity <- c()
    
    #Calculate bouts
    for (j in 1:length(startIndexes)){
      
      End <- FALSE
      incProgress(1/length(startIndexes))
      
      #Analyse bouts
      activityBout_dt <- bout_analysis(movingBout, damData$dt[startIndexes[j]:finalIndexes[j],])
      sleepBout_dt <- bout_analysis(asleep, damData$dt[startIndexes[j]:finalIndexes[j],])
      t <- damData$dt[startIndexes[j]:finalIndexes[j],t]
      
      sleepBouts <- sleepBout_dt[asleep == TRUE, -"asleep"]
      activityBouts <- activityBout_dt[movingBout == TRUE, -"movingBout"]
      
      ### Get start and finish indexes of bouts
      sleepT <- sleepBouts[,t]
      activityT <- activityBouts[,t]
      sleepDuration <- sleepBouts[,duration]
      activityDuration <- activityBouts[,duration]
      
      startRowInactivity <- which(t %in% sleepT)
      startRowActivity <- which(t %in% activityT)
      finishRowInactivity <- which(t %in% (sleepT+sleepDuration))
      finishRowActivity <- which(t %in% (activityT+activityDuration))
      
      latency <- TRUE

      #Calculate sleep bouts duration
      if(length(startRowInactivity)>0){
        for (k in 1:length(startRowInactivity)){
          
          dataSleepBout[startIndexes[j]+startRowInactivity[k]-1] <- TRUE
          dataSleepBoutTime[startIndexes[j]+startRowInactivity[k]-1]<- sleepDuration[k]
        }
      }
      
      #Sleep Latency
      periodicTime <- damData$dt[startIndexes[j]:finalIndexes[j],periodT]
      replica_data <- damData$dt[startIndexes[j]:finalIndexes[j],]
      boutReplica <- dataSleepBout[startIndexes[j]:finalIndexes[j]]
      
      threshold_hours <- hours(l_hours())
      
      minimum <- min(damData$dt[,experimentDay])
      maximum <- max(damData$dt[,experimentDay])
      
      for (k in minimum:maximum){
        
        Time <- periodicTime - hours(l_period())*k - threshold_hours

        if(max(Time)>0){
          n <- which(abs(Time) == min(abs(Time)))
          m <- which(boutReplica[Time>0]==TRUE)
            
            if(length(m)>0){
              dataSleepLatency[min(m)+n+startIndexes[j]-1] <- Time[min(m)+n]}
          # }
        }
      }

      #Activity Bouts
      if (length(startRowActivity)>0){
        for (k in 1:min(c(length(startRowActivity),length(finishRowActivity)))){
          
          
          dataActivityBout[(startIndexes[j]+startRowActivity[k]-1)] <- TRUE
          dataActivityBoutTime[(startIndexes[j]+startRowActivity[k]-1)] <- activityDuration[k]
        
          bout_activity <- sum(Activity[(startIndexes[j]+startRowActivity[k]-1):(startIndexes[j]+finishRowActivity[k]-1)],na.rm = TRUE)
          dataBoutActivity[(startIndexes[j]+startRowActivity[k]-1)] <- bout_activity
        }
      }
    }
    
  })
  
  damData$dt[,sleepBout := dataSleepBout]
  damData$dt[,sleepBoutTime := dataSleepBoutTime]
  damData$dt[,sleepLatency:=dataSleepLatency]
  damData$dt[,activityBout:=dataActivityBout]
  damData$dt[,activityBoutTime:=dataActivityBoutTime]
  damData$dt[,boutActivity:=as.numeric(dataBoutActivity)]
  
  settings <- settingsTable()
  settings[1,2] <- l_period()
  settings[2,2] <- l_hours()
  settings[4,2] <- input$boutWindow
  settingsTable(settings)
  
  ### Update figures
  
  ActivityBoutsData()
  SleepBoutsData()
  
  updateTabsetPanel(session, "boutActivityPlotsTabs", selected = "Activity per bout per light phase")
  updateTabsetPanel(session, "boutTimePlotsTabs", selected = "Bout duration per light phase")
  updateTabsetPanel(session, "SleepTimePlotsTabs", selected = "Sleep bout duration per light phase")
  updateTabsetPanel(session, "SleepLatencyPlotsTabs", selected = "Sleep latency")
  
  #### Update Y range sliders
  boutActivity <- BoutActivityData$lightDark
  updateSliderInput(session,'yLimitsBoutActivity',min = 0, max =ceiling(max(boutActivity[,'yPlot'], na.rm =TRUE)), value = c(0,max(boutActivity[,'yPlot'], na.rm =TRUE)))
  
  boutTime <- BoutTimeData$lightDark
  updateSliderInput(session,'yLimitsBoutTime',min = 0, max =ceiling(max(boutTime[,'yPlot'], na.rm =TRUE)), value = c(0,max(boutTime[,'yPlot'], na.rm =TRUE)))
  
  sleepTime <- BoutSleepTimeData$lightDark
  updateSliderInput(session,'yLimitsBoutSleepTime',min = 0, max =ceiling(max(sleepTime[,'yPlot'], na.rm =TRUE)), value = c(0,max(sleepTime[,'yPlot'], na.rm =TRUE)))
  
  sleepLatency <- BoutSleepLatencyData$all
  updateSliderInput(session,'yLimitsSleepLatency',min = 0, max =ceiling(max(sleepLatency[,'yPlot'], na.rm =TRUE)), value = c(0,max(sleepLatency[,'yPlot'], na.rm =TRUE)))
  
  #Create figures
  updateBoutActivityFigures()
  updateBoutTimeFigures()
  updateBoutSleepTimeFigures()
  updateBoutSleepLatencyFigures()
    
})

########################### Change labels ###############################

#Bout Activity
observeEvent(input$BoutActivitySummary_cell_edit,{
  
  info <- input$BoutActivitySummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  changeLabels <- function(boutActivity){
  
    conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=boutActivity, FUN=mean)[,1]
    Label <-  str_split_fixed(conditions, " - ",2)[,2]
    
    for (k in 1:nrow(boutActivity)){
      if (boutActivity[k,'xPlot']==Label[i]){
        boutActivity[k,'xPlot'] <- v
      }
    }
    return(boutActivity)
  }
  
  if (input$boutActivityPlotsTabs == "Activity per bout per light phase"){
    BoutActivityData$lightDark <- changeLabels(BoutActivityData$lightDark)
    updateBoutActivityLightDark()
  }
  else{
    if (input$boutActivityPlotsTabs == "Activity per bout per day"){
      BoutActivityData$day <- changeLabels(BoutActivityData$day)
      updateBoutActivityDay()
    }
    else{
      if(input$boutActivityPlotsTabs == "Activity per bout per day and light phase"){
        BoutActivityData$dayLightDark <- changeLabels(BoutActivityData$dayLightDark)
        updateBoutActivityDayLightDark()
      }
      else{
        BoutActivityData$custom <- changeLabels(BoutActivityData$custom)
        updateBoutActivityCustom()
      }
    }
  }
  
})

#Bout Time
observeEvent(input$BoutTimeSummary_cell_edit,{
  
  info <- input$BoutTimeSummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  changeLabels <- function(boutTime){
    
    conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=boutTime, FUN=mean)[,1]
    Label <-  str_split_fixed(conditions, " - ",2)[,2]
    
    for (k in 1: nrow(boutTime)){
      if (boutTime[k,'xPlot']==Label[i]){
        boutTime[k,'xPlot'] <- v
      }
    }
    return(boutTime)
  }
  
  if (input$boutTimePlotsTabs == "Bout duration per light phase"){
    BoutTimeData$lightDark <- changeLabels(BoutTimeData$lightDark)
    updateBoutTimeLightDark()
  }
  else{
    if (input$boutTimePlotsTabs == "Bout duration per day"){
      BoutTimeData$day <- changeLabels(BoutTimeData$day)
      updateBoutTimeDay()
    }
    else{
      if(input$boutTimePlotsTabs == "Bout duration per day and light phase"){
        BoutTimeData$dayLightDark <- changeLabels(BoutTimeData$dayLightDark)
        updateBoutTimeDayLightDark()
      }
      else{
        BoutTimeData$custom <- changeLabels(BoutTimeData$custom)
        updateBoutTimeCustom()
      }
    }
  }
  
})

#Bout sleep time
observeEvent(input$SleepTimeSummary_cell_edit,{
  
  info <- input$SleepTimeSummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  changeLabels <- function(sleepTime){
    conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=sleepTime, FUN=mean)[,1]
    Label <-  str_split_fixed(conditions, " - ",2)[,2]
    
    for (k in 1:nrow(sleepTime)){
      if (sleepTime[k,'xPlot']==Label[i]){
        sleepTime[k,'xPlot'] <- v
      }
    }
    return(sleepTime)
  }
  
  if (input$SleepTimePlotsTabs == "Sleep bout duration per light phase"){
    BoutSleepTimeData$lightDark <- changeLabels(BoutSleepTimeData$lightDark)
    updateBoutSleepTimeLightDark()
  }
  else{
    if (input$SleepTimePlotsTabs == "Sleep bout duration per day"){
      BoutSleepTimeData$day <- changeLabels(BoutSleepTimeData$day)
      updateBoutSleepTimeDay()
    }
    else{
      if(input$SleepTimePlotsTabs == "Sleep bout duration per day and light phase"){
        BoutSleepTimeData$dayLightDark <- changeLabels(BoutSleepTimeData$dayLightDark)
        updateBoutSleepTimeDayLightDark()
      }
      else{
        BoutSleepTimeData$custom <- changeLabels(BoutSleepTimeData$custom)
        updateBoutSleepTimeCustom()
      }
    }
  }
  
})

#Sleep latency
observeEvent(input$SleepLatencySummary_cell_edit,{
  
  info <- input$SleepLatencySummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  changeLabels <- function(sleepLatency){
    
    conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=sleepLatency, FUN=mean)[,1]
    Label <-  str_split_fixed(conditions, " - ",2)[,2]
    
    for (k in 1:nrow(sleepLatency)){
      if (sleepLatency[k,'xPlot']==Label[i]){
        sleepLatency[k,'xPlot'] <- v
      }
    }
    return(sleepLatency)
  }
  
  if (input$SleepLatencyPlotsTabs == "Sleep latency"){
    BoutSleepLatencyData$lightDark <- changeLabels(BoutSleepLatencyData$lightDark)
    updateBoutSleepLatencyAll()
  }
  else{
    if (input$SleepLatencyPlotsTabs == "Sleep latency per day"){
      BoutSleepLatencyData$day <- changeLabels(BoutSleepLatencyData$day)
      updateBoutSleepLatencyDay()
    }
    else{
      BoutSleepLatencyData$custom <- changeLabels(BoutSleepLatencyData$custom)
      updateBoutSleepLatencyCustom()
    }
  }
  
})

#################### Update statistical data presented ###################

#Update data presented
#Activity Bouts table
observe({
  req(nrow(damData$dt)>0)
  req(nrow(BoutActivityData$lightDark)>0)
  
  if (input$boutActivityPlotsTabs == "Activity per bout per light phase"){
    boutActivity <- BoutActivityData$lightDark
  }
  else{
    if (input$boutActivityPlotsTabs == "Activity per bout per day"){
      boutActivity <- BoutActivityData$day
    }
    else{
      if(input$boutActivityPlotsTabs == "Activity per bout per day and light phase"){
        boutActivity <- BoutActivityData$dayLightDark
      }
      else{
        boutActivity <- BoutActivityData$custom
      }
    }
  }
  
  updateSliderInput(session,'yLimitsBoutActivity',min = 0, max =ceiling(max(boutActivity[,'yPlot'], na.rm =TRUE)), value = c(0,max(boutActivity[,'yPlot'])))
  
  
  output$BoutActivitySummary <- DT::renderDataTable(statisticsReport(boutActivity),
                                                escape = FALSE, selection = 'none', 
                                                editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
})
#Activity Bouts time table
observe({
  req(nrow(damData$dt)>0)
  req(nrow(BoutTimeData$lightDark)>0)
  
  if (input$boutTimePlotsTabs == "Bout duration per light phase"){
    boutTime <- BoutTimeData$lightDark
  }
  else{
    if (input$boutTimePlotsTabs == "Bout duration per day"){
      boutTime <- BoutTimeData$day
    }
    else{
      if(input$boutTimePlotsTabs == "Bout duration per day and light phase"){
        boutTime <- BoutTimeData$dayLightDark
      }
      else{
        boutTime <- BoutTimeData$custom
      }
    }
  }
  
  updateSliderInput(session,'yLimitsBoutTime',min = 0, max =ceiling(max(boutTime[,'yPlot'], na.rm =TRUE)), value = c(0,max(boutTime[,'yPlot'])))
  
  output$BoutTimeSummary <- DT::renderDataTable(statisticsReport(boutTime),
                                             escape = FALSE, selection = 'none', 
                                             editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
})
#Sleep bouts time table
observe({
  req(nrow(damData$dt)>0)
  req(nrow(BoutSleepTimeData$lightDark)>0)
  
  if (input$SleepTimePlotsTabs == "Sleep bout duration per light phase"){
    sleepBoutTime <- BoutSleepTimeData$lightDark
  }
  else{
    if (input$SleepTimePlotsTabs == "Sleep bout duration per day"){
      sleepBoutTime <- BoutSleepTimeData$day
    }
    else{
      if(input$SleepTimePlotsTabs == "Sleep bout duration per day and light phase"){
        sleepBoutTime <- BoutSleepTimeData$dayLightDark
      }
      else{
        sleepBoutTime <- BoutSleepTimeData$custom
      }
    }
  }
  
  updateSliderInput(session,'yLimitsBoutSleepTime',min = 0, max =ceiling(max(sleepBoutTime[,'yPlot'], na.rm =TRUE)), value = c(0,max(sleepBoutTime[,'yPlot'])))
  
  output$SleepTimeSummary <- DT::renderDataTable(statisticsReport(sleepBoutTime),
                                                 escape = FALSE, selection = 'none', 
                                                 editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
})
#Sleep Latency table
observe({
  req(nrow(damData$dt)>0)
  req(nrow(BoutSleepLatencyData$all)>0)
  
  if (input$SleepLatencyPlotsTabs == "Sleep latency"){
    sleepLatency <- BoutSleepLatencyData$all
  }
  else{
    if (input$SleepLatencyPlotsTabs == "Sleep latency per day"){
      sleepLatency <- BoutSleepLatencyData$day
    }
    else{
      sleepLatency <- BoutSleepLatencyData$custom
    }
  }
  
  updateSliderInput(session,'yLimitsSleepLatency',min = 0, max =ceiling(max(sleepLatency[,'yPlot'], na.rm =TRUE)), value = c(0,max(sleepLatency[,'yPlot'])))
  
  output$SleepLatencySummary <- DT::renderDataTable(statisticsReport(sleepLatency),
                                                      escape = FALSE, selection = 'none', 
                                                      editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
})


############################### PLOTS ####################################

#Update graphs
observeEvent(input$updateBoutsStatistics,{

  req(nrow(damData$dt)>0)
  req(nrow(BoutActivityData$lightDark)>0)

  updateActivityBoutsLabels()
  updateSleepBoutsLabels()
  
  damData$dt[, 'boutBoxPlot_time' := floor(damData$dt[,'t']/(input$boutGroupBoxTime * l_period()*3600))]

  settings <- settingsTable()
  settings[1,2] <- l_period()
  settings[2,2] <- l_hours()
  settingsTable(settings)
  
  updateFigures()
  
  updateActivityFigures()
  updateSleepFigures()
  
  
  withProgress(message = 'Update data and graphs', value = 0, {
    
    ActivityBoutsData()
    incProgress(0.25)
    SleepBoutsData()
    incProgress(0.25)
  
    updateBoutActivityFigures()
    updateBoutTimeFigures()
    updateBoutSleepTimeFigures()
    updateBoutSleepLatencyFigures()
    incProgress(0.25)
  
    updateYBoutActivity()
    updateYBoutTime()
    updateYSleepTime()
    updateYSleepLatency()
    incProgress(0.25)
  })
  
  #Periodic representations
  updateFigures()
  
  #Activity and Sleep statistics
  updateActivityFigures()
  updateSleepFigures()
  updateYactivity()

})

#Update Text fields
observeEvent(input$boutActivityPlotsTabs,{
  
  #Update activity text fields
  updateTextInput(session, "BoutActivityBoxXLabel", value = BoutActivityFiguresXLabel())
  updateTextInput(session, "BoutActivityBoxYLabel", value = BoutActivityFiguresYLabel())
  
  if (input$boutActivityPlotsTabs == "Activity per bout per light phase"){
    updateTextInput(session,"BoutActivityBoxTitle", value = BoutActivityFiguresTitles$lightDark)
  }
  else{
    if (input$boutActivityPlotsTabs == "Activity per bout per day"){
      updateTextInput(session,"BoutActivityBoxTitle", value = BoutActivityFiguresTitles$day)
    }
    else{
      if(input$boutActivityPlotsTabs == "Activity per bout per day and light phase"){
        updateTextInput(session,"BoutActivityBoxTitle", value = BoutActivityFiguresTitles$dayLightDark)
      }
      else{
        updateTextInput(session,"BoutActivityBoxTitle", value = BoutActivityFiguresTitles$custom)
      }
    }
  }
  
  
})
observeEvent(input$boutTimePlotsTabs,{
  
  #Update activity text fields
  updateTextInput(session, "BoutTimeBoxXLabel", value = BoutTimeFiguresXLabel())
  updateTextInput(session, "BoutTimeBoxYLabel", value = BoutTimeFiguresYLabel())
  
  if (input$boutTimePlotsTabs == "Bout duration per light phase"){
    updateTextInput(session,"BoutTimeBoxTitle", value = BoutTimeFiguresTitles$lightDark)
  }
  else{
    if (input$boutTimePlotsTabs == "Bout duration per day"){
      updateTextInput(session,"BoutTimeBoxTitle", value = BoutTimeFiguresTitles$day)
    }
    else{
      if(input$boutTimePlotsTabs == "Bout duration per day and light phase"){
        updateTextInput(session,"BoutTimeBoxTitle", value = BoutTimeFiguresTitles$dayLightDark)
      }
      else{
        updateTextInput(session,"BoutTimeBoxTitle", value = BoutTimeFiguresTitles$custom)
      }
    }
  }
  
  
})
observeEvent(input$SleepTimePlotsTabs,{
  
  #Update activity text fields
  updateTextInput(session, "SleepTimeBoxXLabel", value = BoutSleepTimeFiguresXLabel())
  updateTextInput(session, "SleepTimeBoxYLabel", value = BoutSleepTimeFiguresYLabel())
  
  if (input$SleepTimePlotsTabs == "Sleep bout duration per light phase"){
    updateTextInput(session,"SleepTimeBoxTitle", value = BoutSleepTimeFiguresTitles$lightDark)
  }
  else{
    if (input$SleepTimePlotsTabs == "Sleep bout duration per day"){
      updateTextInput(session,"SleepTimeBoxTitle", value = BoutSleepTimeFiguresTitles$day)
    }
    else{
      if(input$SleepTimePlotsTabs == "Sleep bout duration per day and light phase"){
        updateTextInput(session,"SleepTimeBoxTitle", value = BoutSleepTimeFiguresTitles$dayLightDark)
      }
      else{
        updateTextInput(session,"SleepTimeBoxTitle", value = BoutSleepTimeFiguresTitles$custom)
      }
    }
  }
  
  
})
observeEvent(input$SleepLatencyPlotsTabs,{
  
  #Update activity text fields
  updateTextInput(session, "SleepLatencyBoxXLabel", value = BoutSleepLatencyFiguresXLabel())
  updateTextInput(session, "SleepLatencyBoxYLabel", value = BoutSleepLatencyFiguresYLabel())
  
  if (input$SleepLatencyPlotsTabs == "Sleep latency"){
    updateTextInput(session,"SleepLatencyBoxTitle", value = BoutSleepLatencyFiguresTitles$all)
  }
  else{
    if (input$SleepLatencyPlotsTabs == "Sleep latency per day"){
      updateTextInput(session,"SleepLatencyBoxTitle", value = BoutSleepLatencyFiguresTitles$day)
    }
    else{
      updateTextInput(session,"SleepLatencyBoxTitle", value = BoutSleepLatencyFiguresTitles$custom)
    }
  }
  
  
})

#Bout activity box plots
output$boutActivityDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData$lightDark)>0, "")
  )
  
  fig <- BoutActivityFigures$lightDark
  
  return(fig)
  
  
  
},width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$boutActivityPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData$lightDark)>0, "")
  )

  fig <- BoutActivityFigures$day
  
  
  return(fig)
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$boutActivityDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData$lightDark)>0, "")
  )

  fig <- BoutActivityFigures$dayLightDark
  
  
  return(fig)
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$boutActivityCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData$lightDark)>0, "")
  )
  
  req(input$boutGroupBoxTime)
  req(input$boutBoxTime)
  
  fig <- BoutActivityFigures$custom
  
  return(fig)
  
},width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)


#Bout time box plots
output$boutTimeDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData$lightDark)>0, "")
  )
  
  fig <- BoutTimeFigures$lightDark
  
  
  return(fig)
  
  
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$boutTimePerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData$lightDark)>0, "")
  )
  
  fig <- BoutTimeFigures$day
  
  
  return(fig)
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$boutTimeDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData$lightDark)>0, "")
  )
  
  fig <- BoutTimeFigures$dayLightDark
  
  return(fig)
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$boutTimeCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData$lightDark)>0, "")
  )
  req(input$boutGroupBoxTime)
  req(input$boutBoxTime)
  
  fig <- BoutTimeFigures$custom
  
  return(fig)
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)


#Sleep time box plots
output$SleepTimeDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData$lightDark)>0, "")
  )
  
  fig <- BoutSleepTimeFigures$lightDark
  
  return(fig)
  
  
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$SleepTimePerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData$lightDark)>0, "")
  )
  
  fig <- BoutSleepTimeFigures$day
  
  return(fig)
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$SleepTimeDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData$lightDark)>0, "")
  )
  
  fig <- BoutSleepTimeFigures$dayLightDark
  
  
  return(fig)
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$SleepTimeCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData$lightDark)>0, "")
  )
  req(input$boutGroupBoxTime)
  req(input$boutBoxTime)
  
  fig <- BoutSleepTimeFigures$custom
  
  return(fig)
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)


#Sleep latency box plots
output$SleepLatencyDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepLatencyData$all)>0, "")
  )

  fig <- BoutSleepLatencyFigures$all
  
  
  return(fig)
  
  
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$SleepLatencyPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepLatencyData$all)>0, "")
  )
  
  fig <- BoutSleepLatencyFigures$day
  
  
  return(fig)
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)
output$SleepLatencyCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepLatencyData$all)>0, "")
  )
  req(input$boutGroupBoxTime)
  req(input$boutBoxTime)
  
  fig <- BoutSleepLatencyFigures$custom
  
  
  return(fig)
  
}, width = function() input$boutWidth,
height = function() input$boutHeight,
res = 96)

######################## SAVE FIGS AND DATA #############################


# #save Figures
observe({
  req(BoutActivityFigures$lightDark)

  output$saveBoutActivityFig <- downloadHandler(

    filename = function(){
      paste0(input$boutActivityPlotsTabs,input$BoutActivityFig)
    },
    content = function(file){
      
      if (input$boutActivityPlotsTabs == "Activity per bout per light phase"){
        fig <- BoutActivityFigures$lightDark
      }
      else{
        if (input$boutActivityPlotsTabs == "Activity per bout per day"){
          fig <- BoutActivityFigures$day
        }
        else{
          if(input$boutActivityPlotsTabs == "Activity per bout per day and light phase"){
            fig <- BoutActivityFigures$dayLightDark
          }
          else{
            fig <- BoutActivityFigures$custom
          }
        }
      }

      ggsave(filename = file, plot = fig,
         width = round(input$boutWidth/97), height= round(input$boutHeight/97))
    })
})
observe({
  req(BoutTimeFigures$lightDark)

  output$saveBoutTimeFig <- downloadHandler(

    filename = function(){
      paste0(input$boutTimePlotsTabs,input$BoutTimeFig)
    },
    content = function(file){
      
      if (input$boutTimePlotsTabs == "Bout duration per light phase"){
        fig <- BoutTimeFigures$lightDark
      }
      else{
        if (input$boutTimePlotsTabs == "Bout duration per day"){
          fig <- BoutTimeFigures$day
        }
        else{
          if(input$boutTimePlotsTabs == "Bout duration per day and light phase"){
            fig <- BoutTimeFigures$dayLightDark
          }
          else{
            fig <- BoutTimeFigures$custom
          }
        }
      }

  ggsave(filename = file, plot = fig,
         width = round(input$boutWidth/97), height= round(input$boutHeight/97))
    })
})
observe({
  req(BoutSleepTimeFigures$lightDark)

  output$saveSleepLatencyFig <- downloadHandler(

    filename = function(){
      paste0(input$SleepTimePlotsTabs,input$SleepLatencyFig)
    },
    content = function(file){
      
      if (input$SleepTimePlotsTabs == "Sleep bout duration per light phase"){
        fig <- BoutSleepTimeFigures$lightDark
      }
      else{
        if (input$SleepTimePlotsTabs == "Sleep bout duration per day"){
          fig <- BoutSleepTimeFigures$day
        }
        else{
          if(input$SleepTimePlotsTabs == "Sleep bout duration per day and light phase"){
            fig <- BoutSleepTimeFigures$dayLightDark
          }
          else{
            fig <- BoutSleepTimeFigures$custom
          }
        }
      }

      ggsave(filename = file, plot = fig,
             width = round(input$boutWidth/97), height= round(input$boutHeight/97))
    })
})
observe({
  req(BoutSleepLatencyFigures$all)

  output$saveSleepTimeFig <- downloadHandler(

    filename = function(){
      paste0(input$SleepLatencyPlotsTabs,input$SleepTimeFig)
    },
    content = function(file){
      
      if (input$SleepLatencyPlotsTabs == "Sleep latency"){
        fig <- BoutSleepLatencyFigures$all
      }
      else{
        if (input$SleepLatencyPlotsTabs == "Sleep latency per day"){
          fig <- BoutSleepLatencyFigures$day
        }
        else{
          fig <- BoutSleepLatencyFigures$custom
        }
      }

      ggsave(filename = file, plot = fig,
             width = round(input$boutWidth/97), height= round(input$boutHeight/97))
    })
})
# 
# 
#Save graph data
observe({
  req(damData$dt)
  req(BoutActivityData$lightDark)

  output$saveBoutActivityReport <- downloadHandler(

    filename = function(){
      paste0(input$boutActivityPlotsTabs,".xlsx")
    },
    content = function(file){

      wb <- createWorkbook(type="xlsx")
      
      if (input$boutActivityPlotsTabs == "Activity per bout per light phase"){
        data <- BoutActivityData$lightDark
      }
      else{
        if (input$boutActivityPlotsTabs == "Activity per bout per day"){
          data <- BoutActivityData$day
        }
        else{
          if(input$boutActivityPlotsTabs == "Activity per bout per day and light phase"){
            data <- BoutActivityData$dayLightDark
          }
          else{
            data <- BoutActivityData$custom
          }
        }
      }
      
      settings <- rbind(settingsTable()[1:2,],settingsTable()[4,])
      sheet <- createSheet(wb, "Settings")
      addDataFrame(settings, sheet=sheet, startColumn=1, row.names=FALSE)
      
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(statisticsReport(data), sheet=sheet, startColumn=1, row.names=FALSE)
      
      animalsData <-  dataReport(data)
      
      sheet <- createSheet(wb,"All data")
      addDataFrame(animalsData, sheet=sheet, startColumn=1, row.names=FALSE)
      
      
      #Create organized data sheet
      Labels <- animalsData[,'Labels']
      
      for (k in 6:ncol(animalsData)){
        
        dataColumn <- animalsData[,k]
        DF <- data.frame(Labels,dataColumn)
        joinedData <- DF %>%group_by(Labels) %>% group_nest()
        
        Data <- data.frame()
        
        for (i in 1:nrow(joinedData)){
          Data <- cbind.fill(Data, (unlist(joinedData[i,'data'])))
        }
        colnames(Data)<- unlist(joinedData[,'Labels'])
        
        sheet <- createSheet(wb, colnames(animalsData)[k])
        addDataFrame(Data, sheet=sheet, startColumn=1, row.names=FALSE)
      }


      saveWorkbook(wb, file = file)
    })
})
observe({
  req(BoutTimeData$lightDark)

  output$saveBoutTimeReport <- downloadHandler(

    filename = function(){
      paste0(input$boutTimePlotsTabs,".xlsx")
    },
    content = function(file){

      wb <- createWorkbook(type="xlsx")
      
      if (input$boutTimePlotsTabs == "Bout duration per light phase"){
        data <- BoutTimeData$lightDark
      }
      else{
        if (input$boutTimePlotsTabs == "Bout duration per day"){
          data <- BoutTimeData$day
        }
        else{
          if(input$boutTimePlotsTabs == "Bout duration per day and light phase"){
            data <- BoutTimeData$dayLightDark
          }
          else{
            data <- BoutTimeData$custom
          }
        }
      }
      
      settings <- rbind(settingsTable()[1:2,],settingsTable()[4,])
      sheet <- createSheet(wb, "Settings")
      addDataFrame(settings, sheet=sheet, startColumn=1, row.names=FALSE)
      
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(statisticsReport(data), sheet=sheet, startColumn=1, row.names=FALSE)
      
      animalsData <-  dataReport(data)
      
      sheet <- createSheet(wb,"All data")
      addDataFrame(animalsData, sheet=sheet, startColumn=1, row.names=FALSE)
      
      
      #Create organized data sheet
      Labels <- animalsData[,'Labels']
      
      for (k in 6:ncol(animalsData)){
        
        dataColumn <- animalsData[,k]
        DF <- data.frame(Labels,dataColumn)
        joinedData <- DF %>%group_by(Labels) %>% group_nest()
        
        Data <- data.frame()
        
        for (i in 1:nrow(joinedData)){
          Data <- cbind.fill(Data, (unlist(joinedData[i,'data'])))
        }
        colnames(Data)<- unlist(joinedData[,'Labels'])
        
        sheet <- createSheet(wb, colnames(animalsData)[k])
        addDataFrame(Data, sheet=sheet, startColumn=1, row.names=FALSE)
      }


      saveWorkbook(wb, file = file)
    })
})
observe({
  req(BoutSleepTimeData$lightDark)

  output$saveSleepTimeReport <- downloadHandler(

    filename = function(){
      paste0(input$SleepTimePlotsTabs,".xlsx")
    },
    content = function(file){
      
      wb <- createWorkbook(type="xlsx")
      
      if (input$SleepTimePlotsTabs == "Sleep bout duration per light phase"){
        data <- BoutSleepTimeData$lightDark
      }
      else{
        if (input$SleepTimePlotsTabs == "Sleep bout duration per day"){
          data <- BoutSleepTimeData$day
        }
        else{
          if(input$SleepTimePlotsTabs == "Sleep bout duration per day and light phase"){
            data <- BoutSleepTimeData$dayLightDark
          }
          else{
            data <- BoutSleepTimeData$custom
          }
        }
      }

      settings <- rbind(settingsTable()[1:2,],settingsTable()[5,])
      sheet <- createSheet(wb, "Settings")
      addDataFrame(settings, sheet=sheet, startColumn=1, row.names=FALSE)
      
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(statisticsReport(data), sheet=sheet, startColumn=1, row.names=FALSE)
      
      animalsData <-  dataReport(data)
      
      sheet <- createSheet(wb,"All data")
      addDataFrame(animalsData, sheet=sheet, startColumn=1, row.names=FALSE)
      
      
      #Create organized data sheet
      Labels <- animalsData[,'Labels']
      
      for (k in 6:ncol(animalsData)){
        
        dataColumn <- animalsData[,k]
        DF <- data.frame(Labels,dataColumn)
        joinedData <- DF %>%group_by(Labels) %>% group_nest()
        
        Data <- data.frame()
        
        for (i in 1:nrow(joinedData)){
          Data <- cbind.fill(Data, (unlist(joinedData[i,'data'])))
        }
        colnames(Data)<- unlist(joinedData[,'Labels'])
        
        sheet <- createSheet(wb, colnames(animalsData)[k])
        addDataFrame(Data, sheet=sheet, startColumn=1, row.names=FALSE)
      }


      saveWorkbook(wb, file = file)
    })
})
observe({
  req(BoutSleepLatencyData$all)

  output$saveSleepLatencyReport <- downloadHandler(

    filename = function(){
      paste0(input$SleepLatencyPlotsTabs,".xlsx")
    },
    content = function(file){
      
      wb <- createWorkbook(type="xlsx")
      
      if (input$SleepLatencyPlotsTabs == "Sleep latency"){
        data <- BoutSleepLatencyData$all
      }
      else{
        if (input$SleepLatencyPlotsTabs == "Sleep latency per day"){
          data <- BoutSleepLatencyData$day
        }
        else{
          data <- BoutSleepLatencyData$custom
        }
      }
      
      settings <- rbind(settingsTable()[1:2,],settingsTable()[5,])
      sheet <- createSheet(wb, "Settings")
      addDataFrame(settings, sheet=sheet, startColumn=1, row.names=FALSE)
      
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(statisticsReport(data), sheet=sheet, startColumn=1, row.names=FALSE)
      
      animalsData <-  dataReport(data)
      
      sheet <- createSheet(wb,"All data")
      addDataFrame(animalsData, sheet=sheet, startColumn=1, row.names=FALSE)
      
      
      #Create organized data sheet
      Labels <- animalsData[,'Labels']
      
      for (k in 6:ncol(animalsData)){
        
        dataColumn <- animalsData[,k]
        DF <- data.frame(Labels,dataColumn)
        joinedData <- DF %>%group_by(Labels) %>% group_nest()
        
        Data <- data.frame()
        
        for (i in 1:nrow(joinedData)){
          Data <- cbind.fill(Data, (unlist(joinedData[i,'data'])))
        }
        colnames(Data)<- unlist(joinedData[,'Labels'])
        
        sheet <- createSheet(wb, colnames(animalsData)[k])
        addDataFrame(Data, sheet=sheet, startColumn=1, row.names=FALSE)
      }

      saveWorkbook(wb, file = file)
    })
})
