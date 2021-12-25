############################# VARIABLES #################################

### Sleep data and figure variables
SleepData <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
SleepFigures <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
SleepFiguresTitles <- reactiveValues(lightDark = "Sleep ratio per light phase", day = "Sleep ratio per day", 
                                     dayLightDark = "Sleep ratio per day and per light phase", custom = "Sleep ratio")
SleepFiguresXLabel <- reactiveVal("")
SleepFiguresYLabel <- reactiveVal("Sleep ratio")

### TST and WASO and figure variables
TST_WASOData <- reactiveValues(TST_all = NULL, TST_day = NULL, WASO_all = NULL, WASO_day = NULL)
TST_WASOFigures <- reactiveValues(TST_all = NULL, TST_day = NULL, WASO_all = NULL, WASO_day = NULL)
TST_WASOTitles <- reactiveValues(TST_all = "Total sleep time", TST_day = "Total sleep time per day", 
                                     WASO_all = "Wake after sleep onset", WASO_day = "Wake after sleep onset per day")
TST_WASOFiguresXLabel <- reactiveVal("")
TST_WASOFiguresYLabel <- reactiveValues(TST = "TST (h)", WASO = "WASO (h)")


### Time per sleep bout data and figure variables
BoutSleepTimeData <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
BoutSleepTimeFigures <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
BoutSleepTimeFiguresTitles <- reactiveValues(lightDark = "Sleep bout duration per light phase", day = "Sleep bout duration per day", 
                                             dayLightDark = "Sleep bout duration per day and per light phase", custom = "Sleep bout duration")
BoutSleepTimeFiguresXLabel <- reactiveVal("")
BoutSleepTimeFiguresYLabel <- reactiveVal("Mean sleep bout duration (min)")

### Sleep Latency data and figure variables
BoutSleepLatencyData <- reactiveValues(all = NULL, day = NULL, custom = NULL)
BoutSleepLatencyFigures <- reactiveValues(all = NULL, day = NULL, custom = NULL)
BoutSleepLatencyFiguresTitles <- reactiveValues(all = "Sleep latency", day = "Sleep latency per day", 
                                                custom = "Mean sleep latency (min)")
BoutSleepLatencyFiguresXLabel <- reactiveVal("")
BoutSleepLatencyFiguresYLabel <- reactiveVal("Mean sleep latency")
########################### FUNCTIONS ################################

sleepBoutAnalysis <- function(){
  
  req(damData$dt)
  
  
  withProgress(message = 'Sleep Bout analysis', value = 0, {
    
    #Calculus of the time difference between measurements
    damData$dt[,timeDiff := c(NaN,damData$dt[2:nrow(damData$dt),t]- damData$dt[1:(nrow(damData$dt)-1),t])]
    
    meanTimeDiff <- mean(damData$dt[damData$dt[,timeDiff]>0,timeDiff],na.rm=TRUE) #Minimum bin size
    step <- round(mins(input$ActivityBoutWindow)/meanTimeDiff) #Step for analysis
    
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
    dataActiveSleepBout <- rep(FALSE,nrow(damData$dt))
    dataActiveSleepBoutTime <- rep(0,nrow(damData$dt))
    dataSleepLatency <- rep(0, nrow(damData$dt))
    
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
      sleepBout_dt <- bout_analysis(asleep, damData$dt[startIndexes[j]:finalIndexes[j],])
      t <- damData$dt[startIndexes[j]:finalIndexes[j],t]
      
      sleepBouts <- sleepBout_dt[asleep == TRUE, -"asleep"]
      activeSleepBouts <- sleepBout_dt[asleep == FALSE, -"asleep"]
      
      #Remove last sleep bout of dark phases
      indexes <- which(floor(sleepBouts[,t]/hours(l_period())) != floor((sleepBouts[,t]+sleepBouts[,duration])/hours(l_period())))
      sleepBouts[indexes,'duration']<- (ceiling(sleepBouts[indexes,t]/hours(l_period()))-sleepBouts[indexes,t]/hours(l_period()))*hours(l_period())
      
      
      ### Get start and finish indexes of bouts
      sleepT <- sleepBouts[,t]
      sleepDuration <- sleepBouts[,duration]
      
      startRowInactivity <- which(t %in% sleepT)
      finishRowInactivity <- which(t %in% (sleepT+sleepDuration))
      
      latency <- TRUE
      
      #Calculate sleep bouts duration
      if(length(startRowInactivity)>0){
        for (k in 1:length(startRowInactivity)){
          
          dataSleepBout[startIndexes[j]+startRowInactivity[k]-1] <- TRUE
          dataSleepBoutTime[startIndexes[j]+startRowInactivity[k]-1]<- sleepDuration[k]
        }
      }
      
      
      #Remove last sleep bout of dark phases
      indexes <- which(floor(activeSleepBouts[,t]/hours(l_period())) != floor((activeSleepBouts[,t]+activeSleepBouts[,duration])/hours(l_period())))
      activeSleepBouts[indexes,'duration']<- (ceiling(activeSleepBouts[indexes,t]/hours(l_period()))-activeSleepBouts[indexes,t]/hours(l_period()))*hours(l_period())
      
      
      ### Get start and finish indexes of active sleep bouts
      activeT <- activeSleepBouts[,t]
      activeDuration <- activeSleepBouts[,duration]
      
      startRowActivity <- which(t %in% activeT)
      finishRowActivity <- which(t %in% (activeT+activeDuration))
      
      #Calculate sleep bouts duration
      if(length(startRowActivity)>0){
        for (k in 1:length(startRowActivity)){
          
          dataActiveSleepBout[startIndexes[j]+startRowActivity[k]-1] <- TRUE
          dataActiveSleepBoutTime[startIndexes[j]+startRowActivity[k]-1]<- activeDuration[k]
          }
        }
      
      #Sleep Latency
      periodicTime <- damData$dt[startIndexes[j]:finalIndexes[j],t]
      replica_data <- damData$dt[startIndexes[j]:finalIndexes[j],]
      boutReplica <- dataSleepBout[startIndexes[j]:finalIndexes[j]]
      
      threshold_hours <- hours(l_hours())
      
      minimum <- min(damData$dt[,experimentDay])
      maximum <- max(damData$dt[,experimentDay])
      
      
      for (k in minimum:maximum){
        
        Time <- periodicTime - hours(l_period())*k - threshold_hours
        n <- which(Time>0)[1]

        indexes <- which(damData$dt[startIndexes[j]:finalIndexes[j],experimentDay] == k &
                           damData$dt[startIndexes[j]:finalIndexes[j],day_night] == "FALSE" & 
                           dataSleepBout[startIndexes[j]:finalIndexes[j]] == TRUE)
        
        if(length(indexes)>0){
          dataSleepLatency[startIndexes[j]+indexes[1]-2] <- Time[indexes[1]]
        }
      }
    }
      
    
  })

  damData$dt[,sleepBout := dataSleepBout]
  damData$dt[,sleepBoutTime := dataSleepBoutTime]
  damData$dt[,sleepLatency:=dataSleepLatency]

  damData$dt[,activeSleepBout:=dataActiveSleepBout]
  damData$dt[,activeSleepBoutTime:=dataActiveSleepBoutTime]
  
  settings <- settingsTable()
  settings[1,2] <- l_period()
  settings[2,2] <- l_hours()
  settingsTable(settings)
  
  ### Update figures
  
  SleepBoutsDataSummary()
  
  updateTabsetPanel(session, "SleepTimePlotsTabs", selected = "Sleep bout duration per light phase")
  updateTabsetPanel(session, "SleepLatencyPlotsTabs", selected = "Sleep latency")
  
  #### Update Y range sliders
  sleepTime <- BoutSleepTimeData$lightDark
  updateSliderInput(session,'yLimitsBoutSleepTime',min = 0, max =ceiling(max(sleepTime[,'yPlot'], na.rm =TRUE)), value = c(0,max(sleepTime[,'yPlot'], na.rm =TRUE)))
  
  sleepLatency <- BoutSleepLatencyData$all
  updateSliderInput(session,'yLimitsSleepLatency',min = 0, max =ceiling(max(sleepLatency[,'yPlot'], na.rm =TRUE)), value = c(0,max(sleepLatency[,'yPlot'], na.rm =TRUE)))
  
  #Create figures
  updateBoutSleepTimeFigures()
  updateBoutSleepLatencyFigures()
} # Calculate sleep bouts

#Functions to calculate data to plot
sleepSummary <- function(graph){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ### Create data for statistic plots
  if(graph == "lightDark"){
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(Sleep_Light = mean(asleep[day_night == "TRUE"],na.rm=TRUE)), by = id])
    summaryNight <- rejoin(damData$dt[, .(Sleep_Dark = mean(asleep[day_night == "FALSE"],na.rm = TRUE)), by = id])
    
    summaryDT <- cbind(summaryLight, summaryNight[,Sleep_Dark])
    names(summaryDT)[length(names(summaryDT))] <- paste0("Sleep_Dark")
    
    #Get summary by light and dark phase  
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Sleep_"),variable.name = "xPlot", value.name = "yPlot")
    
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "Sleep_Dark", "Dark")]
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "Sleep_Light", "Light")]
  }
  
  if(graph == "day"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(Sleep = mean(asleep[experimentDay==startDay],na.rm=TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryDT)[names(summaryDT) == "Sleep"] <- paste0("Sleep_Day_",startDay)
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(Sleep = mean(asleep[experimentDay==n], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,Sleep])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Sleep_Day_",n)
      }
    }
    
    #Rename files as Day0, Day1...
    names(summaryDT) <- str_replace_all(names(summaryDT), "Sleep_", "")
    
    #Get summary by days
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  if(graph == "dayLightDark"){
    
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(Sleep_Day = mean(asleep[experimentDay==startDay & day_night == "TRUE"],na.rm=TRUE)), by = id])
    summaryNight <- rejoin(damData$dt[, .(Sleep_Night = mean(asleep[experimentDay==startDay & day_night == "FALSE"],na.rm = TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryLight)[names(summaryLight) == "Sleep_Day"] <- paste0("Sleep_Day_",startDay,"_Light")
    
    summaryDT <- cbind(summaryLight, summaryNight[,Sleep_Night])
    names(summaryDT)[length(names(summaryDT))] <- paste0("Sleep_Day_",startDay,"_Dark")
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(Sleep_Day = mean(asleep[experimentDay==n & day_night == "TRUE"], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,Sleep_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Sleep_Day_",n,"_Light")
        
        summary <- rejoin(damData$dt[, .(Sleep_Night = mean(asleep[experimentDay==n & day_night == "FALSE"], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,Sleep_Night])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Sleep_Day_",n,"_Dark")
      }
    }
    
    #Rename by day and light phase
    names(summaryDT) <- str_replace_all(names(summaryDT), "Sleep_", "")
    
    #Get summary by light and dark phase and by day
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  if(graph == "custom"){
    
    #Separate by customized activity
    startTime <- min(damData$dt[,SleepBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(sleep_Day = mean(asleep[SleepBoxPlot_time==startTime],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "sleep_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,SleepBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(sleep_Day = mean(asleep[SleepBoxPlot_time==n],na.rm=TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,sleep_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Group",n)
      }
    }
    
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Group"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
    
  }
  
  return(summaryDT_melted)
}
TST_WASOSummary <- function(parameter, graph){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ##### Create data for statistic plots #####
  
  if(parameter == "TST" & graph == "all"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(TST = sum(sleepBoutTime[day_night == "FALSE"],na.rm=TRUE)), by = id])
    
    #Get summary by light and dark phase  
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("TST"),variable.name = "xPlot", value.name = "yPlot")
    
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "TST", "TotalSleepTime")]
  }
  
  if(parameter == "TST" & graph == "day"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(TST = sum(sleepBoutTime[experimentDay==startDay & day_night == "FALSE"],na.rm=TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryDT)[names(summaryDT) == "TST"] <- paste0("TotalSleepTime_Day_",startDay)
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(TST = sum(sleepBoutTime[experimentDay==n & day_night == "FALSE"],na.rm=TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,TST])
        
        names(summaryDT)[length(names(summaryDT))] <- paste0("TotalSleepTime_Day_",n)
      }
    }
    
    
    #Rename files as Day0, Day1...
    names(summaryDT) <- str_replace_all(names(summaryDT), "TotalSleepTime_", "")
    
    #Get summary by days
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
  }
  
  if(parameter == "WASO" & graph == "all"){
    
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(WASO = sum(activeSleepBoutTime[day_night == "FALSE"],na.rm=TRUE)), by = id])
    
    #Get summary by light and dark phase  
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("WASO"),variable.name = "xPlot", value.name = "yPlot")
    
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "WASO", "WakeAfterSleepOnset")]
  }
  
  if(parameter == "WASO" & graph == "day"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(WASO = sum(activeSleepBoutTime[experimentDay==startDay & day_night == "FALSE"],na.rm=TRUE)), by = id])

    #Change names to use in the graphs
    names(summaryDT)[names(summaryDT) == "WASO"] <- paste0("WakeAfterSleepOnset_Day_",startDay)
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(WASO = sum(activeSleepBoutTime[experimentDay==n & day_night == "FALSE"],na.rm=TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,WASO])
        
        names(summaryDT)[length(names(summaryDT))] <- paste0("WakeAfterSleepOnset_Day_",n)
      }
    }
    
    
    #Rename files as Day0, Day1...
    names(summaryDT) <- str_replace_all(names(summaryDT), "WakeAfterSleepOnset_", "")
    
    #Get summary by days
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Day"),
                             variable.name = "xPlot", 
                             value.name = "yPlot")
    
  }
  
  summaryDT_melted[,'yPlot':= summaryDT_melted[,'yPlot']/3600]
  
  
  return(summaryDT_melted)
}
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
    startTime <- min(damData$dt[,ActivityBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(SleepLatency_Day = mean(sleepLatency[ActivityBoxPlot_time==startTime & sleepLatency > 0],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "SleepLatency_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,ActivityBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(SleepLatency_Day = mean(sleepLatency[ActivityBoxPlot_time==n & sleepLatency > 0],na.rm=TRUE)), by = id])
        
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
    startTime <- min(damData$dt[,ActivityBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(activity_Day = mean(sleepBoutTime[ActivityBoxPlot_time==startTime & sleepBoutTime>0],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "activity_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,ActivityBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(activity_Day = mean(sleepBoutTime[ActivityBoxPlot_time==n & sleepBoutTime>0],na.rm=TRUE)), by = id])
        
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
SleepDataSummary <- function(graph = NaN, all=TRUE){
    
  if (is.nan(graph) | graph == "sleep"){
    withProgress(message = 'Compute sleep statistics', value = 0, {
      SleepData$lightDark <- sleepSummary("lightDark")
      incProgress(1/3)
      SleepData$day <- sleepSummary("day")
      incProgress(1/3)
      SleepData$dayLightDark <- sleepSummary("dayLightDark")
      incProgress(1/3)
    })
  }

  if (is.nan(graph) | graph == "sleep"){
    SleepData$custom <- sleepSummary("custom")}
}
SleepBoutsDataSummary <- function(graph = NaN, all=TRUE){
  
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
TST_WASODataSummary <- function(graph = NaN, all=TRUE){
  
  if (all == TRUE){
    if (is.nan(graph) | graph == "TST"){
      withProgress(message = 'Compute total sleep time statistics', value = 0, {
        TST_WASOData$TST_all <- TST_WASOSummary("TST","all")
        incProgress(1/2)
        TST_WASOData$TST_day <- TST_WASOSummary("TST","day")
        incProgress(1/2)})
    }
    
    if (is.nan(graph) | graph == "WASO"){
      withProgress(message = 'Compute wake after sleep onset statistics', value = 0, {
        TST_WASOData$WASO_all <- TST_WASOSummary("WASO","all")
        incProgress(1/2)
        TST_WASOData$WASO_day <- TST_WASOSummary("WASO","day")
        incProgress(1/2)})
    }}
}


#Update Labels
updateSleepLabels <- function(){
  
  SleepFiguresXLabel(input$SleepBoxXLabel)
  SleepFiguresYLabel(input$SleepBoxYLabel)
  
  if (input$SleepboxPlotsTabs == "Sleep ratio per light phase"){
    SleepFiguresTitles$lightDark <- input$SleepBoxTitle
  }
  else{
    if (input$SleepboxPlotsTabs == "Sleep ratio per day"){
      SleepFiguresTitles$day <- input$sleepBoxTitle
    }
    else{
      if(input$SleepboxPlotsTabs == "Sleep ratio per day and light phase"){
        SleepFiguresTitles$dayLightDark <- input$SleepBoxTitle
      }
      else{
        SleepFiguresTitles$custom <- input$SleepBoxTitle
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
updateTST_WASOLabels <- function(){
  
  TST_WASOFiguresXLabel(input$TST_WASOXLabel)
  
  if (input$TST_WASOPlotsTabs == "Total Sleep Time"){
    TST_WASOTitles$TST_all <- input$TST_WASOTitle
    TST_WASOFiguresYLabel$TST <- input$TST_WASOYLabel
  }
  else{
    if (input$TST_WASOPlotsTabs == "Total Sleep Time per day"){
      TST_WASOTitles$TST_day <- input$TST_WASOTitle
      TST_WASOFiguresYLabel$TST <- input$TST_WASOYLabel
    }
    else{
      if(input$TST_WASOPlotsTabs == "Wake after sleep onset"){
        TST_WASOTitles$WASO_all <- input$TST_WASOTitle
        TST_WASOFiguresYLabel$WASO <- input$TST_WASOYLabel
      }
      else{
        TST_WASOTitles$WASO_day <- input$TST_WASOTitle
        TST_WASOFiguresYLabel$WASO <- input$TST_WASOYLabel
      }
    }
  }
}

##### Create and update Figures 

# Sleep
updateSleepLightDark <- function() {
  ##### Sleep per light and dark phases figure #####
  
  fig <- statisticPlots(SleepData$lightDark ,input$SleepPlot,input$SleepError, graph = "sleep")+
    labs(title = SleepFiguresTitles$lightDark, x = SleepFiguresXLabel(),
         y = SleepFiguresYLabel())+ coord_cartesian( ylim = c(0,1))
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  
  SleepFigures$lightDark <- fig
}
updateSleepDay <- function() {
  ##### Sleep per day figure #####
  
  fig <- statisticPlots(SleepData$day ,input$SleepPlot,input$SleepError,graph = "sleep")+
    labs(title = SleepFiguresTitles$day, x = SleepFiguresXLabel(),
         y = SleepFiguresYLabel()) +coord_cartesian( ylim = c(0,1))
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  
  SleepFigures$day <- fig
}
updateSleepDayLightDark <- function() {
  
  ##### Sleep per day and per light and dark phases figure #####
  
  fig <- statisticPlots(SleepData$dayLightDark,input$SleepPlot,input$SleepError,graph = "sleep")+
    labs(title = SleepFiguresTitles$dayLightDark, x = SleepFiguresXLabel(),
         y = SleepFiguresYLabel()) + coord_cartesian( ylim = c(0,1))

  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  
  SleepFigures$dayLightDark <- fig
}
updateSleepCustom <- function() {
  
  fig <- statisticPlots(SleepData$custom,input$SleepPlot,input$SleepError,graph = "sleep")+
    labs(title = SleepFiguresTitles$custom, x = SleepFiguresXLabel(),
         y = SleepFiguresYLabel()) + coord_cartesian( ylim = c(0,1))
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  
  SleepFigures$custom <- fig
}

updateSleepFigures <- function (all=TRUE) {
  
    if (all == TRUE){
      withProgress(message = 'Activity figures', value = 0, {
        updateSleepLightDark()
        incProgress(1/3)
        updateSleepDay()
        incProgress(1/3)
        updateSleepDayLightDark()
        incProgress(1/3)
      })
    }
    
    updateSleepCustom()
  
}

# TST and WASO
updateTSTall <- function() {
  fig <- statisticPlots(TST_WASOData$TST_all ,input$SleepPlot,input$SleepError, graph = "sleep")+
    labs(title = TST_WASOTitles$TST_all, x = TST_WASOFiguresXLabel(),
         y = TST_WASOFiguresYLabel$TST)+ coord_cartesian(ylim = input$yLimitsTST_WASOTime)
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  
  TST_WASOFigures$TST_all <- fig
}
updateTSTday <- function() {
  fig <- statisticPlots(TST_WASOData$TST_day ,input$SleepPlot,input$SleepError, graph = "sleep")+
    labs(title = TST_WASOTitles$TST_day, x = TST_WASOFiguresXLabel(),
         y = TST_WASOFiguresYLabel$TST)+ coord_cartesian(ylim = input$yLimitsTST_WASOTime)
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  
  TST_WASOFigures$TST_day <- fig
}
updateWASOall <- function() {
  fig <- statisticPlots(TST_WASOData$WASO_all ,input$SleepPlot,input$SleepError, graph = "sleep")+
    labs(title = TST_WASOTitles$WASO_all, x = TST_WASOFiguresXLabel(),
         y = TST_WASOFiguresYLabel$WASO)+ coord_cartesian(ylim = input$yLimitsTST_WASOTime)
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  
  TST_WASOFigures$WASO_all <- fig
}
updateWASOday <- function() {
  fig <- statisticPlots(TST_WASOData$WASO_day ,input$SleepPlot,input$SleepError, graph = "sleep")+
    labs(title = TST_WASOTitles$WASO_day, x = TST_WASOFiguresXLabel(),
         y = TST_WASOFiguresYLabel$WASO)+ coord_cartesian(ylim = input$yLimitsTST_WASOTime)
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  
  TST_WASOFigures$WASO_day <- fig
}

updateTST_WASOFigures <- function () {
  
  withProgress(message = 'Activity figures', value = 0, {
    updateTSTall()
    incProgress(1/4)
    updateTSTday()
    incProgress(1/4)
    updateWASOall()
    incProgress(1/4)
    updateWASOday()
    incProgress(1/4)
    })
  
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

# Sleep Bout Time
updateBoutSleepTimeLightDark <- function(){
  ##### Sleep Bout time per light and dark phases figure #####
  fig <- statisticPlots(BoutSleepTimeData$lightDark ,input$SleepPlot,input$SleepError)+
    labs(title = BoutSleepTimeFiguresTitles$lightDark, x = BoutSleepTimeFiguresXLabel(),
         y = BoutSleepTimeFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  
  BoutSleepTimeFigures$lightDark <- fig
}
updateBoutSleepTimeDay <- function(){
  ##### Sleep Bout time per day #####
  fig <- statisticPlots(BoutSleepTimeData$day ,input$SleepPlot,input$SleepError)+
    labs(title = BoutSleepTimeFiguresTitles$day, x = BoutSleepTimeFiguresXLabel(),
         y = BoutSleepTimeFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  BoutSleepTimeFigures$day <- fig
}
updateBoutSleepTimeDayLightDark <- function(){
  ##### SleepBout time per day per light and dark phases figure #####
  fig <- statisticPlots(BoutSleepTimeData$dayLightDark ,input$SleepPlot,input$SleepError)+
    labs(title = BoutSleepTimeFiguresTitles$dayLightDark, x = BoutSleepTimeFiguresXLabel(),
         y = BoutSleepTimeFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  BoutSleepTimeFigures$dayLightDark <- fig
}
updateBoutSleepTimeCustom <- function(){
  ##### Sleep bout time custom figure #####
  fig <- statisticPlots(BoutSleepTimeData$custom ,input$SleepPlot,input$SleepError)+
    labs(title = BoutSleepTimeFiguresTitles$custom, x = BoutSleepTimeFiguresXLabel(),
         y = BoutSleepTimeFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
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
  fig <- statisticPlots(BoutSleepLatencyData$all ,input$SleepPlot,input$SleepError)+
    labs(title = BoutSleepLatencyFiguresTitles$all, x = BoutSleepLatencyFiguresXLabel(),
         y = BoutSleepLatencyFiguresYLabel()) + coord_cartesian(ylim = input$yLimitsSleepLatency)
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  BoutSleepLatencyFigures$all <- fig
}
updateBoutSleepLatencyDay <- function(){
  ##### Sleep Latency per day #####
  fig <- statisticPlots(BoutSleepLatencyData$day ,input$SleepPlot,input$SleepError)+
    labs(title = BoutSleepLatencyFiguresTitles$day, x = BoutSleepLatencyFiguresXLabel(),
         y = BoutSleepLatencyFiguresYLabel()) + coord_cartesian(ylim = input$yLimitsSleepLatency)
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
  BoutSleepLatencyFigures$day <- fig
}
updateBoutSleepLatencyCustom <- function(){
  ##### Sleep latency custom figure #####
  fig <- statisticPlots(BoutSleepLatencyData$custom ,input$SleepPlot,input$SleepError)+
    labs(title = BoutSleepLatencyFiguresTitles$custom, x = BoutSleepLatencyFiguresXLabel(),
         y = BoutSleepLatencyFiguresYLabel()) + coord_cartesian(ylim = input$yLimitsSleepLatency)
  
  fig <- whiteBackground(fig, input$titleLetterSize3, input$axisLabelSize3,
                         input$axisNumbersSize3, input$dataLabelSize3)
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

##################### Reset variables #####################
observeEvent(input$files,{
  
  #Get directory
  validate(
    need(nrow(input$files)>0,""))

  #Sleep
  SleepFigures$lightDark <- NULL
  SleepFigures$day <- NULL
  SleepFigures$dayLightDark <- NULL
  SleepFigures$custom <- NULL

  SleepFiguresTitles$lightDark <- "Sleep ratio per light phase"
  SleepFiguresTitles$day <- "Sleep ratio per day"
  SleepFiguresTitles$dayLightDark <- "Sleep ratio per day and per light phase"
  SleepFiguresTitles$custom <- "Sleep ratio"
  SleepFiguresXLabel("")
  SleepFiguresYLabel("Sleep ratio")
  
  #TSTand WASO
  TST_WASOFigures$TST_all <- NULL
  TST_WASOFigures$TST_day <- NULL
  TST_WASOFigures$WASO_all <- NULL
  TST_WASOFigures$WASO_day <- NULL
  
  TST_WASOTitles$TST_all <- "Total sleep time"
  TST_WASOTitles$TST_day <- "Total sleep time per day"
  TST_WASOTitles$WASO_all <- "Wake after sleep onset"
  TST_WASOTitles$WASO_day <- "Wake after sleep onsetper day"
  TST_WASOFiguresXLabel("")
  TST_WASOFiguresYLabel$TST <- "TST (h)"
  TST_WASOFiguresYLabel$WASO <- "WASO (h)"


  #Sleep bout time
  BoutSleepTimeData$lightDark <- NULL
  BoutSleepTimeData$day <- NULL
  BoutSleepTimeData$dayLightDark <- NULL
  BoutSleepTimeData$custom <- NULL

  BoutSleepTimeFigures$lightDark <- NULL
  BoutSleepTimeFigures$day <- NULL
  BoutSleepTimeFigures$dayLightDark <- NULL
  BoutSleepTimeFigures$custom <- NULL

  BoutSleepTimeFiguresTitles$lightDark <- "Sleep bout duration per light phase"
  BoutSleepTimeFiguresTitles$day <- "Sleep bout duration per day"
  BoutSleepTimeFiguresTitles$dayLightDark <- "Sleep bout duration per day and per light phase"
  BoutSleepTimeFiguresTitles$custom <- "Sleep bout duration"
  BoutSleepTimeFiguresXLabel("")
  BoutSleepTimeFiguresYLabel("Mean sleep bout duration (min)")

  #Sleep latency

  BoutSleepLatencyData$all <- NULL
  BoutSleepLatencyData$dayLightDark <- NULL
  BoutSleepLatencyData$custom <- NULL

  BoutSleepLatencyFigures$all <- NULL
  BoutSleepLatencyFigures$dayLightDark <- NULL
  BoutSleepLatencyFigures$custom <- NULL

  BoutSleepLatencyFiguresTitles$all <- "Sleep latency"
  BoutSleepLatencyFiguresTitles$dayLightDark <- "Sleep latency per day"
  BoutSleepLatencyFiguresTitles$custom <- "Sleep latency"
  BoutSleepLatencyFiguresXLabel("")
  BoutSleepLatencyFiguresYLabel("Mean sleep latency (min)")


  ### Clean statistical data
  output$SleepTimeSummary <- NULL
  output$SleepLatencySummary <- NULL
})

############################ Data to plot ################################
#Start analysis data
observeEvent(input$startanalysis,{

  req(damData$dt)
  req("timeDiff" %in% colnames(damData$dt))
  
  #Sleep
  SleepData$lightDark <- NULL
  SleepData$day <- NULL
  SleepData$dayLightDark <- NULL
  SleepData$custom <- NULL
  
  SleepFigures$lightDark <- NULL
  SleepFigures$day <- NULL
  SleepFigures$dayLightDark <- NULL
  SleepFigures$custom <- NULL
  
  SleepFiguresTitles$lightDark <- "Sleep ratio per light phase"
  SleepFiguresTitles$day <- "Sleep ratio per day"
  SleepFiguresTitles$dayLightDark <- "Sleep ratio per day and per light phase"
  SleepFiguresTitles$custom <- "Sleep ratio"
  SleepFiguresXLabel("")
  SleepFiguresYLabel("Sleep ratio")
  
  
  #TST and WASO
  TST_WASOData$TST_all <- NULL
  TST_WASOData$TST_day <- NULL
  TST_WASOData$WASO_all <- NULL
  TST_WASOData$WASO_day <- NULL
  
  TST_WASOFigures$TST_all <- NULL
  TST_WASOFigures$TST_day <- NULL
  TST_WASOFigures$WASO_all <- NULL
  TST_WASOFigures$WASO_day <- NULL
  
  TST_WASOTitles$TST_all <- "Total sleep time"
  TST_WASOTitles$TST_day <- "Total sleep time per day"
  TST_WASOTitles$WASO_all <- "Wake after sleep onset"
  TST_WASOTitles$WASO_day <- "Wake after sleep onsetper day"
  TST_WASOFiguresXLabel("")
  TST_WASOFiguresYLabel$TST <- "TST (h)"
  TST_WASOFiguresYLabel$WASO <- "WASO (h)"
  
  
  #Sleep bout time
  BoutSleepTimeData$lightDark <- NULL
  BoutSleepTimeData$day <- NULL
  BoutSleepTimeData$dayLightDark <- NULL
  BoutSleepTimeData$custom <- NULL
  
  BoutSleepTimeFigures$lightDark <- NULL
  BoutSleepTimeFigures$day <- NULL
  BoutSleepTimeFigures$dayLightDark <- NULL
  BoutSleepTimeFigures$custom <- NULL
  
  BoutSleepTimeFiguresTitles$lightDark <- "Sleep bout duration per light phase"
  BoutSleepTimeFiguresTitles$day <- "Sleep bout duration per day"
  BoutSleepTimeFiguresTitles$dayLightDark <- "Sleep bout duration per day and per light phase"
  BoutSleepTimeFiguresTitles$custom <- "Sleep bout duration"
  BoutSleepTimeFiguresXLabel("")
  BoutSleepTimeFiguresYLabel("Mean sleep bout duration (min)")
  
  #Sleep latency
  
  BoutSleepLatencyData$all <- NULL
  BoutSleepLatencyData$dayLightDark <- NULL
  BoutSleepLatencyData$custom <- NULL
  
  BoutSleepLatencyFigures$all <- NULL
  BoutSleepLatencyFigures$dayLightDark <- NULL
  BoutSleepLatencyFigures$custom <- NULL
  
  BoutSleepLatencyFiguresTitles$all <- "Sleep latency"
  BoutSleepLatencyFiguresTitles$dayLightDark <- "Sleep latency per day"
  BoutSleepLatencyFiguresTitles$custom <- "Sleep latency"
  BoutSleepLatencyFiguresXLabel("")
  BoutSleepLatencyFiguresYLabel("Mean sleep latency (min)")
  
  
  sleepBoutAnalysis()
  
  SleepDataSummary()
  TST_WASODataSummary()
  SleepBoutsDataSummary()
  
  updateSleepFigures()
  updateTST_WASOFigures()
  updateBoutSleepTimeFigures()
  updateBoutSleepLatencyFigures()
  
  
  updateTabsetPanel(session, "SleepboxPlotsTabs", selected = "Mean sleep")
  updateSliderInput(session, "yLimitsTST_WASOTime", max = l_period())

})

#Change DAM data if sleep time changes
observeEvent(input$sleepAnalysis2,{
  
  req(damData$dt)
  
  #get DAM data
  damData$dt[,"asleep" := sleep_dam_annotation(damData$dt[,1:3],min_time_immobile = 60*input$sleepTime2)[,'asleep']]
  
  settings <- settingsTable()
  settings[1,2] <- l_period()
  settings[2,2] <- l_hours()
  settings[5,2] <- input$sleepTime2
  settingsTable(settings)
  
  sleepBoutAnalysis()
  
  SleepDataSummary()
  TST_WASODataSummary()
  SleepBoutsDataSummary()
  
  updateSleepFigures()
  updateTST_WASOFigures()
  updateBoutSleepTimeFigures()
  updateBoutSleepLatencyFigures()
  
  updateSleep() #Sleep Chronogram
  
  updateSliderInput(session,"sleepTime",value = input$sleepTime2)
  
})


#Update figures if data is deleted
observeEvent(input$deleteAnimals,{
  
  sleepBoutAnalysis()
  
  SleepDataSummary()
  TST_WASODataSummary()
  SleepBoutsDataSummary()
  
  updateSleepFigures()
  updateTST_WASOFigures()
  updateBoutSleepTimeFigures()
  updateBoutSleepLatencyFigures()
  
})
observeEvent(input$deleteInactivity,{
  
  sleepBoutAnalysis()
  
  SleepDataSummary()
  TST_WASODataSummary()
  SleepBoutsDataSummary()
  
  updateSleepFigures()
  updateTST_WASOFigures()
  updateBoutSleepTimeFigures()
  updateBoutSleepLatencyFigures()
  
})

############################ Change labels ###############################

#Sleep
observeEvent(input$SleepSummary_cell_edit,{
  
  info <- input$SleepSummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  changeLabels <- function(sleep){
    
    conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=sleep, FUN=mean)[,1]
    Label <-  str_split_fixed(conditions, " - ",2)[,2]
    
    for (k in 1: nrow(sleep)){
      if (sleep[k,'xPlot']==Label[i]){
        sleep[k,'xPlot'] <- v
      }
    }
    return(sleep)
  }
  
  if (input$SleepboxPlotsTabs == "Sleep ratio per light phase"){
    SleepData$lightDark <- changeLabels(SleepData$lightDark)
    updateSleepLightDark()
  }
  else{
    if (input$SleepboxPlotsTabs == "Sleep ratio per day"){
      SleepData$day <- changeLabels(SleepData$day)
      updateSleepDay()
    }
    else{
      if(input$SleepboxPlotsTabs == "Sleep ratio per day and light phase"){
        SleepData$dayLightDark <- changeLabels(SleepData$dayLightDark)
        updateSleepDayLightDark()
      }
      else{
        SleepData$custom <- changeLabels(SleepData$custom)
        updateSleepCustom()
      }
    }
  }
  
})

#TST and WASO
observeEvent(input$TST_WASOSummary_cell_edit,{
  
  info <- input$TST_WASOSummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  changeLabels <- function(TST_WASO){
    
    conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=TST_WASO, FUN=mean)[,1]
    Label <-  str_split_fixed(conditions, " - ",2)[,2]
    
    for (k in 1: nrow(TST_WASO)){
      if (TST_WASO[k,'xPlot']==Label[i]){
        TST_WASO[k,'xPlot'] <- v
      }
    }
    return(TST_WASO)
  }
  
  if (input$TST_WASOPlotsTabs == "Total Sleep Time"){
    TST_WASOData$TST_all <- changeLabels(TST_WASOData$TST_all)
    updateTSTall()
  }
  else{
    if (input$TST_WASOPlotsTabs == "Total Sleep Time per day"){
      TST_WASOData$TST_day <- changeLabels(TST_WASOData$TST_day)
      updateTSTday()
    }
    else{
      if(input$TST_WASOPlotsTabs == "Wake after sleep onset"){
        TST_WASOData$WASO_all <- changeLabels(TST_WASOData$WASO_all)
        updateWASOall()
      }
      else{
        TST_WASOData$WASO_day <- changeLabels(TST_WASOData$WASO_day)
        updateWASOday()
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
    BoutSleepLatencyData$all <- changeLabels(BoutSleepLatencyData$all)
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

#Sleep table
observe({
  
  req(nrow(damData$dt)>0)
  req(nrow(SleepData$lightDark)>0)
  
  if (input$SleepboxPlotsTabs == "Sleep ratio per light phase"){
    sleep <- SleepData$lightDark
  }
  else{
    if (input$SleepboxPlotsTabs == "Sleep ratio per day"){
      sleep <- SleepData$day
    }
    else{
      if(input$SleepboxPlotsTabs == "Sleep ratio per day and light phase"){
        sleep <- SleepData$dayLightDark
      }
      else{
        sleep <- SleepData$custom
      }
    }
  }
  
  output$SleepSummary <- DT::renderDataTable(statisticsReport(sleep),
                                             escape = FALSE, selection = 'none', 
                                             editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
})

#TST and WASO table
observe({

  req(nrow(damData$dt)>0)
  req(nrow(TST_WASOData$TST_all)>0)

  if (input$TST_WASOPlotsTabs == "Total Sleep Time"){
    TST_WASO <- TST_WASOData$TST_all
  }
  else{
    if (input$TST_WASOPlotsTabs == "Total Sleep Time per day"){
      TST_WASO <- TST_WASOData$TST_day
    }
    else{
      if(input$TST_WASOPlotsTabs == "Wake after sleep onset"){
        TST_WASO <- TST_WASOData$WASO_all
      }
      else{
        TST_WASO <- TST_WASOData$WASO_day
      }
    }
  }

  updateSliderInput(session,'yLimitsTST_WASOTime',min = 0, max =ceiling(max(TST_WASO[,'yPlot'], na.rm =TRUE)), value = c(0,max(TST_WASO[,'yPlot'])))

  output$TST_WASOSummary <- DT::renderDataTable(statisticsReport(TST_WASO),
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
observeEvent(input$updateSleepStatistics,{
  
  req(nrow(damData$dt)>0)
  req(nrow(BoutSleepTimeData$lightDark)>0)
  
  updateSleepBoutsLabels()
  updateSleepLabels()
  updateTST_WASOLabels()
  
  
  damData$dt[, 'SleepBoxPlot_time' := floor(damData$dt[,'t']/(input$SleepGroupBoxTime * l_period()*3600))]
  
  settings <- settingsTable()
  settings[1,2] <- l_period()
  settings[2,2] <- l_hours()
  settingsTable(settings)
  
  #################################

  withProgress(message = 'Update data and graphs', value = 0, {
    
    SleepDataSummary()
    updateSleepFigures()
    incProgress(0.25)
    
    SleepBoutsDataSummary()
    updateBoutSleepTimeFigures()
    updateBoutSleepLatencyFigures()
    incProgress(0.5)
    
    TST_WASODataSummary()
    updateTST_WASOFigures()
    incProgress(0.25)
  })
  
})

#Update Text fields
observeEvent(input$SleepboxPlotsTabs,{
  #Update sleep text fields
  updateTextInput(session, "SleepBoxXLabel", value = SleepFiguresXLabel())
  updateTextInput(session, "SleepBoxYLabel", value = SleepFiguresYLabel())
  
  if (input$SleepboxPlotsTabs == "Sleep ratio per light phase"){
    updateTextInput(session,"SleepBoxTitle", value = SleepFiguresTitles$lightDark)
  }
  else{
    if (input$SleepboxPlotsTabs == "Sleep ratio per day"){
      updateTextInput(session,"SleepBoxTitle", value = SleepFiguresTitles$day)
    }
    else{
      if(input$SleepboxPlotsTabs == "Sleep ratio per day and light phase"){
        updateTextInput(session,"SleepBoxTitle", value = SleepFiguresTitles$dayLightDark)
      }
      else{
        updateTextInput(session,"SleepBoxTitle", value = SleepFiguresTitles$custom)
      }
    }
  }
})
observeEvent(input$TST_WASOPlotsTabs,{
  #Update sleep text fields
  updateTextInput(session, "TST_WASOXLabel", value = SleepFiguresXLabel())
  
  if (input$TST_WASOPlotsTabs == "Total Sleep Time"){
    updateTextInput(session,"TST_WASOTitle", value = TST_WASOTitles$TST_all)
    updateTextInput(session, "TST_WASOYLabel", value = TST_WASOFiguresYLabel$TST)
  }
  else{
    if (input$TST_WASOPlotsTabs == "Total Sleep Time per day"){
      updateTextInput(session,"TST_WASOTitle", value = TST_WASOTitles$TST_day)
      updateTextInput(session, "TST_WASOYLabel", value = TST_WASOFiguresYLabel$TST)
    }
    else{
      if(input$TST_WASOPlotsTabs == "Wake after sleep onset"){
        updateTextInput(session,"TST_WASOTitle", value = TST_WASOTitles$WASO_all)
        updateTextInput(session, "TST_WASOYLabel", value = TST_WASOFiguresYLabel$WASO)
      }
      else{
        updateTextInput(session,"TST_WASOTitle", value = TST_WASOTitles$WASO_day)
        updateTextInput(session, "TST_WASOYLabel", value = TST_WASOFiguresYLabel$WASO)
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

#Sleep box plots
output$sleepDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  validate(
    need (!is.null(SleepFigures$lightDark),"No graph")
  )
  
  fig<- SleepFigures$lightDark
  
  return(fig)
  
  
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$sleepPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  validate(
    need (!is.null(SleepFigures$day),"No graph")
  )
  
  fig <-SleepFigures$day
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$sleepDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  validate(
    need (!is.null(SleepFigures$dayLightDark),"No graph")
  )
  
  fig <- SleepFigures$dayLightDark
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$sleepCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  req(input$SleepGroupBoxTime)
  req(input$SleepBoxTime)
  
  validate(
    need (!is.null(SleepFigures$custom),"No graph")
  )
  
  fig <- SleepFigures$custom
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)

#TST and WaSO box plots
output$TSTall <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  validate(
    need (!is.null(TST_WASOFigures$TST_all),"No graph")
  )
  
  fig<- TST_WASOFigures$TST_all
  
  return(fig)
  
  
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$TSTday <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  validate(
    need (!is.null(TST_WASOFigures$TST_day),"No graph")
  )
  
  fig <-TST_WASOFigures$TST_day
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$WASOall <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  validate(
    need (!is.null(TST_WASOFigures$WASO_all),"No graph")
  )
  
  fig <- TST_WASOFigures$WASO_all
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$WASOday <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  validate(
    need (!is.null(TST_WASOFigures$WASO_day),"No graph")
  )
  
  fig <- TST_WASOFigures$WASO_day
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)

#Sleep time box plots
output$SleepTimeDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData$lightDark)>0, "")
  )
  
  fig <- BoutSleepTimeFigures$lightDark
  
  return(fig)
  
  
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$SleepTimePerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData$lightDark)>0, "")
  )
  
  fig <- BoutSleepTimeFigures$day
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$SleepTimeDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData$lightDark)>0, "")
  )
  
  fig <- BoutSleepTimeFigures$dayLightDark
  
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$SleepTimeCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepTimeData$lightDark)>0, "")
  )
  req(input$SleepGroupBoxTime)
  req(input$SleepBoxTime)
  
  fig <- BoutSleepTimeFigures$custom
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)


#Sleep latency box plots
output$SleepLatencyDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepLatencyData$all)>0, "")
  )
  
  fig <- BoutSleepLatencyFigures$all
  
  
  return(fig)
  
  
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$SleepLatencyPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepLatencyData$all)>0, "")
  )
  
  fig <- BoutSleepLatencyFigures$day
  
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)
output$SleepLatencyCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutSleepLatencyData$all)>0, "")
  )
  req(input$SleepGroupBoxTime)
  req(input$SleepBoxTime)
  
  fig <- BoutSleepLatencyFigures$custom
  
  
  return(fig)
  
}, width = function() input$SleepWidth,
height = function() input$SleepHeight,
res = 96)


######################## SAVE FIGS AND DATA #############################

shinyjs::disable("saveActivityFig")
shinyjs::disable("saveSleepFig")
shinyjs::disable("saveActivityReport")
shinyjs::disable("saveSleepReport")
# 
#Save figures Figure

observe({
  req(nrow(damData$dt)>0)
  req(SleepFigures$lightDark)

  if (nrow(damData$dt)>0){
    shinyjs::enable("saveSleepFig")
  }
  else{
    shinyjs::disable("saveSleepFig")
  }

  output$saveSleepFig <- downloadHandler(

    filename = function(){
      paste0(input$SleepboxPlotsTabs,input$SleepFig)
    },
    content = function(file){
      
      if (input$SleepboxPlotsTabs == "Sleep ratio per light phase"){
        fig <- SleepFigures$lightDark
      }
      else{
        if (input$SleepboxPlotsTabs == "Sleep ratio per day"){
          fig <- SleepFigures$day
        }
        else{
          if(input$SleepboxPlotsTabs == "Sleep ratio per day and light phase"){
            fig <- SleepFigures$dayLightDark
          }
          else{
            fig <- SleepFigures$custom
          }
        }
      }
      ggsave(filename = file, plot = fig,
         width = round(input$SleepWidth/97), height= round(input$SleepHeight/97))
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
             width = round(input$ActivityWidth/97), height= round(input$ActivityHeight/97))
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
             width = round(input$ActivityWidth/97), height= round(input$ActivityHeight/97))
    })
})
# 
# 
#Save graph data

observe({
  req(nrow(damData$dt)>0)
  req(SleepData$lightDark)

  if (nrow(damData$dt)>0){
    shinyjs::enable("saveSleepReport")
  }
  else{
    shinyjs::disable("saveSleepReport")
  }

  output$saveSleepReport <- downloadHandler(

    filename = function(){
      paste0(input$SleepboxPlotsTabs,'.xlsx')
    },
    content = function(file){

      wb <- createWorkbook(type="xlsx")
      
      if (input$SleepboxPlotsTabs == "Sleep ratio per light phase"){
        data <- SleepData$lightDark
      }
      else{
        if (input$SleepboxPlotsTabs == "Sleep ratio per day"){
          data <- SleepData$day
        }
        else{
          if(input$SleepboxPlotsTabs == "Sleep ratio per day and light phase"){
            data <- SleepData$dayLightDark
          }
          else{
            data <- SleepData$custom
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

