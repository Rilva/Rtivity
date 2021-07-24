##### FUNCTIONS #####

#Standard error
se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

#Present statistics
dataReport <- function(Data){
  
  req(nrow(Data)>0)
  
  conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=mean)[,1]
  
  Condition <- str_split_fixed(conditions, " - ",2)[,1]
  Label <-  str_split_fixed(conditions, " - ",2)[,2]
  
  N  <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=length)[,2]
  Data_mean <- round(aggregate(yPlot ~ interaction(labels,xPlot, sep = " - "), data=Data, FUN=mean)[,2],3)
  Data_se <- round(aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=se)[,2],3)
  Data_std <- round(aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=sd)[,2],3)
  Data_median <- round(aggregate(yPlot ~ interaction(labels,xPlot, sep = " - "), data=Data, FUN=median)[,2],3)
  Data_25 <- round(aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=quantile)[,2][,2],3)
  Data_75 <- round(aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=quantile)[,2][,4],3)
  
  return(data.frame(Condition, Label, N, Data_mean, Data_se, Data_std, Data_median, Data_25, Data_75))
}

#Report to save
saveReport <- function(data){
  
  orderedData <- data[order(data[,'order'])]
  
  
  resultSplit <- data.frame(split(orderedData$yPlot, orderedData$xPlot, drop = TRUE))
  data <- data[1:nrow(resultSplit),]
  data <- data[order(data[,'order'])]
  
  
  File <- data$File[1:nrow(resultSplit)]
  Labels <- data$labels[1:nrow(resultSplit)]
  Channels <- data$region_id[1:nrow(resultSplit)]
  Start_date <- data$start_datetime[1:nrow(resultSplit)]
  End_date <- data$stop_datetime[1:nrow(resultSplit)]
  
  exportData <- data.frame(File, Labels, Channels, Start_date, End_date, resultSplit)
  names(exportData)[6:ncol(exportData)]<-names(resultSplit)
  
  return(exportData)
}

# Statistical graphics
statisticPlots <- function(data, plot, error = NaN, graph = "activity" ){
  
  
  if (plot == "BoxPlot"){
    fig <- ggplot(data, aes(x=xPlot, y=yPlot, fill=Data, na.rm=TRUE))+
      geom_boxplot(outlier.colour = "black", na.rm=TRUE)
  }
  else{ if (plot == "DotPlot"){
    fig <- ggplot(data, aes(x=xPlot, y=yPlot, colour=Data, na.rm=TRUE))+
      geom_point(position=position_jitterdodge(),na.rm=TRUE, size = 0.5)+
      stat_summary(fun.data = error, position = position_dodge(width=0.75), size = 1, width = 0.25, geom="errorbar")+
      stat_summary(geom = "point",fun = "mean", size = 3, position = position_dodge(width=0.75))
  }
    else if (plot == "pointRange"){
      fig <- ggplot(data, aes(x=xPlot, y=yPlot, colour=Data, na.rm=TRUE))+
        stat_summary(fun.data = error, position = position_dodge(width=0.75), size = 1, width = 0.25, geom="errorbar")+
        stat_summary(geom = "point",fun = "mean", size = 3, position = position_dodge(width=0.75))
    }
    else{
      if (plot == "BarPlot"){
        fig <- ggplot(data, aes(x=xPlot, y=yPlot, fill=Data, na.rm=TRUE))+
          stat_summary(fun.data = error, size = 0.5, width = 0.25, geom="errorbar", position = position_dodge(width=0.8))+
          stat_summary(geom = "bar",fun = "mean", width = 0.6, color = "black", size = 0.5, position = position_dodge(width=0.8))
      }
    }
    
  }
  
  #Update y Limits
  if (graph  == "activity"){
    updateSliderInput(session,'yLimits',min = 0, max =ceiling(max(data[,'yPlot'], na.rm =TRUE)), value = input$yLimits)
    
    fig <- fig +
      xlab(input$ActivityBoxXLabel)+
      coord_cartesian( ylim = input$yLimits)
    
    if (input$ActivityBoxYLabel == ""){
      fig <- fig + ylab(paste("Mean activity"))
    }
    else{
      fig <- fig + ylab(input$ActivityBoxYLabel)
    }
    
  }
  if (graph == "sleep"){
    
    fig <- fig +
      xlab(input$SleepBoxXLabel)+
      coord_cartesian( ylim = c(0,1))
    
    if (input$SleepBoxYLabel == ""){
      fig <- fig + ylab(paste("Mean sleep ratio"))
    }
    else{
      fig <- fig + ylab(input$SleepBoxYLabel)
    }
  }

  if (graph == "boutActivity"){
    updateSliderInput(session,'yLimitsBoutActivity',min = 0, max =ceiling(max(data[,'yPlot'], na.rm =TRUE)), value = input$yLimitsBoutActivity)
      
    fig <- fig +
      xlab(input$BoutActivityBoxXLabel)+
      coord_cartesian(ylim = input$yLimitsBoutActivity)
        
    if (input$BoutActivityBoxYLabel == ""){
      fig <- fig + ylab(paste("Mean activity per bout"))
    }
    else{
      fig <- fig + ylab(input$BoutActivityBoxYLabel)
    }
  }
  if (graph == "boutTime"){
    updateSliderInput(session,'yLimitsBoutTime',min = 0, max =ceiling(max(data[,'yPlot'], na.rm =TRUE)), value = input$yLimitsBoutTime)
        
    fig <- fig +
      xlab(input$BoutTimeBoxXLabel)+
      coord_cartesian( ylim = input$yLimitsBoutTime)
        
    if (input$BoutTimeBoxYLabel == ""){
      fig <- fig + ylab(paste("Mean time per activity bout (s)"))
    }
    else{
      fig <- fig + ylab(input$BoutTimeBoxYLabel)
    }
  }
   if (graph == "sleepLatency"){
   updateSliderInput(session,'yLimitsSleepLatency',min = 0, max =ceiling(max(data[,'yPlot'], na.rm =TRUE)), value = input$yLimitsSleepLatency)
  
   fig <- fig +
     xlab(input$SleepLatencyBoxXLabel)+
     coord_cartesian(ylim = input$yLimitsSleepLatency)
  
    if (input$SleepLatencyBoxYLabel == ""){
     fig <- fig + ylab(paste("Mean sleep latency time (s)"))
   }
   else{
     fig <- fig + ylab(input$SleepLatencyBoxYLabel)
   }
  }
  if (graph == "sleepTime"){
    updateSliderInput(session,'yLimitsBoutSleepTime',min = 0, max =ceiling(max(data[,'yPlot'], na.rm =TRUE)), value = input$yLimitsBoutSleepTime)
    
    fig <- fig +
      xlab(input$SleepTimeBoxXLabel)+
      coord_cartesian( ylim = input$yLimitsBoutSleepTime)
    
    if (input$SleepTimeBoxYLabel == ""){
      fig <- fig + ylab(paste("Mean sleep latency time (s)"))
    }
    else{
      fig <- fig + ylab(input$SleepTimeBoxYLabel)
    }
  }
  
  return (whiteBackground(fig) + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
            scale_linetype_manual(values=graphsAestethics$df[,'lineType']) +
            scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], .8)))
}

############################# VARIABLES ##################################

### Activity data and figure variables
ActivityData <- reactiveVal()
ActivityFig <- reactiveVal()

### Sleep data and figure variables
SleepData <- reactiveVal()
SleepFig <- reactiveVal()


############################ Data to plot ################################

#Get data to plot
activitySummary <- function(){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ### Create data for statistic plots
  if(input$ActivityboxPlotsTabs == "Mean activity"){
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(Activity_Light = sum(activity[day_night == "TRUE"],na.rm=TRUE)), by = id])
    summaryNight <- rejoin(damData$dt[, .(Activity_Dark = sum(activity[day_night == "FALSE"],na.rm = TRUE)), by = id])
    
    summaryDT <- cbind(summaryLight, summaryNight[,Activity_Dark])
    names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Dark")
    
    #Get summary by light and dark phase  
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Activity_"),variable.name = "xPlot", value.name = "yPlot")
    
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "Activity_Dark", "Dark")]
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "Activity_Light", "Light")]
  }
  
  if(input$ActivityboxPlotsTabs == "Mean activity per day"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(Activity = sum(activity[experimentDay==startDay],na.rm=TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryDT)[names(summaryDT) == "Activity"] <- paste0("Activity_Day_",startDay)
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(Activity = sum(activity[experimentDay==n], na.rm = TRUE)), by = id])
        
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
  
  if(input$ActivityboxPlotsTabs == "Mean activity daytime vs nighttime"){
    
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(activity_Day = sum(activity[experimentDay==startDay & day_night == "TRUE"],na.rm=TRUE)), by = id])
    summaryNight <- rejoin(damData$dt[, .(activity_Night = sum(activity[experimentDay==startDay & day_night == "FALSE"],na.rm = TRUE)), by = id])
    
    #Change names to use in the graphs
    names(summaryLight)[names(summaryLight) == "activity_Day"] <- paste0("Activity_Day_",startDay,"_Light")
    
    summaryDT <- cbind(summaryLight, summaryNight[,activity_Night])
    names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",startDay,"_Dark")
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(activity_Day = sum(activity[experimentDay==n & day_night == "TRUE"], na.rm = TRUE)), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,activity_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",n,"_Light")
        
        summary <- rejoin(damData$dt[, .(activity_Night = sum(activity[experimentDay==n & day_night == "FALSE"], na.rm = TRUE)), by = id])
        
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
  
  if(input$ActivityboxPlotsTabs == "Mean activity custom time"){
    
    #Separate by customized activity
    startTime <- min(damData$dt[,activityBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(activity_Day = sum(activity[activityBoxPlot_time==startTime],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "activity_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,activityBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(activity_Day = sum(activity[activityBoxPlot_time==n],na.rm=TRUE)), by = id])
        
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
sleepSummary <- function(){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ### Create data for statistic plots
  if(input$SleepboxPlotsTabs == "Mean sleep"){
    
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
  
  if(input$SleepboxPlotsTabs == "Mean sleep per day"){
    
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
  
  if(input$SleepboxPlotsTabs == "Mean sleep daytime vs nighttime"){
    
    
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
  
  if(input$SleepboxPlotsTabs == "Mean sleep custom time"){
    
    #Separate by customized activity
    startTime <- min(damData$dt[,activityBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(sleep_Day = mean(asleep[activityBoxPlot_time==startTime],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "sleep_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,activityBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(sleep_Day = mean(asleep[activityBoxPlot_time==n],na.rm=TRUE)), by = id])
        
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

observe({
  req(damData$dt)
  req('experimentDay' %in% colnames(damData$dt))
  req(input$activityGroupBoxTime)
  req(input$activityBoxTime)
  
  ActivityData(activitySummary())
  SleepData(sleepSummary())
})

########################### Change labels ###############################

#Activity
observeEvent(input$ActivitySummary_cell_edit,{
  
  info <- input$ActivitySummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  activity <- ActivityData()
  conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=activity, FUN=mean)[,1]
  Label <-  str_split_fixed(conditions, " - ",2)[,2]
  
  for (k in 1: nrow(activity)){
    if (activity[k,'xPlot']==Label[i]){
      activity[k,'xPlot'] <- v
    }
  }
  
  ActivityData(activity)
  
})
observeEvent(input$ActivityboxPlotsTabs,{
  
  req(damData$dt)
  summaryDT_melted <- activitySummary()
  
  ActivityData(summaryDT_melted)
})

#Sleep
observeEvent(input$SleepSummary_cell_edit,{
  
  info <- input$SleepSummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  sleep <- SleepData()
  conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=sleep, FUN=mean)[,1]
  Label <-  str_split_fixed(conditions, " - ",2)[,2]
  
  for (k in 1: nrow(sleep)){
    if (sleep[k,'xPlot']==Label[i]){
      sleep[k,'xPlot'] <- v
    }
  }
  
  SleepData(sleep)
  
})
observeEvent(input$SleepboxPlotsTabs,{
  
  req(damData$dt)
  summaryDT_melted <- sleepSummary()
  
  SleepData(summaryDT_melted)
})

#Update data presented
observe({
  req(nrow(damData$dt)>0)
  req(ActivityData())
  
  output$ActivitySummary <- DT::renderDataTable(dataReport(ActivityData()),
                                                escape = FALSE, selection = 'none', 
                                                editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
  output$SleepSummary <- DT::renderDataTable(dataReport(SleepData()),
                                             escape = FALSE, selection = 'none', 
                                             editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
})

observe({
  
  if(is.null(damData$dt)){
    ActivityData(NULL)
    SleepData(NULL)}
})

############################### PLOTS ####################################

#Activity box plots
output$activityDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- statisticPlots(ActivityData() ,input$plot,input$error, graph = "activity")
  
  if (input$ActivityBoxTitle == ""){
    fig <- fig+ggtitle(paste("Total activity during Day and Night times"))}
  else{
    fig <- fig+ggtitle(input$ActivityBoxTitle)
  }
  
  ActivityFig(fig)
  
  return(fig)
  
  
  
},width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$activityPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- statisticPlots(ActivityData() ,input$plot,input$error)
  
  if (input$ActivityBoxTitle == ""){
    fig <- fig+ggtitle(paste("Total activity per day"))}
  else{
    fig <- fig+ggtitle(input$ActivityBoxTitle)
  }
  
  ActivityFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$activityDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- statisticPlots(ActivityData(),input$plot,input$error)
  
  if (input$ActivityBoxTitle == ""){
    fig <- fig+ggtitle(paste("Total activity per light and dark phases"))}
  else{
    fig <- fig+ggtitle(input$ActivityBoxTitle)
  }
  
  ActivityFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$activityCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  req(input$activityGroupBoxTime)
  req(input$activityBoxTime)
  
  fig <- statisticPlots(ActivityData(),input$plot,input$error)
  
  if (input$ActivityBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean activity"))}
  else{
    fig <- fig+ggtitle(input$ActivityBoxTitle)
  }
  
  ActivityFig(fig)
  
  
  return(fig)
  
},width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)


#Sleep box plots
output$sleepDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- statisticPlots(SleepData(),input$plot,input$error, graph = "sleep")
  
  if (input$SleepBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep ratio during Day and Night times"))}
  else{
    fig <- fig+ggtitle(input$SleepBoxTitle)
  }
  
  SleepFig(fig)
  
  
  return(fig)
  
  
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$sleepPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  
  fig <- statisticPlots(SleepData(),input$plot,input$error, graph="sleep")
  
  if (input$SleepBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep ratio per day"))}
  else{
    fig <- fig+ggtitle(input$SleepBoxTitle)
  }
  
  SleepFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$sleepDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- statisticPlots(SleepData() ,input$plot,input$error, graph = "sleep")
  
  if (input$SleepBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep ratio per light and dark phases"))}
  else{
    fig <- fig+ggtitle(input$SleepBoxTitle)
  }
  
  SleepFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$sleepCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  req(input$activityGroupBoxTime)
  req(input$activityBoxTime)
  
  fig <- statisticPlots(SleepData() ,input$plot,input$error, graph = "sleep")
  
  if (input$SleepBoxTitle == ""){
    fig <- fig+ggtitle(paste("Mean sleep ratio"))}
  else{
    fig <- fig+ggtitle(input$SleepBoxTitle)
  }
  
  SleepFig(fig)
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)


######################## SAVE FIGS AND DATA #############################

shinyjs::disable("saveActivityFig")
shinyjs::disable("saveSleepFig")
shinyjs::disable("saveActivityReport")
shinyjs::disable("saveSleepReport")

#Save figures Figure
observe({  
  req(damData$dt)
  
  if (nrow(damData$dt)>0){
    shinyjs::enable("saveActivityFig")
  }
  else{
    shinyjs::disable("saveActivityFig")
  }
  
  output$saveActivityFig <- downloadHandler(
    
    filename = function(){
      paste0(input$ActivityboxPlotsTabs,input$ActivityFig)
    },
    content = function(file){
      ggsave(filename = file, plot = ActivityFig(),
             width = round(input$periodicWidth/97), height= round(input$periodicHeight/97))
    })

})
observe({  
  req(damData$dt)
  
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
      ggsave(filename = file, plot = SleepFig(),
         width = round(input$periodicWidth/97), height= round(input$periodicHeight/97))
    })
})


#Save graph data

observe({
  req(damData$dt)
  req(ActivityData())
  
  if (nrow(damData$dt)>0){
    shinyjs::enable("saveActivityReport")
  }
  else{
    shinyjs::disable("saveActivityReport")
  }
  
  output$saveActivityReport <- downloadHandler(
    
    filename = function(){
      paste0(input$ActivityboxPlotsTabs,'.xlsx')
    },
    content = function(file){
      #Create xlsx workbook of conditions and zeitgeber table
      wb<-createWorkbook(type="xlsx")
      sheet <- createSheet(wb,"Replicates")
      addDataFrame(saveReport(ActivityData()), sheet=sheet, startColumn=1, row.names=FALSE)
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(dataReport(ActivityData()), sheet=sheet, startColumn=1, row.names=FALSE)
      
      saveWorkbook(wb, file = file)
    })
  })

observe({
  req(damData$dt)
  req(SleepData())
  
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
  
      #Create xlsx workbook of conditions and zeitgeber table
      wb<-createWorkbook(type="xlsx")
      sheet <- createSheet(wb,"Replicates")
      addDataFrame(saveReport(SleepData()), sheet=sheet, startColumn=1, row.names=FALSE)
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(dataReport(SleepData()), sheet=sheet, startColumn=1, row.names=FALSE)
      
      saveWorkbook(wb, file = file)
    })
})