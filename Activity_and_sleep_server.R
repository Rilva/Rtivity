##### FUNCTIONS #####

#Standard error
se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

#Present statistics
statisticsReport <- function(Data){
  
  req(nrow(Data)>0)
  
  conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=mean)[,1]
  
  Condition <- str_split_fixed(conditions, " - ",2)[,1]
  Label <-  str_split_fixed(conditions, " - ",2)[,2]
  
  N  <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=length)[,2]
  Data_Mean <- round(aggregate(yPlot ~ interaction(labels,xPlot, sep = " - "), data=Data, FUN=mean)[,2],3)
  Data_SEM <- round(aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=se)[,2],3)
  Data_SD <- round(aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=sd)[,2],3)
  Data_Median <- round(aggregate(yPlot ~ interaction(labels,xPlot, sep = " - "), data=Data, FUN=median)[,2],3)
  Data_25Q <- round(aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=quantile)[,2][,2],3)
  Data_75Q <- round(aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=quantile)[,2][,4],3)
  
  return(data.frame(Condition, Label, N, Data_Mean, Data_SEM, Data_SD, Data_Median, Data_25Q, Data_75Q))
}

#Report to save
dataReport <- function(data){
  
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
  
  return (whiteBackground(fig) + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
            scale_linetype_manual(values=graphsAestethics$df[,'lineType']) +
            scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], .8)))
}

#Functions to calculate data to plot
activitySummary <- function(graph){
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ### Create data for statistic plots
  if(graph == "lightDark"){
    
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
  
  if(graph == "day"){
    
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
  
  if(graph == "dayLightDark"){
    
    
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
  
  if(graph == "custom"){
    
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

#Attribute data to variables
ActivityAndSleepData <- function(graph = NaN, all=TRUE){
  
  if (all == TRUE){
    if (is.nan(graph) | graph == "activity"){
      ActivityData$lightDark <- activitySummary("lightDark")
      ActivityData$day <- activitySummary("day")
      ActivityData$dayLightDark <- activitySummary("dayLightDark")}
    
    if (is.nan(graph) | graph == "sleep"){
      SleepData$lightDark <- sleepSummary("lightDark")
      SleepData$day <- sleepSummary("day")
      SleepData$dayLightDark <- sleepSummary("dayLightDark")}}
  
  if (is.nan(graph) | graph == "activity"){
    ActivityData$custom <- activitySummary("custom")}
  if (is.nan(graph) | graph == "sleep"){
    SleepData$custom <- sleepSummary("custom")}
}

#Update Y axis range
updateYactivity <- function() {
  
  if (input$ActivityboxPlotsTabs == "Mean activity"){
    ActivityFigures$lightDark <- ActivityFigures$lightDark + coord_cartesian(ylim = input$yLimits)
  }
  else{
    if (input$ActivityboxPlotsTabs == "Mean activity per day"){
      ActivityFigures$day <- ActivityFigures$day + coord_cartesian(ylim = input$yLimits)
    }
    else{
      if(input$ActivityboxPlotsTabs == "Mean activity daytime vs nighttime"){
        ActivityFigures$dayLightDark <- ActivityFigures$dayLightDark + coord_cartesian(ylim = input$yLimits)
      }
      else{
        ActivityFigures$custom <- ActivityFigures$custom + coord_cartesian(ylim = input$yLimits)
      }
    }
  }
}

#Update Labels
updateActivityLabels <- function(){
  
  ActivityFiguresXLabel(input$ActivityBoxXLabel)
  ActivityFiguresYLabel(input$ActivityBoxYLabel)
  
  if (input$ActivityboxPlotsTabs == "Mean activity"){
    ActivityFiguresTitles$lightDark <- input$ActivityBoxTitle
  }
  else{
    if (input$ActivityboxPlotsTabs == "Mean activity per day"){
      ActivityFiguresTitles$day <- input$ActivityBoxTitle
    }
    else{
      if(input$ActivityboxPlotsTabs == "Mean activity daytime vs nighttime"){
        ActivityFiguresTitles$dayLightDark <- input$ActivityBoxTitle
      }
      else{
        ActivityFiguresTitles$custom <- input$ActivityBoxTitle
      }
    }
  }
  
  SleepFiguresXLabel(input$SleepBoxXLabel)
  SleepFiguresYLabel(input$SleepBoxYLabel)
  
  if (input$SleepboxPlotsTabs == "Mean sleep"){
    SleepFiguresTitles$lightDark <- input$SleepBoxTitle
  }
  else{
    if (input$SleepboxPlotsTabs == "Mean sleep per day"){
      SleepFiguresTitles$day <- input$sleepBoxTitle
    }
    else{
      if(input$SleepboxPlotsTabs == "Mean sleep daytime vs nighttime"){
        SleepFiguresTitles$dayLightDark <- input$SleepBoxTitle
      }
      else{
        SleepFiguresTitles$custom <- input$SleepBoxTitle
      }
    }
  }
}

##### Create and update Figures #####
# Activity
updateActivityLightDark <- function(){
  ##### Activity per light and dark phases figure #####
  fig <- statisticPlots(ActivityData$lightDark ,input$plot,input$error)+
    labs(title = ActivityFiguresTitles$lightDark, x = ActivityFiguresXLabel(),
         y = ActivityFiguresYLabel())
  
  ActivityFigures$lightDark <- fig
}
updateActivityDay <- function(){
  fig <- statisticPlots(ActivityData$day ,input$plot,input$error)+
    labs(title = ActivityFiguresTitles$day, x = ActivityFiguresXLabel(),
         y = ActivityFiguresYLabel())
  
  ActivityFigures$day <- fig
}
updateActivityDayLightDark <- function() {
  ##### Activity per day and per light and dark phases figure #####
  
  fig <- statisticPlots(ActivityData$dayLightDark,input$plot,input$error)+
    labs(title = ActivityFiguresTitles$dayLightDark, x = ActivityFiguresXLabel(),
         y = ActivityFiguresYLabel())
  
  ActivityFigures$dayLightDark <- fig
}
updateActivityCustom <- function() {
  fig <- statisticPlots(ActivityData$custom,input$plot,input$error)+
    labs(title = ActivityFiguresTitles$custom, x = ActivityFiguresXLabel(),
         y = ActivityFiguresYLabel())
  
  ActivityFigures$custom <- fig
}

updateActivityFigures <- function (all=TRUE) {
  
    if (all == TRUE){
      updateActivityLightDark()
      updateActivityDay()
      updateActivityDayLightDark()
    }
    updateActivityCustom()
  
}

# Sleep
updateSleepLightDark <- function() {
  ##### Sleep per light and dark phases figure #####
  
  fig <- statisticPlots(SleepData$lightDark ,input$plot,input$error, graph = "sleep")+
    labs(title = SleepFiguresTitles$lightDark, x = SleepFiguresXLabel(),
         y = SleepFiguresYLabel())+ coord_cartesian( ylim = c(0,1))
  
  SleepFigures$lightDark <- fig
}
updateSleepDay <- function() {
  ##### Sleep per day figure #####
  
  fig <- statisticPlots(SleepData$day ,input$plot,input$error,graph = "sleep")+
    labs(title = SleepFiguresTitles$day, x = SleepFiguresXLabel(),
         y = SleepFiguresYLabel()) +coord_cartesian( ylim = c(0,1))
  
  SleepFigures$day <- fig
}
updateSleepDayLightDark <- function() {
  
  ##### Activity per day and per light and dark phases figure #####
  
  fig <- statisticPlots(SleepData$dayLightDark,input$plot,input$error,graph = "sleep")+
    labs(title = SleepFiguresTitles$dayLightDark, x = SleepFiguresXLabel(),
         y = SleepFiguresYLabel()) + coord_cartesian( ylim = c(0,1))

  SleepFigures$dayLightDark <- fig
}
updateSleepCustom <- function() {
  
  fig <- statisticPlots(SleepData$custom,input$plot,input$error,graph = "sleep")+
    labs(title = SleepFiguresTitles$custom, x = SleepFiguresXLabel(),
         y = SleepFiguresYLabel()) + coord_cartesian( ylim = c(0,1))
  
  SleepFigures$custom <- fig
}

updateSleepFigures <- function (all=TRUE) {
  
    if (all == TRUE){
      
      updateSleepLightDark()
      updateSleepDay()
      updateSleepDayLightDark()
    }
    
    updateSleepCustom()
  
}

############################# VARIABLES ##################################

### Activity data and figure variables
ActivityData <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
ActivityFigures <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
ActivityFiguresTitles <- reactiveValues(lightDark = "Mean activity per light phase", day = "Mean activity per day", 
                                        dayLightDark = "Mean activity per day and per light phase", custom = "Mean activity")
ActivityFiguresXLabel <- reactiveVal("")
ActivityFiguresYLabel <- reactiveVal("Mean activity")

### Sleep data and figure variables
SleepData <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
SleepFigures <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
SleepFiguresTitles <- reactiveValues(lightDark = "Mean sleep ratio per light phase", day = "Mean sleep ratio per day", 
                                        dayLightDark = "Mean sleep ratio per day and per light phase", custom = "Mean sleep ratio")
SleepFiguresXLabel <- reactiveVal("")
SleepFiguresYLabel <- reactiveVal("Mean sleep ratio")

##################### Restart variables #####################
observeEvent(input$files,{
  
  #Get directory
  validate(
    need(nrow(input$files)>0,""))
  
  ###Activity and sleep statistics
  #Activity
  ActivityData$lightDark <- NULL
  ActivityData$day <- NULL
  ActivityData$dayLightDark <- NULL
  ActivityData$custom <- NULL
  
  ActivityFigures$lightDark <- NULL
  ActivityFigures$day <- NULL
  ActivityFigures$dayLightDark <- NULL
  ActivityFigures$custom <- NULL
  
  ActivityFiguresTitles$lightDark <- "Mean activity per light phase"
  ActivityFiguresTitles$day <- "Mean activity per day"
  ActivityFiguresTitles$dayLightDark <- "Mean activity per day and per light phase"
  ActivityFiguresTitles$custom <- "Mean activity"
  ActivityFiguresXLabel("")
  ActivityFiguresYLabel("Mean activity")
  
  #Sleep
  SleepData$lightDark <- NULL
  SleepData$day <- NULL
  SleepData$dayLightDark <- NULL
  SleepData$custom <- NULL
  
  SleepFigures$lightDark <- NULL
  SleepFigures$day <- NULL
  SleepFigures$dayLightDark <- NULL
  SleepFigures$custom <- NULL
  
  SleepFiguresTitles$lightDark <- "Mean sleep ratio per light phase"
  SleepFiguresTitles$day <- "Mean sleep ratio per day"
  SleepFiguresTitles$dayLightDark <- "Mean sleep ratio per day and per light phase"
  SleepFiguresTitles$custom <- "Mean sleep ratio"
  SleepFiguresXLabel("")
  SleepFiguresYLabel("Mean sleep ratio")
})

############################ Data to plot ################################
#Start analysis data
observeEvent(input$startanalysis,{

  req(damData$dt)

  ActivityAndSleepData()
  
  updateTabsetPanel(session, "ActivityboxPlotsTabs", selected = "Mean activity")
  updateTabsetPanel(session, "SleepboxPlotsTabs", selected = "Mean sleep")
  
  activity <- ActivityData$lightDark
  updateSliderInput(session,'yLimits',min = 0, max =ceiling(max(activity[,'yPlot'], na.rm =TRUE)), value = c(0,max(activity[,'yPlot'], na.rm =TRUE)))
  
  updateActivityFigures()
  updateSleepFigures()

})

#Change DAM data if sleep time changes
observeEvent(input$sleepAnalysis2,{
  
  req(damData$dt)
  
  #get DAM data
  damData$dt[,"asleep" := sleep_dam_annotation(damData$dt[,1:3],min_time_immobile = 60*input$sleepTime2)[,'asleep']]
  
  ActivityRepresentationsData()
  updateSleep()
  
  updateSliderInput(session,"sleepTime",value = input$sleepTime2)
  
  ActivityAndSleepData(graph = "sleep")
  updateSleepFigures()
  
  req(nrow((BoutSleepTimeData$lightDark))>0)
  SleepBoutsData()
  updateBoutSleepTimeFigures()
  updateBoutSleepLatencyFigures()
  
})


############################ Change labels ###############################

#Activity
observeEvent(input$ActivitySummary_cell_edit,{
  
  info <- input$ActivitySummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  changeLabels <- function(activity){
    
    conditions <- aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=activity, FUN=mean)[,1]
    Label <-  str_split_fixed(conditions, " - ",2)[,2]
    
    for (k in 1: nrow(activity)){
      if (activity[k,'xPlot']==Label[i]){
        activity[k,'xPlot'] <- v
      }
    }
    return(activity)
  }
  
  if (input$ActivityboxPlotsTabs == "Mean activity"){
    ActivityData$lightDark <- changeLabels(ActivityData$lightDark)
    updateActivityLightDark()
  }
  else{
    if (input$ActivityboxPlotsTabs == "Mean activity per day"){
      ActivityData$day <- changeLabels(ActivityData$day)
      updateActivityDay()
    }
    else{
      if(input$ActivityboxPlotsTabs == "Mean activity daytime vs nighttime"){
        ActivityData$dayLightDark <- changeLabels(ActivityData$dayLightDark)
        updateActivityDayLightDark()
      }
      else{
        ActivityData$custom <- changeLabels(ActivityData$custom)
        updateActivityCustom()
      }
    }
  }
  
})

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
  
  if (input$SleepboxPlotsTabs == "Mean sleep"){
    SleepData$lightDark <- changeLabels(SleepData$lightDark)
    updateSleepLightDark()
  }
  else{
    if (input$SleepboxPlotsTabs == "Mean sleep per day"){
      SleepData$day <- changeLabels(SleepData$day)
      updateSleepDay()
    }
    else{
      if(input$SleepboxPlotsTabs == "Mean sleep daytime vs nighttime"){
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

#################### Update statistical data presented ###################

#Activity table
observe({
  req(nrow(damData$dt)>0)
  req(nrow(ActivityData$lightDark)>0)
  
  if (input$ActivityboxPlotsTabs == "Mean activity"){
    activity <- ActivityData$lightDark
  }
  else{
    if (input$ActivityboxPlotsTabs == "Mean activity per day"){
      activity <- ActivityData$day
    }
    else{
      if(input$ActivityboxPlotsTabs == "Mean activity daytime vs nighttime"){
        activity <- ActivityData$dayLightDark
      }
      else{
        activity <- ActivityData$custom
      }
    }
  }
  
  updateSliderInput(session,'yLimits',min = 0, max =ceiling(max(activity[,'yPlot'], na.rm =TRUE)), value = c(0,max(activity[,'yPlot'])))
  
  output$ActivitySummary <- DT::renderDataTable(statisticsReport(activity),
                                                escape = FALSE, selection = 'none', 
                                                editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
})

#Sleep table
observe({
  
  req(nrow(damData$dt)>0)
  req(nrow(SleepData$lightDark)>0)
  
  if (input$SleepboxPlotsTabs == "Mean sleep"){
    sleep <- SleepData$lightDark
  }
  else{
    if (input$SleepboxPlotsTabs == "Mean sleep per day"){
      sleep <- SleepData$day
    }
    else{
      if(input$SleepboxPlotsTabs == "Mean sleep daytime vs nighttime"){
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

############################### PLOTS ####################################

#Update graphs
observeEvent(input$updateActivitySleepStatistics,{
  
  req(damData$dt)
  
  updateActivityLabels()
  damData$dt[, 'activityBoxPlot_time' := floor(damData$dt[,'t']/(input$activityGroupBoxTime * l_period()*3600))]
  
  withProgress(message = 'Update data and graphs', value = 0, {
    
    ActivityAndSleepData()
    incProgress(0.33)
    updateActivityFigures()
    incProgress(0.33)
    updateSleepFigures()
    incProgress(0.33)
  })
  
  updateYactivity()
  
  #Periodic representation update
  updateFigures()
  
  req(nrow((BoutActivityData$lightDark))>0)
  updateBoutActivityFigures()
  updateBoutTimeFigures()
  updateBoutSleepTimeFigures()
  updateBoutSleepLatencyFigures()
  
  updateYBoutActivity()
  updateYBoutTime()
  updateYSleepTime()
  updateYSleepLatency()
  
  
  
})

#Update text fields
observeEvent(input$ActivityboxPlotsTabs,{
  
  #Update activity text fields
  updateTextInput(session, "ActivityBoxXLabel", value = ActivityFiguresXLabel())
  updateTextInput(session, "ActivityBoxYLabel", value = ActivityFiguresYLabel())
  
  if (input$ActivityboxPlotsTabs == "Mean activity"){
    updateTextInput(session,"ActivityBoxTitle", value = ActivityFiguresTitles$lightDark)
  }
  else{
    if (input$ActivityboxPlotsTabs == "Mean activity per day"){
      updateTextInput(session,"ActivityBoxTitle", value = ActivityFiguresTitles$day)
    }
    else{
      if(input$ActivityboxPlotsTabs == "Mean activity daytime vs nighttime"){
        updateTextInput(session,"ActivityBoxTitle", value = ActivityFiguresTitles$dayLightDark)
      }
      else{
        updateTextInput(session,"ActivityBoxTitle", value = ActivityFiguresTitles$custom)
      }
    }
  }
  
  
})
observeEvent(input$SleepboxPlotsTabs,{
  #Update sleep text fields
  updateTextInput(session, "SleepBoxXLabel", value = SleepFiguresXLabel())
  updateTextInput(session, "SleepBoxYLabel", value = SleepFiguresYLabel())
  
  if (input$SleepboxPlotsTabs == "Mean sleep"){
    updateTextInput(session,"SleepBoxTitle", value = SleepFiguresTitles$lightDark)
  }
  else{
    if (input$SleepboxPlotsTabs == "Mean sleep per day"){
      updateTextInput(session,"SleepBoxTitle", value = SleepFiguresTitles$day)
    }
    else{
      if(input$SleepboxPlotsTabs == "Mean sleep daytime vs nighttime"){
        updateTextInput(session,"SleepBoxTitle", value = SleepFiguresTitles$dayLightDark)
      }
      else{
        updateTextInput(session,"SleepBoxTitle", value = SleepFiguresTitles$custom)
      }
    }
  }
})

#Activity box plots
output$activityDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- ActivityFigures$lightDark
  
  return(fig)
  
  
  
},width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$activityPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- ActivityFigures$day
  
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$activityDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- ActivityFigures$dayLightDark
  
  
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
  
  fig <- ActivityFigures$custom
  
  
  return(fig)
  
},width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)


#Sleep box plots
output$sleepDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig<- SleepFigures$lightDark
  
  return(fig)
  
  
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$sleepPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <-SleepFigures$day
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
res = 96)
output$sleepDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- SleepFigures$dayLightDark
  
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
  
  fig <- SleepFigures$custom
  
  return(fig)
  
}, width = function() input$activityWidth,
height = function() input$activityHeight,
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
  
  req(ActivityFigures$lightDark)

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
      
      if (input$ActivityboxPlotsTabs == "Mean activity"){
        fig <- ActivityFigures$lightDark
      }
      else{
        if (input$ActivityboxPlotsTabs == "Mean activity per day"){
          fig <- ActivityFigures$day
        }
        else{
          if(input$ActivityboxPlotsTabs == "Mean activity daytime vs nighttime"){
            fig <- ActivityFigures$dayLightDark
          }
          else{
            fig <- ActivityFigures$custom
          }
        }
      }
      
      ggsave(filename = file, plot = fig,
             width = round(input$activityWidth/97), height= round(input$activityHeight/97))
    })

})
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
      
      if (input$SleepboxPlotsTabs == "Mean sleep"){
        fig <- SleepFigures$lightDark
      }
      else{
        if (input$SleepboxPlotsTabs == "Mean sleep per day"){
          fig <- SleepFigures$day
        }
        else{
          if(input$SleepboxPlotsTabs == "Mean sleep daytime vs nighttime"){
            fig <- SleepFigures$dayLightDark
          }
          else{
            fig <- SleepFigures$custom
          }
        }
      }
      ggsave(filename = file, plot = fig,
         width = round(input$activityWidth/97), height= round(input$activityHeight/97))
    })
})
# 
# 
#Save graph data
observe({
  req(nrow(damData$dt)>0)
  req(ActivityData$lightDark)

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
      
      if (input$ActivityboxPlotsTabs == "Mean activity"){
        data <- ActivityData$lightDark
      }
      else{
        if (input$ActivityboxPlotsTabs == "Mean activity per day"){
          data <- ActivityData$day
        }
        else{
          if(input$ActivityboxPlotsTabs == "Mean activity daytime vs nighttime"){
            data <- ActivityData$dayLightDark
          }
          else{
            data <- ActivityData$custom
          }
        }
      }
      
      animalsData <-  dataReport(data)
      
      wb<-createWorkbook(type="xlsx")
      sheet <- createSheet(wb,"Replicates")
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
      
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(statisticsReport(data), sheet=sheet, startColumn=1, row.names=FALSE)

      saveWorkbook(wb, file = file)
    })
  })
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

      #Create xlsx workbook of conditions and zeitgeber table
      
      if (input$SleepboxPlotsTabs == "Mean sleep"){
        data <- SleepData$lightDark
      }
      else{
        if (input$SleepboxPlotsTabs == "Mean sleep per day"){
          data <- SleepData$day
        }
        else{
          if(input$SleepboxPlotsTabs == "Mean sleep daytime vs nighttime"){
            data <- SleepData$dayLightDark
          }
          else{
            data <- SleepData$custom
          }
        }
      }
      
      animalsData <-  dataReport(data)
      
      wb<-createWorkbook(type="xlsx")
      sheet <- createSheet(wb,"Replicates")
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
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(statisticsReport(data), sheet=sheet, startColumn=1, row.names=FALSE)

      saveWorkbook(wb, file = file)
    })
})