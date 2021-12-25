############################# VARIABLES ##################################

### Activity data and figure variables
ActivityData <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
ActivityFigures <- reactiveValues(lightDark = NULL, day = NULL, dayLightDark = NULL, custom = NULL)
ActivityFiguresTitles <- reactiveValues(lightDark = "Activity per light phase", day = "Activity per day", 
                                        dayLightDark = "Activity per day and per light phase", custom = "Activity")
ActivityFiguresXLabel <- reactiveVal("")
ActivityFiguresYLabel <- reactiveVal("Activity (counts)")


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



############################# FUNCTIONS #####

##### Statistics plots
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
  
  return (fig + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
            scale_linetype_manual(values=graphsAestethics$df[,'lineType']) +
            scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], .8)))
}

##### Statistics calculus
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
  Data_1st_Quartile <- round(aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=quantile)[,2][,2],3)
  Data_3rd_Quartile <- round(aggregate(yPlot ~ interaction(labels, xPlot,sep = " - "), data=Data, FUN=quantile)[,2][,4],3)
  
  return(data.frame(Condition, Label, N, Data_Mean, Data_SEM, Data_SD, Data_Median, Data_1st_Quartile, Data_3rd_Quartile))
}

# Statistics report
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



#Functions to calculate data to plot
activitySummary <- function(graph){
  
  sumNA <- function(x) if((is.nan(mean(x)))) NA_integer_ else sum(x, na.rm=TRUE)
  
  req(damData$dt)
  
  #Day 0 of the data
  startDay <- min(damData$dt[,experimentDay])
  
  ### Create data for statistic plots
  if(graph == "lightDark"){
    
    #Light and Dark of day 0
    summaryLight <- rejoin(damData$dt[, .(Activity_Light = sumNA(activity[day_night == "TRUE"])), by = id])
    summaryNight <- rejoin(damData$dt[, .(Activity_Dark = sumNA(activity[day_night == "FALSE"])), by = id])
    
    summaryDT <- cbind(summaryLight, summaryNight[,Activity_Dark])
    names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Dark")
    
    #Get summary by light and dark phase  
    summaryDT_melted <- melt(summaryDT, measure.vars = patterns("Activity_"),variable.name = "xPlot", value.name = "yPlot")
    
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "Activity_Dark", "Dark")]
    summaryDT_melted[, xPlot := str_replace_all(xPlot, "Activity_Light", "Light")]
  }
  
  if(graph == "day"){
    
    #Light and Dark of day 0
    summaryDT <- rejoin(damData$dt[, .(Activity = sumNA(activity[experimentDay==startDay])), by = id])
    
    #Change names to use in the graphs
    names(summaryDT)[names(summaryDT) == "Activity"] <- paste0("Activity_Day_",startDay)
    
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(Activity = sumNA(activity[experimentDay==n])), by = id])
        
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
    summaryLight <- rejoin(damData$dt[, .(activity_Day = sumNA(activity[experimentDay==startDay & day_night == "TRUE"])), by = id])
    summaryNight <- rejoin(damData$dt[, .(activity_Night = sumNA(activity[experimentDay==startDay & day_night == "FALSE"])), by = id])
    
    #Change names to use in the graphs
    names(summaryLight)[names(summaryLight) == "activity_Day"] <- paste0("Activity_Day_",startDay,"_Light")
    
    summaryDT <- cbind(summaryLight, summaryNight[,activity_Night])
    names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",startDay,"_Dark")
    
    #Add data for next days
    if (max(damData$dt[,'experimentDay'])>min(damData$dt[,'experimentDay'])){
      for (n in (startDay+1):max(damData$dt[,'experimentDay'])){
        
        summary <- rejoin(damData$dt[, .(activity_Day = sumNA(activity[experimentDay==n & day_night == "TRUE"])), by = id])
        
        summaryDT <- cbind(summaryDT,summary[,activity_Day])
        names(summaryDT)[length(names(summaryDT))] <- paste0("Activity_Day_",n,"_Light")
        
        summary <- rejoin(damData$dt[, .(activity_Night = sumNA(activity[experimentDay==n & day_night == "FALSE"])), by = id])
        
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
    startTime <- min(damData$dt[,SleepBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(activity_Day = sumNA(activity[SleepBoxPlot_time==startTime])), by = id])
    
    names(summaryDT)[names(summaryDT) == "activity_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,SleepBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(activity_Day = sumNA(activity[SleepBoxPlot_time==n])), by = id])
        
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
    startTime <- min(damData$dt[,ActivityBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(activity_Day = mean(boutActivity[ActivityBoxPlot_time==startTime & boutActivity > 0 & activityBout == TRUE],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "activity_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,ActivityBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(activity_Day = mean(boutActivity[ActivityBoxPlot_time==n & boutActivity > 0 & activityBout == TRUE],na.rm=TRUE)), by = id])
        
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
    startTime <- min(damData$dt[,ActivityBoxPlot_time])
    
    summaryDT <- rejoin(damData$dt[, .(boutTime_Day = mean(activityBoutTime[ActivityBoxPlot_time==startTime & activityBoutTime > 0 ],na.rm=TRUE)), by = id])
    
    names(summaryDT)[names(summaryDT) == "boutTime_Day"] <- paste0("Group",startTime)
    
    if (max(damData$dt[,t])>startTime){
      for (n in (startTime+1):(max(damData$dt[,ActivityBoxPlot_time])-startTime)){
        
        summary <- rejoin(damData$dt[, .(boutTime_Day = mean(activityBoutTime[ActivityBoxPlot_time==n & activityBoutTime > 0 ],na.rm=TRUE)), by = id])
        
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

#Attribute data to variables
ActivityDataSummary <- function(graph = NaN, all=TRUE){
  
  if (all == TRUE){
    if (is.nan(graph) | graph == "activity"){
      withProgress(message = 'Compute activity statistics', value = 0, {
        ActivityData$lightDark <- activitySummary("lightDark")
        incProgress(1/3)
        ActivityData$day <- activitySummary("day")
        incProgress(1/3)
        ActivityData$dayLightDark <- activitySummary("dayLightDark")
        incProgress(1/3)
      })
    }
  }
  
  if (is.nan(graph) | graph == "activity"){
    ActivityData$custom <- activitySummary("custom")}
}
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



#####Update Y axis range
updateYactivity <- function() {
  
  if (input$ActivityboxPlotsTabs == "Activity per light phase"){
    ActivityFigures$lightDark <- ActivityFigures$lightDark + coord_cartesian(ylim = input$yLimitsActivity)
  }
  else{
    if (input$ActivityboxPlotsTabs == "Activity per day"){
      ActivityFigures$day <- ActivityFigures$day + coord_cartesian(ylim = input$yLimitsActivity)
    }
    else{
      if(input$ActivityboxPlotsTabs == "Activity per day and light phase"){
        ActivityFigures$dayLightDark <- ActivityFigures$dayLightDark + coord_cartesian(ylim = input$yLimitsActivity)
      }
      else{
        ActivityFigures$custom <- ActivityFigures$custom + coord_cartesian(ylim = input$yLimitsActivity)
      }
    }
  }
}
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

#Update Labels
updateActivityLabels <- function(){
  
  ActivityFiguresXLabel(input$ActivityBoxXLabel)
  ActivityFiguresYLabel(input$ActivityBoxYLabel)
  
  if (input$ActivityboxPlotsTabs == "Activity per light phase"){
    ActivityFiguresTitles$lightDark <- input$ActivityBoxTitle
  }
  else{
    if (input$ActivityboxPlotsTabs == "Activity per day"){
      ActivityFiguresTitles$day <- input$ActivityBoxTitle
    }
    else{
      if(input$ActivityboxPlotsTabs == "Activity per day and light phase"){
        ActivityFiguresTitles$dayLightDark <- input$ActivityBoxTitle
      }
      else{
        ActivityFiguresTitles$custom <- input$ActivityBoxTitle
      }
    }
  }
  
}
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


##### Figures
#Activity
updateActivityLightDark <- function(){
  ##### Activity per light and dark phases figure #####
  fig <- statisticPlots(ActivityData$lightDark ,input$ActivityPlot,input$ActivityError)+
    labs(title = ActivityFiguresTitles$lightDark, x = ActivityFiguresXLabel(),
         y = ActivityFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
  ActivityFigures$lightDark <- fig
}
updateActivityDay <- function(){
  fig <- statisticPlots(ActivityData$day ,input$ActivityPlot,input$ActivityError)+
    labs(title = ActivityFiguresTitles$day, x = ActivityFiguresXLabel(),
         y = ActivityFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
  ActivityFigures$day <- fig
}
updateActivityDayLightDark <- function() {
  ##### Activity per day and per light and dark phases figure #####
  fig <- statisticPlots(ActivityData$dayLightDark,input$ActivityPlot,input$ActivityError)+
    labs(title = ActivityFiguresTitles$dayLightDark, x = ActivityFiguresXLabel(),
         y = ActivityFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
  ActivityFigures$dayLightDark <- fig
}
updateActivityCustom <- function() {
  fig <- statisticPlots(ActivityData$custom,input$ActivityPlot,input$ActivityError)+
    labs(title = ActivityFiguresTitles$custom, x = ActivityFiguresXLabel(),
         y = ActivityFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  ActivityFigures$custom <- fig
}

updateActivityFigures <- function (all=TRUE) {
  
  if (all == TRUE){
    withProgress(message = 'Activity figures', value = 0, {
      updateActivityLightDark()
      incProgress(1/3)
      updateActivityDay()
      incProgress(1/3)
      updateActivityDayLightDark()
      incProgress(1/3)
    })
  }
  updateActivityCustom()
  
}

# Bout Activity
updateBoutActivityLightDark <- function(){
  ##### Bout activity per light and dark phases figure #####
  fig <- statisticPlots(BoutActivityData$lightDark ,input$ActivityPlot,input$ActivityError)+
    labs(title = BoutActivityFiguresTitles$lightDark, x = BoutActivityFiguresXLabel(),
         y = BoutActivityFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
  BoutActivityFigures$lightDark <- fig
}
updateBoutActivityDay <- function(){
  ##### Bout activity per day #####
  fig <- statisticPlots(BoutActivityData$day ,input$ActivityPlot,input$ActivityError)+
    labs(title = BoutActivityFiguresTitles$day, x = BoutActivityFiguresXLabel(),
         y = BoutActivityFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
  BoutActivityFigures$day <- fig
}
updateBoutActivityDayLightDark <- function(){
  ##### Bout activity per day per light and dark phases figure 
  fig <- statisticPlots(BoutActivityData$dayLightDark ,input$ActivityPlot,input$ActivityError)+
    labs(title = BoutActivityFiguresTitles$dayLightDark, x = BoutActivityFiguresXLabel(),
         y = BoutActivityFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
  BoutActivityFigures$dayLightDark <- fig
}
updateBoutActivityCustom <- function(){
  ##### Bout activity custom figure 
  fig <- statisticPlots(BoutActivityData$custom ,input$ActivityPlot,input$ActivityError)+
    labs(title = BoutActivityFiguresTitles$custom, x = BoutActivityFiguresXLabel(),
         y = BoutActivityFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
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
  fig <- statisticPlots(BoutTimeData$lightDark ,input$ActivityPlot,input$ActivityError)+
    labs(title = BoutTimeFiguresTitles$lightDark, x = BoutTimeFiguresXLabel(),
         y = BoutTimeFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
  BoutTimeFigures$lightDark <- fig
}
updateBoutTimeDay <- function(){
  ##### Bout time per day 
  fig <- statisticPlots(BoutTimeData$day ,input$ActivityPlot,input$ActivityError)+
    labs(title = BoutTimeFiguresTitles$day, x = BoutTimeFiguresXLabel(),
         y = BoutTimeFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
  BoutTimeFigures$day <- fig
}
updateBoutTimeDayLightDark <- function(){
  ##### Bout time per day per light and dark phases figure
  fig <- statisticPlots(BoutTimeData$dayLightDark ,input$ActivityPlot,input$ActivityError)+
    labs(title = BoutTimeFiguresTitles$dayLightDark, x = BoutTimeFiguresXLabel(),
         y = BoutTimeFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
  BoutTimeFigures$dayLightDark <- fig
}
updateBoutTimeCustom <- function(){
  ##### Bout time custom figure 
  fig <- statisticPlots(BoutTimeData$custom ,input$ActivityPlot,input$ActivityError)+
    labs(title = BoutTimeFiguresTitles$custom, x = BoutTimeFiguresXLabel(),
         y = BoutTimeFiguresYLabel())
  
  fig <- whiteBackground(fig, input$titleLetterSize2, input$axisLabelSize2,
                         input$axisNumbersSize2, input$dataLabelSize2)
  
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

  ActivityFiguresTitles$lightDark <- "Activity per light phase"
  ActivityFiguresTitles$day <- "Activity per day"
  ActivityFiguresTitles$dayLightDark <- "Activity per day and per light phase"
  ActivityFiguresTitles$custom <- "Activity"
  ActivityFiguresXLabel("")
  ActivityFiguresYLabel("Activity (counts)")

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
  # 
  #Bout time
  BoutTimeData$lightDark <- NULL
  BoutTimeData$day <- NULL
  BoutTimeData$dayLightDark <- NULL
  BoutTimeData$custom <- NULL

  BoutTimeFigures$lightDark <- NULL
  BoutTimeFigures$day <- NULL
  BoutTimeFigures$dayLightDark <- NULL
  BoutTimeFigures$custom <- NULL
  # 
  BoutTimeFiguresTitles$lightDark <- "Mean bout duration per light phase"
  BoutTimeFiguresTitles$day <- "Mean bout duration per day"
  BoutTimeFiguresTitles$dayLightDark <- "Mean bout duration day and per light phase"
  BoutTimeFiguresTitles$custom <- "Mean bout duration"
  BoutTimeFiguresXLabel("")
  BoutTimeFiguresYLabel("Mean bout duration")
  # 
  # #Clean data presented
  output$ActivitySummary <- NULL
  output$FractalSummary <- NULL
  output$BoutActivitySummary <- NULL
  output$BoutTimeSummary <- NULL
})


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
  
  
  #Clean data presented
  output$BoutActivitySummary <- NULL
  output$BoutTimeSummary <- NULL
  output$SleepTimeSummary <- NULL
  output$SleepLatencySummary <- NULL
  
  ActivityDataSummary()
  updateActivityFigures()
  
  activity <- ActivityData$lightDark
  updateSliderInput(session,'yLimitsActivity',min = 0, max =ceiling(max(activity[,'yPlot'], na.rm =TRUE)), value = c(0,max(activity[,'yPlot'], na.rm =TRUE)))

  #Update bout analysis bin size
  if ((max(damData$dt[,'t'])-min(damData$dt[,'t']))/60 < 180){
    updateSliderInput(session,"ActivityBoutWindow",max = (max(damData$dt[,'t'])-min(damData$dt[,'t']))/60,
                      min = max(damData$dt[,timeDiff],na.rm=TRUE)/60, step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)
  }
  else{
    updateSliderInput(session,"ActivityBoutWindow",value = max(damData$dt[,timeDiff],na.rm=TRUE)/60, max = 60,
                      min = max(damData$dt[,timeDiff],na.rm=TRUE)/60, step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)}
})

#Bout analysis
observeEvent(input$ActivityBoutAnalysis,{
  
  req(damData$dt)
  
  # Analyze bouts
  withProgress(message = 'Analyzing bouts', value = 0, {
    
    #Calculus of the time difference between measurements
    damData$dt[,timeDiff := c(NaN,damData$dt[2:nrow(damData$dt),t]- damData$dt[1:(nrow(damData$dt)-1),t])]
    
    meanTimeDiff <- mean(damData$dt[damData$dt[,timeDiff]>0,timeDiff],na.rm=TRUE) #Minimum bin size
    step <- round(mins(input$ActivityBoutWindow)/meanTimeDiff) #Step for analysis
    
    #Actiivty bouts
    damData$dt[,"movingBout" := !sleep_dam_annotation(damData$dt[,1:3],min_time_immobile = 60*input$ActivityBoutWindow)[,'asleep']]
    
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
      t <- damData$dt[startIndexes[j]:finalIndexes[j],t]
      
      activityBouts <- activityBout_dt[movingBout == TRUE, -"movingBout"]
      
      ### Get start and finish indexes of bouts
      activityT <- activityBouts[,t]
      activityDuration <- activityBouts[,duration]
      
      startRowActivity <- which(t %in% activityT)
      finishRowActivity <- which(t %in% (activityT+activityDuration))

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
  
  damData$dt[,activityBout:=dataActivityBout]
  damData$dt[,activityBoutTime:=dataActivityBoutTime]
  damData$dt[,boutActivity:=as.numeric(dataBoutActivity)]
  
  settings <- settingsTable()
  settings[1,2] <- l_period()
  settings[2,2] <- l_hours()
  settings[4,2] <- input$ActivityBoutWindow
  settingsTable(settings)
  
  ### Update figures
  
  ActivityBoutsData()
  
  updateTabsetPanel(session, "boutActivityPlotsTabs", selected = "Activity per bout per light phase")
  updateTabsetPanel(session, "boutTimePlotsTabs", selected = "Bout duration per light phase")

  #### Update Y range sliders
  boutActivity <- BoutActivityData$lightDark
  updateSliderInput(session,'yLimitsBoutActivity',min = 0, max =ceiling(max(boutActivity[,'yPlot'], na.rm =TRUE)), value = c(0,max(boutActivity[,'yPlot'], na.rm =TRUE)))
  
  boutTime <- BoutTimeData$lightDark
  updateSliderInput(session,'yLimitsBoutTime',min = 0, max =ceiling(max(boutTime[,'yPlot'], na.rm =TRUE)), value = c(0,max(boutTime[,'yPlot'], na.rm =TRUE)))
  
  #Create figures
  updateBoutActivityFigures()
  updateBoutTimeFigures()
})

#Update figures if data is deleted
observeEvent(input$deleteAnimals,{
  
  ActivityDataSummary()
  updateActivityFigures()
  
  activity <- ActivityData$lightDark
  updateSliderInput(session,'yLimitsActivity',min = 0, max =ceiling(max(activity[,'yPlot'], na.rm =TRUE)), value = c(0,max(activity[,'yPlot'], na.rm =TRUE)))
  
  #Update bout analysis bin size
  if ((max(damData$dt[,'t'])-min(damData$dt[,'t']))/60 < 180){
    updateSliderInput(session,"ActivityBoutWindow",max = (max(damData$dt[,'t'])-min(damData$dt[,'t']))/60,
                      min = max(damData$dt[,timeDiff],na.rm=TRUE)/60, step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)
  }
  else{
    updateSliderInput(session,"ActivityBoutWindow",value = max(damData$dt[,timeDiff],na.rm=TRUE)/60, max = 60,
                      min = max(damData$dt[,timeDiff],na.rm=TRUE)/60, step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)}
  
  req(BoutActivityData$lightDark)
  
  ### Update figures bout
  
  ActivityBoutsData()
  
  updateTabsetPanel(session, "boutActivityPlotsTabs", selected = "Activity per bout per light phase")
  updateTabsetPanel(session, "boutTimePlotsTabs", selected = "Bout duration per light phase")
  
  #### Update Y range sliders
  boutActivity <- BoutActivityData$lightDark
  updateSliderInput(session,'yLimitsBoutActivity',min = 0, max =ceiling(max(boutActivity[,'yPlot'], na.rm =TRUE)), value = c(0,max(boutActivity[,'yPlot'], na.rm =TRUE)))
  
  boutTime <- BoutTimeData$lightDark
  updateSliderInput(session,'yLimitsBoutTime',min = 0, max =ceiling(max(boutTime[,'yPlot'], na.rm =TRUE)), value = c(0,max(boutTime[,'yPlot'], na.rm =TRUE)))
  
  #Create figures
  updateBoutActivityFigures()
  updateBoutTimeFigures()
  
})
observeEvent(input$deleteInactivity,{
  
  ActivityDataSummary()
  updateActivityFigures()
  
  activity <- ActivityData$lightDark
  updateSliderInput(session,'yLimitsActivity',min = 0, max =ceiling(max(activity[,'yPlot'], na.rm =TRUE)), value = c(0,max(activity[,'yPlot'], na.rm =TRUE)))
  
  #Update bout analysis bin size
  if ((max(damData$dt[,'t'])-min(damData$dt[,'t']))/60 < 180){
    updateSliderInput(session,"ActivityBoutWindow",max = (max(damData$dt[,'t'])-min(damData$dt[,'t']))/60,
                      min = max(damData$dt[,timeDiff],na.rm=TRUE)/60, step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)
  }
  else{
    updateSliderInput(session,"ActivityBoutWindow",value = max(damData$dt[,timeDiff],na.rm=TRUE)/60, max = 60,
                      min = max(damData$dt[,timeDiff],na.rm=TRUE)/60, step = max(damData$dt[,timeDiff],na.rm=TRUE)/60)}
  
  req(BoutActivityData$lightDark)
  
  ### Update figures bout
  
  ActivityBoutsData()
  
  updateTabsetPanel(session, "boutActivityPlotsTabs", selected = "Activity per bout per light phase")
  updateTabsetPanel(session, "boutTimePlotsTabs", selected = "Bout duration per light phase")
  
  #### Update Y range sliders
  boutActivity <- BoutActivityData$lightDark
  updateSliderInput(session,'yLimitsBoutActivity',min = 0, max =ceiling(max(boutActivity[,'yPlot'], na.rm =TRUE)), value = c(0,max(boutActivity[,'yPlot'], na.rm =TRUE)))
  
  boutTime <- BoutTimeData$lightDark
  updateSliderInput(session,'yLimitsBoutTime',min = 0, max =ceiling(max(boutTime[,'yPlot'], na.rm =TRUE)), value = c(0,max(boutTime[,'yPlot'], na.rm =TRUE)))
  
  #Create figures
  updateBoutActivityFigures()
  updateBoutTimeFigures()
  
})

########################### Change labels ###############################


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
    
    print(conditions)

    for (k in 1: nrow(activity)){
      if (activity[k,'xPlot']==Label[i]){
        activity[k,'xPlot'] <- v
      }
    }
    return(activity)
  }
  
  if (input$ActivityboxPlotsTabs == "Activity per light phase"){
    ActivityData$lightDark <- changeLabels(ActivityData$lightDark)
    updateActivityLightDark()
  }
  else{
    if (input$ActivityboxPlotsTabs == "Activity per day"){
      ActivityData$day <- changeLabels(ActivityData$day)
      updateActivityDay()
    }
    else{
      if(input$ActivityboxPlotsTabs == "Activity per day and light phase"){
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

#################### Update statistical data presented ###################

#Activity table
observe({
  req(nrow(damData$dt)>0)
  req(nrow(ActivityData$lightDark)>0)
  
  if (input$ActivityboxPlotsTabs == "Activity per light phase"){
    activity <- ActivityData$lightDark
  }
  else{
    if (input$ActivityboxPlotsTabs == "Activity per day"){
      activity <- ActivityData$day
    }
    else{
      if(input$ActivityboxPlotsTabs == "Activity per day and light phase"){
        activity <- ActivityData$dayLightDark
      }
      else{
        activity <- ActivityData$custom
      }
    }
  }
  
  updateSliderInput(session,'yLimitsActivity',min = 0, max =ceiling(max(activity[,'yPlot'], na.rm =TRUE)), value = c(0,max(activity[,'yPlot'])))
  
  output$ActivitySummary <- DT::renderDataTable(statisticsReport(activity),
                                                escape = FALSE, selection = 'none', 
                                                editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
  
})

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


############################### PLOTS ###################################

#Update graphs
observeEvent(input$updateActivityStatistics,{
  
  req(damData$dt)
  
  updateActivityLabels()
  updateActivityBoutsLabels()
  
  damData$dt[, 'ActivityBoxPlot_time' := floor(damData$dt[,'t']/(input$ActivityGroupBoxTime * l_period()*3600))]
  
  settings <- settingsTable()
  settings[1,2] <- l_period()
  settings[2,2] <- l_hours()
  settingsTable(settings)
  
  withProgress(message = 'Update data and graphs', value = 0, {
    
    ActivityDataSummary()
    incProgress(0.5)
    updateActivityFigures()
    incProgress(0.5)
  })
  
  updateYactivity()
  
  #Periodic representation update
  updateFigures()
  
  req(nrow((BoutActivityData$lightDark))>0)
  updateBoutActivityFigures()
  updateBoutTimeFigures()
  
  updateYBoutActivity()
  updateYBoutTime()
  
  
  
})

#Update text fields
observeEvent(input$ActivityboxPlotsTabs,{
  
  #Update activity text fields
  updateTextInput(session, "ActivityBoxXLabel", value = ActivityFiguresXLabel())
  updateTextInput(session, "ActivityBoxYLabel", value = ActivityFiguresYLabel())
  
  if (input$ActivityboxPlotsTabs == "Activity per light phase"){
    updateTextInput(session,"ActivityBoxTitle", value = ActivityFiguresTitles$lightDark)
  }
  else{
    if (input$ActivityboxPlotsTabs == "Activity per day"){
      updateTextInput(session,"ActivityBoxTitle", value = ActivityFiguresTitles$day)
    }
    else{
      if(input$ActivityboxPlotsTabs == "Activity per day and light phase"){
        updateTextInput(session,"ActivityBoxTitle", value = ActivityFiguresTitles$dayLightDark)
      }
      else{
        updateTextInput(session,"ActivityBoxTitle", value = ActivityFiguresTitles$custom)
      }
    }
  }
  
  
})
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

#Activity box plots
output$activityDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  validate(
    need (!is.null(ActivityFigures$lightDark),"No graph")
  )
  
  fig <- ActivityFigures$lightDark
  
  return(fig)
  
  
  
},width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)
output$activityPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  validate(
    need (!is.null(ActivityFigures$day),"No graph")
  )
  
  fig <- ActivityFigures$day
  
  
  return(fig)
  
}, width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)
output$activityDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  validate(
    need (!is.null(ActivityFigures$dayLightDark),"No graph")
  )
  
  fig <- ActivityFigures$dayLightDark
  
  
  return(fig)
  
}, width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)
output$activityCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  req(input$ActivityGroupBoxTime)
  req(input$ActivityBoxTime)
  
  validate(
    need (!is.null(ActivityFigures$custom),"No graph")
  )
  
  fig <- ActivityFigures$custom
  
  
  return(fig)
  
},width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)

#Bout activity box plots
output$boutActivityDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData$lightDark)>0, "")
  )
  
  fig <- BoutActivityFigures$lightDark
  
  return(fig)
  
  
  
},width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)
output$boutActivityPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData$lightDark)>0, "")
  )
  
  fig <- BoutActivityFigures$day
  
  
  return(fig)
  
}, width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)
output$boutActivityDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData$lightDark)>0, "")
  )
  
  fig <- BoutActivityFigures$dayLightDark
  
  
  return(fig)
  
}, width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)
output$boutActivityCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutActivityData$lightDark)>0, "")
  )
  
  req(input$ActivityGroupBoxTime)
  req(input$ActivityBoxTime)
  
  fig <- BoutActivityFigures$custom
  
  return(fig)
  
},width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)


#Bout time box plots
output$boutTimeDayNight <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData$lightDark)>0, "")
  )
  
  fig <- BoutTimeFigures$lightDark
  
  
  return(fig)
  
  
  
}, width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)
output$boutTimePerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData$lightDark)>0, "")
  )
  
  fig <- BoutTimeFigures$day
  
  
  return(fig)
  
}, width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)
output$boutTimeDayNightPerDay <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData$lightDark)>0, "")
  )
  
  fig <- BoutTimeFigures$dayLightDark
  
  return(fig)
  
}, width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)
output$boutTimeCustomized <- renderPlot({
  
  validate(
    need (nrow(damData$dt)>0,"No data selected"),
    need (nrow(BoutTimeData$lightDark)>0, "")
  )
  req(input$ActivityGroupBoxTime)
  req(input$ActivityBoxTime)
  
  fig <- BoutTimeFigures$custom
  
  return(fig)
  
}, width = function() input$ActivityWidth,
height = function() input$ActivityHeight,
res = 96)

######################## SAVE FIGS AND DATA #############################


# #save Figures
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
      
      if (input$ActivityboxPlotsTabs == "Activity per light phase"){
        fig <- ActivityFigures$lightDark
      }
      else{
        if (input$ActivityboxPlotsTabs == "Activity per day"){
          fig <- ActivityFigures$day
        }
        else{
          if(input$ActivityboxPlotsTabs == "Activity per day and light phase"){
            fig <- ActivityFigures$dayLightDark
          }
          else{
            fig <- ActivityFigures$custom
          }
        }
      }
      
      ggsave(filename = file, plot = fig,
             width = round(input$SleepWidth/97), height= round(input$SleepHeight/97))
    })
  
})
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
         width = round(input$ActivityWidth/97), height= round(input$ActivityHeight/97))
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
         width = round(input$ActivityWidth/97), height= round(input$ActivityHeight/97))
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
      
      wb <- createWorkbook(type="xlsx")
      
      if (input$ActivityboxPlotsTabs == "Activity per light phase"){
        data <- ActivityData$lightDark
      }
      else{
        if (input$ActivityboxPlotsTabs == "Activity per day"){
          data <- ActivityData$day
        }
        else{
          if(input$ActivityboxPlotsTabs == "Activity per day and light phase"){
            data <- ActivityData$dayLightDark
          }
          else{
            data <- ActivityData$custom
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




