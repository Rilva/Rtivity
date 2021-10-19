
###########################  FUNCTIONS #################################

#Change symbols for Excel sheets names
checkSymbolsExcel <- function(x){
  
  x <- gsub('[[:punct:]]','.',x)
  
  return(x)
}

#Cbind fill
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

#Convert vector of seconds into vector of days
sec_to_day <- function(t){
  
  result <- c()
  for (i in 1:length(t)){
    result <- c(result,signif(t[i]/86400, digits = 3))
  }
  return(result)
}

#Convert vector of seconds into vector of hours
sec_to_hour <- function(t){
  
  result <- c()
  for (i in 1:length(t)){
    result <- c(result,signif(t[i]/3600, digits = 3))
  }
  return(result)
}

#Convert vector of seconds into vector of minutes
sec_to_min <- function(t){
  
  result <- c()
  for (i in 1:length(t)){
    result <- c(result,signif(t[i]/60, digits = 3))
  }
  return(result)
}

#Remove background from plots
whiteBackground <- function(fig){
  
  if (input$pages == "Periodic representations"){
    titleLetter <-input$titleLetterSize
    axisLetter <- input$axisLabelSize
    axisNumberLetter <- input$axisNumbersSize
    legendLetter <- input$dataLabelSize
  }else{
    if (input$pages == "Activity and sleep statistics"){
      titleLetter <-input$titleLetterSize2
      axisLetter <- input$axisLabelSize2
      axisNumberLetter <- input$axisNumbersSize2
      legendLetter <- input$dataLabelSize2
    }
    else{
      if (input$pages == "Bouts statistics"){
        titleLetter <-input$titleLetterSize3
        axisLetter <- input$axisLabelSize3
        axisNumberLetter <- input$axisNumbersSize3
        legendLetter <- input$dataLabelSize3
      }
      else{
        titleLetter <- 16
        axisLetter <- 14
        axisNumberLetter <- 12
        legendLetter <- 13
      }
    }
  }
  
  fig <- fig +
    theme(axis.line = element_line(colour = "black", size = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.key = element_rect(fill = NA),
          axis.title.x = element_text(margin = margin(10,10,10,10)),
          axis.title.y = element_text(margin = margin(10,10,10,10)),
          legend.text = element_text(size = legendLetter),
          axis.text = element_text(size = axisNumberLetter, face  ="bold", color = "black"),
          axis.title = element_text(size = axisLetter, face  ="bold"),
          plot.title = element_text (size = titleLetter, face  ="bold", margin = margin(b = 20, t = 10), hjust = 0.5 ))
          
  return(fig)
}

#x Axis of continuous graphs
xAxis <- function (fig,graph,xlabel){
  
  req(input$xTime)
  req(input$x0)
  req(is.numeric(input$ticks_distance))
  
  # If no labels have been inputted, use the labels imposed by the scale_x functions
  
  maximumTime <- max(damData$dt[,periodT])
  minimumTime <- min(damData$dt[,periodT])

  #Update x axis according to the time in analysis
  
  if (graph == "periodogram"){
    fig <- fig + scale_x_continuous(name = xlabel, breaks = seq(from = input$minPer*3600, to = input$maxPer*3600, by = input$ticks_distance*3600), labels = sec_to_hour)
  }
  else{
    if (input$xTime == "days"){
      max_days <- ceiling(maximumTime/86400)
      updateSliderInput(session,'x0', max = floor(max_days/2))
      fig <- fig + scale_x_continuous(name = xlabel, breaks = seq(from = input$tick0*86400, to = max_days*86400, by = input$ticks_distance*86400), labels = sec_to_day)
    }
    if (input$xTime == "hour"){
      max_hour <- floor(maximumTime/3600)
      updateSliderInput(session,'x0', max = round(max_hour/2))
      fig <- fig + scale_x_continuous(name = xlabel, breaks = seq(from = input$tick0*3600, to = max_hour*3600, by = input$ticks_distance*3600), labels = sec_to_hour)
    }
    if (input$xTime == "min"){
      max_min <- floor(maximumTime/60)
      updateSliderInput(session,'x0', max = round(max_min/2))
      fig <- fig + scale_x_continuous(name = xlabel, breaks = seq(from = input$tick0*60, to = max_min*60, by = input$ticks_distance*60), labels = sec_to_min)
    }
    if (input$xTime == "sec"){
      max_sec <- floor(maximumTime)
      updateSliderInput(session,'x0', max = round(max_sec/2))
      fig <- fig + scale_x_continuous(name = xlabel, breaks = seq(from = input$tick0, to = max_sec, by = input$ticks_distance))
    }
  }
  
  return (fig)
}

#y Axis of continuous graphs
yAxis <- function(fig,graph, ylabel){
  
  #Graph y label
  fig <- fig + ylab(ylabel)
  
  if (graph == "chronogram" | graph == "chronogram24h" | graph == "cumActivity"){
    fig <- fig + coord_cartesian(ylim = c(input$y_min,input$y_max))}
  if (graph == "sleep"){
    fig <- fig + coord_cartesian(ylim = c(0,1))}
  
  
  return(fig)
}

#Update period time according to changes in x labels
update_periodT <- function(){
  
  if (is.numeric(input$x0)){

    if (input$xTime == "days"){
      sumTime <- days(input$x0)}
    if (input$xTime == "hour"){
      sumTime <- hours(input$x0)}
    if (input$xTime == "min"){
      sumTime <- mins(input$x0)}
    if (input$xTime == "sec"){
      sumTime <- input$x0}
    
    if(input$dataTime=="normal"){
      damData$dt[,'periodT' := (damData$dt[,'t']-min(damData$dt[,'t'])+sumTime)]
    }
    else{
      damData$dt[,'periodT' := damData$dt[,'t']+sumTime]
    }
  }
}

#Time continuous plots
plotLabels <- function(fig,graph){
  
  #Light dark annotations
  if (input$lightDark == TRUE & graph != "periodogram"){
    fig <- fig + stat_ld_annotations(l_duration = hours(floor(l_hours())), period = hours(l_period()))
  }
  
  #Title, x and Y labels
  x <- FiguresXlabels$ActivityAndSleep
  y <- FiguresYlabels$activity
  if (graph == "actogram"){
    title <- FiguresTitles$actogram
  }
  else{
    if (graph == "doublePlot"){
      title <- FiguresTitles$doublePlot
      y <- FiguresYlabels$doublePlot
    }
    else{
      if (graph == "chronogram"){
        title <- FiguresTitles$chronogram
      }
      else{
        if (graph == "chronogram24h"){
          title <- FiguresTitles$chronogram24h
        }
        else{
          if (graph == "periodogram"){
            title <- FiguresTitles$periodogram
            x <- FiguresXlabels$periodogram
            y <- FiguresYlabels$periodogram
          }
          else{
            if (graph == "cumActivity"){
              title <- FiguresTitles$cumActivity
              y <- FiguresYlabels$cumActivity
            }
            else{
              if (graph == "sleep"){
                title <- FiguresTitles$sleep
                y <- FiguresYlabels$sleep
              }
            }
          }
        }
      }
    }
  }
  
  #X and Y axis update
  fig <- xAxis(fig,graph,x)
  fig <- yAxis(fig,graph,y)
  
  fig <- fig + ggtitle(title)
  
  if (input$individualPlot == 'individual' & graph != "actogram"){
    fig <- fig + facet_grid(~ interaction(labels,order)) }
  
  #Remove background from figures
  fig <- whiteBackground(fig)
  
  return(fig)
}

#Update figures
updateActogram  <- function () {
  
  ##### Actogram Figure #####
  fig <- ggetho(damData$dt,aes(x = periodT, y = Data,z = activity),
                summary_FUN = sum,summary_time_window = mins(binSize()))
  
  if (input$tile_bar == "bar"){
    fig <- fig + stat_bar_tile_etho()
  }
  else{
    fig <- fig + stat_tile_etho()
  }
  
  title <- "Actogram"
  x <-  paste0("Time (",input$xTime,")")
  y <- paste("Mean activity per",binSize(), "minutes")
  
  fig <- plotLabels(fig, "actogram")
  
  Figures$actogram <- fig
}
updateDoublePlot <- function() {
  
  ##### Double-plot Actogram Figure #####
  fig <- ggetho(damData$dt,aes(x = t, z = activity),multiplot = 2, multiplot_period = hours(l_period()), summary_FUN = sum,
                summary_time_window = mins(binSize())) + facet_wrap( ~ Data)
  
  if (input$tile_bar == "bar"){
    fig <- fig + stat_bar_tile_etho()
  }
  else{
    fig <- fig + stat_tile_etho()
  }
  
  title <- "Double Plot Actogram"
  x <-  paste0("Time (",input$xTime,")")
  y <- paste("Mean activity per",binSize(), "minutes")
  
  fig <- plotLabels(fig, "doublePlot")
  
  Figures$doublePlot <- fig
}
updateChronogram <- function() {
  
  ##### Chronogram Figure #####
  fig <- ggetho(damData$dt,aes(x = periodT, y = activity, colour=Data, linetype = Data),summary_FUN = sum,
                summary_time_window = mins(binSize())) + stat_pop_etho()
  
  #Change graphs aestethics
  fig <- fig + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])
  
  #Add errors
  if (input$errorChronogram == "NA"){
    fig <- fig + scale_fill_manual(values = alpha(rep(NA, 100)), na.value = NA)
  }
  else{
    fig <- fig + stat_pop_etho(method = input$errorChronogram) + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  
  title <- "Full chronogram"
  x <-  paste0("Time (",input$xTime,")")
  y <- paste("Mean activity per",binSize(), "minutes")
  
  fig <- plotLabels(fig,"chronogram")
  
  Figures$chronogram <- fig
}
updateChronogram24h <- function(){
  
  ##### Chronogram of 1 day Figure #####
  #Divide data per day, otherwise the representation will represent the sum of the data from all days
  n <- as.numeric(ceiling((max(damData$dt[,'t'])-min(damData$dt[,'t']))/(l_period()*3600)))
  damData$dt[,'activity24h':= damData$dt[,'activity']/n]
  
  fig <- ggetho(damData$dt,aes(x = periodT, y = activity24h, colour=Data, linetype = Data),summary_FUN = sum,
                summary_time_window = mins(binSize()), time_wrap = hours(l_period()))+stat_pop_etho()
  
  #Change graph aestethics
  fig<- fig + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])
  
  #Add errors
  if (input$errorChronogram == "NA"){
    fig <- fig + scale_fill_manual(values = alpha(rep(NA, 100)), na.value = NA)
  }
  else{
    fig <- fig + stat_pop_etho(method = input$errorChronogram) + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  
  title <- "Average LD cycle chronogram"
  x <-  paste0("Time (",input$xTime,")")
  y <- paste("Mean activity per",binSize(), "minutes")
  
  fig <- plotLabels(fig,"chronogram24h")
  
  Figures$chronogram24h <- fig
  
}
updatePeriodogram <- function() {
  
  ##### Periodogram #####
  #Represent the periodogram according to the selected function
  
  req(max(damData$dt[,t])-min(damData$dt[,t]) > hours(l_period()))
  if (input$perFun=='chi-sq'){
    per_dt <- periodogram(activity, damData$dt, FUN = chi_sq_periodogram,
                          period_range = c(hours(input$minPer), hours(input$maxPer)), time_resolution = hours(input$periodogramValue))
  }
  if (input$perFun=='fourier'){
    per_dt <- periodogram(activity, damData$dt, FUN = fourier_periodogram,
                          period_range = c(hours(input$minPer), hours(input$maxPer)))
  }
  if (input$perFun=='ls'){
    per_dt <- periodogram(activity, damData$dt, FUN = ls_periodogram,
                          period_range = c(hours(input$minPer), hours(input$maxPer)),oversampling = input$periodogramValue)
  }
  
  per_dt <- find_peaks(per_dt)
  
  #Create periodogram
  fig<- ggperio(per_dt, aes(period, power, colour=Data, linetype = Data)) +
    stat_pop_etho() +
    scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])
  
  
  if (input$showPeriods){
    fig <- fig + geom_peak(peak_rank = 1)
  }

  #Add errors
  if (input$errorChronogram == "NA"){
    fig <- fig + scale_fill_manual(values = alpha(rep(NA, 100)), na.value = NA)
  }
  else{
    fig <- fig + stat_pop_etho(method = input$errorChronogram) + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  
  title <- "Periodogram"
  x <-  "Period (h)"
  y <- "Power"
  
  fig <- plotLabels(fig,"periodogram")
  
  PeriodData(find_peaks(per_dt))
  
  Figures$periodogram <- fig
}
updateCumActivity<- function(){
  
  ##### Cumulative activity #####
  #Create cumulative activity graph
  fig <- ggetho(damData$dt,aes(x = periodT, y = auc, colour=Data, linetype = Data),summary_FUN = max,
                summary_time_window = mins(binSize())) + stat_pop_etho()
  
  fig <- fig + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])
  
  #Add errors
  if (input$errorChronogram == "NA"){
    fig <- fig + scale_fill_manual(values = alpha(rep(NA, 100)), na.value = NA)
  }
  else{
    fig <- fig + stat_pop_etho(method = input$errorChronogram) + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  
  title <- "Cumulative activity"
  x <-  paste0("Time (",input$xTime,")")
  y <- "Cumulative activity"
  
  fig <- plotLabels(fig,"cumActivity")
  
  Figures$cumActivity <- fig
}
updateSleep <- function(){
  
  ##### Sleep chronogram #####
  
  fig <- ggetho(damData$dt,aes(x = periodT, y = asleep, colour=Data, linetype = Data),summary_FUN = mean,
                summary_time_window = mins(binSize())) + stat_pop_etho()
  
  fig <- fig + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])
  
  #Add errors
  if (input$errorChronogram == "NA"){
    fig <- fig + scale_fill_manual(values = alpha(rep(NA, 100)), na.value = NA)
  }
  else{
    fig <- fig + stat_pop_etho(method = input$errorChronogram) + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  
  title <- "Sleep chronogram"
  x <- paste0("Time (",input$xTime,")")
  y <- paste("Mean sleep time ratio per",binSize(),"minutes")
  
  fig <- plotLabels(fig,"sleep")
  
  Figures$sleep <- fig
}

#Create and update all figures
updateFigures <- function(){
  
  withProgress(message = 'Computing periodic graphs', value = 0, {
    incProgress(1/7) #Increment of progress bar
    updateActogram()
    
    incProgress(1/7) #Increment of progress bar
    updateDoublePlot()
    
    incProgress(1/7) #Increment of progress bar
    updateChronogram()
    
    incProgress(1/7) #Increment of progress bar
    updateChronogram24h()
    
    incProgress(1/7) #Increment of progress bar
    updatePeriodogram()
    
    incProgress(1/7) #Increment of progress bar
    updateCumActivity()
    
    incProgress(1/7) #Increment of progress bar
    updateSleep()
  })
}

updateLabels <- function(tab){
  
  if (tab == "Actogram"){
    FiguresTitles$actogram <- input$graphTitle
    FiguresXlabels$ActivityAndSleep <- input$xLabel
    FiguresYlabels$activity <- input$yLabel
  }
  else{
    if (tab == "Double Plot Actogram"){
      FiguresTitles$doublePlot <- input$graphTitle
      FiguresXlabels$ActivityAndSleep <- input$xLabel
      FiguresYlabels$doublePlot <- input$yLabel
    }
    else{
      if (tab == "Full chronogram"){
        FiguresTitles$chronogram <- input$graphTitle
        FiguresXlabels$ActivityAndSleep <- input$xLabel
        FiguresYlabels$activity <- input$yLabel
      }
      else{
        if (tab == "Average LD cycle chronogram"){
          FiguresTitles$chronogram24h <- input$graphTitle
          FiguresXlabels$ActivityAndSleep <- input$xLabel
          FiguresYlabels$activity <- input$yLabel
        }
        else{
          if (tab == "Periodogram"){
            FiguresTitles$periodogram <- input$graphTitle
            FiguresXlabels$periodogram <- input$xLabel
            FiguresYlabels$periodogram <- input$yLabel
          }
          else{
            if (tab == "Cumuulative Activity"){
              FiguresTitles$cumActivity <- input$graphTitle
              FiguresXlabels$ActivityAndSleep <- input$xLabel
              FiguresYlabels$cumActivity <- input$yLabel
            }
            else{
              if (tab == "Sleep Chronogram"){
                FiguresTitles$sleep <- input$graphTitle
                FiguresXlabels$ActivityAndSleep <- input$xLabel
                FiguresYlabels$sleep <- input$yLabel
              }
            }
          }
        }
      }
    }
  }
}

#Check if filename is still present
saveName <- function(dir, name){
  
  patt <- tools::file_path_sans_ext(name)
  ext <- tools::file_ext(name)
  
  files <- list.files(path = dir, pattern = patt)
  
  if(length(files)>0){
    n=length(files)
    for (i in 1:length(files)){
      if (tools::file_ext(files[i])!=ext){
        n = n-1
      }
    }
    if (n>0){
      name <- paste0(patt,"_",n,".",ext)
    }
  }
  
  return(name)
}


############################  VARIABLES ###############################

### Moving average time length
movingAverage <- reactiveVal()


#Graph colors
colorPallete <- c("black", "red","blue","darkgreen","orange","yellow", "purple","grey","darksalmon","steelblue","springgreen","coral",
                  "black", "red","blue","darkgreen","orange","yellow", "purple","grey","darksalmon","steelblue","springgreen","coral",
                  "black", "red","blue","darkgreen","orange","yellow", "purple","grey","darksalmon","steelblue","springgreen","coral",
                  "black", "red","blue","darkgreen","orange","yellow", "purple","grey","darksalmon","steelblue","springgreen","coral")

#Graphs aestethics variables
graphsAestethics <- reactiveValues(df = data.frame(Label = character(), lineType = character(), lineColor = character()))
changeAestethics <- reactiveValues(selectedCondition = "None", selectedLine  = character(), selectedColor = character())


Figures <- reactiveValues(actogram = NULL, doublePlot = NULL, chronogram = NULL, chronogram24h = NULL, periodogram = NULL, cumActivity = NULL, sleep = NULL)
FiguresTitles <- reactiveValues(actogram = "Actogram", doublePlot = "Double Plot Actogram", chronogram = "Full chronogram", chronogram24h = "Average LD cycle chronogram", periodogram = "Periodogram", cumActivity = "Cumulative Activity", sleep = "Sleep Chronogram")
FiguresXlabels <- reactiveValues(ActivityAndSleep = paste0("Time (days)"), periodogram = "Period (h)")
FiguresYlabels <- reactiveValues(activity = paste("Activity per 60 minutes"), doublePlot = "Period", periodogram = "Power", cumActivity = "Cumulative Activity", sleep = paste("Sleep ratio per 60 minutes"))

PeriodicData <- reactiveValues(activity = NULL, periodogram = NULL, cumActivity = NULL, sleep = NULL)
PeriodicStatistics <- reactiveValues(activity = NULL, periodogram = NULL, cumActivity = NULL, sleep = NULL)

#Periodogram data
PeriodData <- reactiveVal()

# Bin size
binSize <- reactiveVal()


##################### Restart variables #####################
observeEvent(input$files,{
  
  #Get directory
  validate(
    need(nrow(input$files)>0,""))
  
  graphsAestethics$df <- data.frame(Label = character(), lineType = character(), lineColor = character())
  
  PeriodicData$activity <- NULL
  PeriodicData$periodogram <- NULL
  PeriodicData$cumActivity <- NULL
  PeriodicData$sleep <- NULL
  
  PeriodicStatistics$activity <- NULL
  PeriodicStatistics$periodogram <- NULL
  PeriodicStatistics$cumActivity <- NULL
  PeriodicStatistics$sleep <- NULL
  
  PeriodData(NULL)
  binSize(NULL)
})

######################### Get figures to plot ###############################

#Change DAM data if sleep time changes
observeEvent(input$sleepAnalysis,{
  
  req(damData$dt)

  #get DAM data
  damData$dt[,"asleep" := sleep_dam_annotation(damData$dt[,1:3],min_time_immobile = 60*input$sleepTime)[,'asleep']]
  
  binSize(input$movingAverage)
  ActivityRepresentationsData()
  
  updateFigures()
  
  updateSliderInput(session,"sleepTime2",value = input$sleepTime)
  
  settings <- settingsTable()
  settings[1,2] <- l_period()
  settings[2,2] <- l_hours()
  settings[3,2] <- input$movingAverage
  settings[5,2] <- input$sleepTime
  settingsTable(settings)
  
  req(SleepFigures$lightDark)
  
  ActivityAndSleepData(graph='sleep')
  updateSleepFigures()
  
  req(nrow((BoutSleepTimeData$lightDark))>0)
  SleepBoutsData()
  updateBoutSleepTimeFigures()
  updateBoutSleepLatencyFigures()

})

#Change periodogram data
observeEvent(input$periodogramAnalysis,{
  req(damData$dt)
  
  settings <- settingsTable()
  settings[6,2] <- input$perFun
  settings[7,2] <- input$periodogramValue
  settings[8,2] <- input$minPer
  settings[9,2] <- input$maxPer
  settingsTable(settings)
  
  withProgress(message = 'Periodogram analysis analysis', value = 0, {
    updatePeriodogram()
    PeriodicRepresentationsData()
  })
})

observeEvent(input$tabs,{

  req(nrow(damData$dt)>0)
  
  x <- FiguresXlabels$ActivityAndSleep
  y <- FiguresYlabels$activity
  if (input$tabs == "Actogram"){
    title <- FiguresTitles$actogram
  }
  else{
    if (input$tabs == "Double plot actogram"){
      title <- FiguresTitles$doublePlot
      y <- FiguresYlabels$doublePlot
    }
    else{
      if (input$tabs == "Full chronogram"){
        title <- FiguresTitles$chronogram
      }
      else{
        if (input$tabs == "Average LD cycle chronogram"){
          title <- FiguresTitles$chronogram24h
        }
        else{
          if (input$tabs == "Periodogram"){
            title <- FiguresTitles$periodogram
            x <- FiguresXlabels$periodogram
            y <- FiguresYlabels$periodogram
          }
          else{
            if (input$tabs == "Cumulative Activity"){
              title <- FiguresTitles$cumActivity
              y <- FiguresYlabels$cumActivity
            }
            else{
              if (input$tabs == "Sleep Chronogram"){
                title <- FiguresTitles$sleep
                y <- FiguresYlabels$sleep
              }
            }
          }
        }
      }
    }
  }
  updateTextInput(session,"graphTitle", value=title)
  updateTextInput(session,"yLabel", value=y)
  updateTextInput(session,"xLabel", value=x)
})

########################## GRAPHICS AESTETHICS ##############################

#Get user input aestethics
observe({

  req(Conditions$df)

  if(nrow(Conditions$df)>0){
    labels <- unique(Conditions$df[,5])

    changedLabel <- FALSE
    if (nrow(graphsAestethics$df)>0){
      for (i in 1:min(c(nrow(graphsAestethics$df),length(labels)))){
        if (labels[i] != graphsAestethics$df[i,'Label']){
          changedLabel <- TRUE
          break
        }
      }}

    if (changedLabel == FALSE){
      updateSelectInput(session,"addedConditions","Choose condition", choices = c("None",labels), selected = input$addedConditions)
      updateSelectInput(session,"addedConditions2","Choose condition", choices = c("None",labels), selected = input$addedConditions2)
      updateSelectInput(session,"addedConditions3","Choose condition", choices = c("None",labels), selected = input$addedConditions3)}
    else{
      updateSelectInput(session,"addedConditions","Choose condition", choices = c("None",labels), selected = "None")
      updateSelectInput(session,"addedConditions2","Choose condition", choices = c("None",labels), selected = "None")
      updateSelectInput(session,"addedConditions3","Choose condition", choices = c("None",labels), selected = "None")
    }


    if (nrow(graphsAestethics$df)<length(labels)){
      for (i in seq((length(labels)-nrow(graphsAestethics$df)),1, -1)){
        add <- data.frame (Label = labels[length(labels)], lineType = "solid", lineColor = colorPallete[length(labels)-i+1])
        graphsAestethics$df<- rbind(graphsAestethics$df,add)
      }
    }
    else{
      if(nrow(graphsAestethics$df)>length(labels)){
        for (i in seq(nrow(graphsAestethics$df),1,-1)){
          if (is.na(match(graphsAestethics$df[i,'Label'],labels))){
            graphsAestethics$df <- graphsAestethics$df[-i,]
          }
        }
      }
    }
    graphsAestethics$df[,'Label'] <- labels
  }
  else{
    updateSelectInput(session,"addedConditions","Choose condition", choices = c("None"), selected = "None")
    updateSelectInput(session,"addedConditions2","Choose condition", choices = c("None",labels), selected = "Nope")
    updateSelectInput(session,"addedConditions3","Choose condition", choices = c("None",labels), selected = "Nope")
    graphsAestethics$df <- data.frame(Label = character(), lineType = character(), lineColor = character())
  }
})

#Update color palettes
observeEvent(input$pallete,{

  updateColourInput(session,'linecolor', palette=input$pallete)
  updateColourInput(session,'linecolor2', palette=input$pallete)
  updateColourInput(session,'linecolor3', palette=input$pallete)
  updateRadioButtons(session,'pallete2',selected = input$pallete)
  updateRadioButtons(session,'pallete3',selected = input$pallete)
})
observeEvent(input$pallete2,{

  updateColourInput(session,'linecolor', palette=input$pallete2)
  updateColourInput(session,'linecolor2', palette=input$pallete2)
  updateColourInput(session,'linecolor3', palette=input$pallete2)
  updateRadioButtons(session,'pallete',selected = input$pallete2)
  updateRadioButtons(session,'pallete3',selected = input$pallete2)
})
observeEvent(input$pallete3,{

  updateColourInput(session,'linecolor', palette=input$pallete3)
  updateColourInput(session,'linecolor2', palette=input$pallete3)
  updateColourInput(session,'linecolor3', palette=input$pallete3)
  updateRadioButtons(session,'pallete',selected = input$pallete3)
  updateRadioButtons(session,'pallete2',selected = input$pallete3)
})


#Update linetype and color fields after condition selection
observeEvent(changeAestethics$selectedCondition,{

  labels <- unique(Conditions$df[,5])

  if (changeAestethics$selectedCondition != "None"){
    n = match(changeAestethics$selectedCondition,labels)

    updateSelectInput(session,"linetype", selected = graphsAestethics$df[n,'lineType'])
    updateColourInput(session,"linecolor", value = graphsAestethics$df[n,'lineColor'], allowTransparent = TRUE)
    updateColourInput(session,"linecolor2", value = graphsAestethics$df[n,'lineColor'], allowTransparent = TRUE)
    updateColourInput(session,"linecolor3", value = graphsAestethics$df[n,'lineColor'], allowTransparent = TRUE)}
})

#Obtain linetype and color
observeEvent(changeAestethics$selectedLine,{

  labels <- unique(Conditions$df[,5])

  if (changeAestethics$selectedCondition != "None"){
    n = match(changeAestethics$selectedCondition,labels)

    graphsAestethics$df[n,'lineType'] <- changeAestethics$selectedLine}
})
observeEvent(changeAestethics$selectedColor,{

  labels <- unique(Conditions$df[,5])

  if (changeAestethics$selectedCondition != "None"){
    n = match(changeAestethics$selectedCondition,labels)

    graphsAestethics$df[n,'lineColor'] <- changeAestethics$selectedColor}
})

#Update line and colors
observe({
  
  req(damData$dt)
  
  if (input$pages == "Periodic representations"){
    changeAestethics$selectedCondition <- input$addedConditions
    changeAestethics$selectedLine <- input$linetype
    changeAestethics$selectedColor <- input$linecolor
  }
  if (input$pages == "Activity and sleep statistics"){
    changeAestethics$selectedCondition <- input$addedConditions2
    changeAestethics$selectedColor <- input$linecolor2
  }
  if (input$pages == "Bouts statistics"){
    changeAestethics$selectedCondition <- input$addedConditions3
    changeAestethics$selectedLine <- input$linetype3
    changeAestethics$selectedColor <- input$linecolor3
  }

})

########################### Statistical Data ##############################

#Statistics Data
ActivityRepresentationsData <- function(graph = 'all'){
  
  req(damData$dt)
  
  withProgress(message = 'Actogram and Chronogram analysis', value = 0, {
    ##### Calculate activity and sleep data #####
    
    if (graph == "sleep"){
      File <- c()
      Labels <- c()
      Order <- c()
      Channels <- c()
      Start_date <- c()
      End_date <- c()
      Time_min <- c()
      Asleep <- c()
      
      step <- binSize() /(max(damData$dt[,timeDiff],na.rm=TRUE)/60)
      
      if (nrow(damData$dt[,,meta=T])>1){
        start <- which(is.nan(damData$dt[,timeDiff]))
        end <- c(start[2:length(start)]-1,nrow(damData$dt))
      }
      else{
        start <- 1
        end <- nrow(damData$dt)
      }
      
      # From data frame to vector
      time <- damData$dt[,'periodT']
      sleep <- damData$dt[,'asleep']
      sleep <- sleep*1
      
      for (i in 1:length(start)){
        cond <- rep(FALSE,end[i]-step-start[i]+1)
        cond[seq(1,end[i]-start[i]-step+1,step)]<-TRUE
        
        incProgress(1/length(start)) #Increment of progress bar
        
        #How many repetitions  
        n <- length(which(cond==TRUE))
        indexes <- which(cond==TRUE)+start[i]-1
        
        File <-c(File,rep(damData$dt[start[i],file],n))
        Labels <- c(Labels,rep(damData$dt[start[i],labels],n))
        Channels <- c(Channels, rep(damData$dt[start[i],channels],n))
        Start_date <- c(Start_date, rep(damData$dt[,start_datetime,meta=T][i],n))
        End_date <- c(End_date, rep(damData$dt[,stop_datetime,meta=T][i],n))
        
        for (j in 1:length(indexes)){
          Time_min <- c(Time_min,(time[indexes[j]]$periodT))
          Asleep <- c(Asleep,sum(sleep[indexes[j]:(indexes[j]+step-1)])/(step))
        }
      }
      
      PeriodicData$sleep <- data.frame(File,Labels,Channels,Start_date,End_date, Time_min, Asleep)
      
      ##### Organize data in sleep per animal #####
      
      ids <- unique(interaction(File,Labels,Channels,sep = "//"))
      
      File <- str_split_fixed(ids, "//",3)[,1]
      Labels <- str_split_fixed(ids, "//",3)[,2]
      Channels <- str_split_fixed(ids, "//",3)[,3]
      
      Asleep <- round(aggregate(Asleep ~ interaction(File,Labels,Channels,sep=" - "), data=PeriodicData$sleep, FUN=mean)[,2],3)
      
      df <- data.frame(File,Labels,Channels,Asleep)
      
      #Statistics
      Conditions <- aggregate(Asleep ~ Labels, data=df, FUN=length)[,1]
      N  <- aggregate(Asleep ~ Labels, data=df, FUN=length)[,2]
      Sleep_Mean <- round(aggregate(Asleep ~ Labels, data=df, FUN=mean)[,2],3)
      Sleep_SEM <- round(aggregate(Asleep ~ Labels, data=df, FUN=se)[,2],3)
      Sleep_SD <- round(aggregate(Asleep ~ Labels, data=df, FUN=sd)[,2],3)
      Sleep_Median <- round(aggregate(Asleep ~ Labels, data=df, FUN=median)[,2],3)
      Sleep_25Q  <- round(aggregate(Asleep ~ Labels, data=df, FUN=quantile)[,2][,2],3)
      Sleep_75Q  <- round(aggregate(Asleep ~ Labels, data=df, FUN=quantile)[,2][,4],3)
      
      PeriodicStatistics$sleep <- data.frame(Conditions, N, Sleep_Mean, Sleep_SEM, Sleep_SD, Sleep_Median, Sleep_25Q, Sleep_75Q)
      
    }
    else{
    
      File <- c()
      Labels <- c()
      Order <- c()
      Channels <- c()
      Start_date <- c()
      End_date <- c()
      Time_min <- c()
      Activity <- c()
      Cumulative_activity <- c()
      Asleep <- c()
      
      step <- binSize() /(max(damData$dt[,timeDiff],na.rm=TRUE)/60)
      
      if (nrow(damData$dt[,,meta=T])>1){
        start <- which(is.nan(damData$dt[,timeDiff]))
        end <- c(start[2:length(start)]-1,nrow(damData$dt))
      }
      else{
        start <- 1
        end <- nrow(damData$dt)
      }
      
      # From data frame to vector
      time <- damData$dt[,'periodT']
      activity <- damData$dt[,'activity']
      cumActivity <- damData$dt[,'auc']
      sleep <- damData$dt[,'asleep']
      sleep <- sleep*1
  
      for (i in 1:length(start)){
          cond <- rep(FALSE,end[i]-step-start[i]+1)
          cond[seq(1,end[i]-start[i]-step+1,step)]<-TRUE
            
          incProgress(1/length(start)) #Increment of progress bar
          
          #How many repetitions  
          n <- length(which(cond==TRUE))
          indexes <- which(cond==TRUE)+start[i]-1
          
          File <-c(File,rep(damData$dt[start[i],file],n))
          Labels <- c(Labels,rep(damData$dt[start[i],labels],n))
          Channels <- c(Channels, rep(damData$dt[start[i],channels],n))
          Start_date <- c(Start_date, rep(damData$dt[,start_datetime,meta=T][i],n))
          End_date <- c(End_date, rep(damData$dt[,stop_datetime,meta=T][i],n))
          
          for (j in 1:length(indexes)){
            Time_min <- c(Time_min,(time[indexes[j]]$periodT))
            Activity <- c(Activity,sum(activity[indexes[j]:(indexes[j]+step-1)]))
            Cumulative_activity <- c(Cumulative_activity,max(cumActivity[indexes[j]:(indexes[j]+step-1)]))
            Asleep <- c(Asleep,sum(sleep[indexes[j]:(indexes[j]+step-1)])/(step))
          }
      }
    
    
      PeriodicData$activity <- data.frame(File,Labels,Channels,Start_date,End_date, Time_min, Activity)
      PeriodicData$cumActivity <- data.frame(File,Labels,Channels,Start_date,End_date, Time_min, Cumulative_activity)
      PeriodicData$sleep <- data.frame(File,Labels,Channels,Start_date,End_date, Time_min, Asleep)
      
      ##### Organize data in activity per animal #####
      
      ids <- unique(interaction(File,Labels,Channels,sep = "//"))
      
      File <- str_split_fixed(ids, "//",3)[,1]
      Labels <- str_split_fixed(ids, "//",3)[,2]
      Channels <- str_split_fixed(ids, "//",3)[,3]
  
      Activity <- round(aggregate(Activity ~ interaction(File,Labels,Channels,sep=" - "), data=PeriodicData$activity, FUN=mean)[,2],3)
      MaxActivity <- aggregate(Activity ~ interaction(File,Labels,Channels,sep=" - "), data=PeriodicData$activity, FUN=max)[,2]
      MinActivity <- aggregate(Activity ~ interaction(File,Labels,Channels,sep=" - "), data=PeriodicData$activity, FUN=min)[,2]
      AUC <- aggregate(Cumulative_activity ~ interaction(File,Labels,Channels,sep=" - "), data=PeriodicData$cumActivity, FUN=max)[,2]
      Asleep <- round(aggregate(Asleep ~ interaction(File,Labels,Channels,sep=" - "), data=PeriodicData$sleep, FUN=mean)[,2],3)
      
      df <- data.frame(File,Labels,Channels,Activity,MaxActivity,MinActivity,AUC,Asleep)
      
      #### Activity and Sleep Statistics #####
      
      Conditions <- aggregate(Activity ~ Labels, data=df, FUN=length)[,1]
      N  <- aggregate(Activity ~ Labels, data=df, FUN=length)[,2]
      Activity_Mean <- round(aggregate(Activity ~ Labels, data=df, FUN=mean)[,2],3)
      Activity_SEM <- round(aggregate(Activity ~ Labels, data=df, FUN=se)[,2],3)
      Activity_Max <- round(aggregate(MaxActivity ~ Labels, data=df, FUN=mean)[,2],3)
      Activity_Max_SEM <- round(aggregate(MaxActivity ~ Labels, data=df, FUN=se)[,2],3)
      Activity_Min  <- round(aggregate(MinActivity ~ Labels, data=df, FUN=mean)[,2],3)
      Activity_Min_SEM  <- round(aggregate(MinActivity ~ Labels, data=df, FUN=se)[,2],3)
      
      PeriodicStatistics$activity <- data.frame(Conditions, N, Activity_Mean, Activity_SEM, Activity_Max, Activity_Max_SEM, Activity_Min, Activity_Min_SEM)
      
      Sleep_Mean <- round(aggregate(Asleep ~ Labels, data=df, FUN=mean)[,2],3)
      Sleep_SEM <- round(aggregate(Asleep ~ Labels, data=df, FUN=se)[,2],3)
      Sleep_SD <- round(aggregate(Asleep ~ Labels, data=df, FUN=sd)[,2],3)
      Sleep_Median <- round(aggregate(Asleep ~ Labels, data=df, FUN=median)[,2],3)
      Sleep_25Q  <- round(aggregate(Asleep ~ Labels, data=df, FUN=quantile)[,2][,2],3)
      Sleep_75Q  <- round(aggregate(Asleep ~ Labels, data=df, FUN=quantile)[,2][,4],3)
      
      PeriodicStatistics$sleep <- data.frame(Conditions, N, Sleep_Mean, Sleep_SEM, Sleep_SD, Sleep_Median, Sleep_25Q, Sleep_75Q)
      
      CumActivity_Max_Mean <- round(aggregate(AUC ~ Labels, data=df, FUN=mean)[,2],3)
      CumActivity_Max_SEM <- round(aggregate(AUC ~ Labels, data=df, FUN=se)[,2],3)
      CumActivity_Max_SD <- round(aggregate(AUC ~ Labels, data=df, FUN=sd)[,2],3)
      CumActivity_Max_Median <- round(aggregate(AUC ~ Labels, data=df, FUN=median)[,2],3)
      CumActivity_Max_25Q  <- round(aggregate(AUC ~ Labels, data=df, FUN=quantile)[,2][,2],3)
      CumActivity_Max_75Q  <- round(aggregate(AUC ~ Labels, data=df, FUN=quantile)[,2][,4],3)
      
      PeriodicStatistics$cumActivity <- data.frame(Conditions, N, CumActivity_Max_Mean, CumActivity_Max_SEM, CumActivity_Max_SD, CumActivity_Max_Median, CumActivity_Max_25Q, CumActivity_Max_75Q)
    }
  })
}
PeriodicRepresentationsData <- function(){
  
  # Calculate periodogram data
  
  withProgress(message = 'Periodogram analysis', value = 0, {
    
    req(PeriodData())
    PeriodData()[,period := period/3600]
    
    peaks <- PeriodData()[which(peak==1),]
    
    if (nrow(peaks)>0){
    
      File <- peaks[,File, meta=T]
      Labels <- peaks[,labels,meta=T]
      Channels <- peaks[,region_id,meta=T]
      Start_date <- peaks[,start_datetime,meta=T]
      End_date <- peaks[,stop_datetime,meta=T]
      
      #Export data sheet
      PeriodicData$periodogram <- data.frame(File,Labels,Channels,Start_date,End_date, peaks[,2:5])
      
      Conditions <- aggregate(period ~ Labels, data=PeriodicData$periodogram, FUN=length)[,1]
      N  <- aggregate(period ~ Labels, data=PeriodicData$periodogram, FUN=length)[,2]
      Period_mean <- round(aggregate(period ~ Labels, data=PeriodicData$periodogram, FUN=mean)[,2],3)
      Period_se <- round(aggregate(period ~ Labels, data=PeriodicData$periodogram, FUN=se)[,2],3)
      Period_std <- round(aggregate(period ~ Labels, data=PeriodicData$periodogram, FUN=sd)[,2],3)
      Power_mean <- round(aggregate(power ~ Labels, data=PeriodicData$periodogram, FUN=mean)[,2],3)
      Power_se  <- round(aggregate(power ~ Labels, data=PeriodicData$periodogram, FUN=se)[,2],3)
      Power_std  <- round(aggregate(power ~ Labels, data=PeriodicData$periodogram, FUN=sd)[,2],3)
      
      PeriodicStatistics$periodogram <- data.frame(Conditions, N, Period_mean, Period_se, Period_std, Power_mean, Power_se, Power_std)
    }
    else{
      PeriodicStatistics$periodogram <-  NULL
    }
  })

}

#Periodic data table
observe({
  
  if (input$tabs == "Periodogram"){
    statisticalData <- PeriodicStatistics$periodogram}
  else{
    if (input$tabs == "Cumulative Activity"){
      statisticalData <-PeriodicStatistics$cumActivity}
    else{
      if (input$tabs == "Sleep Chronogram"){
        statisticalData <- PeriodicStatistics$sleep}
      else
        statisticalData <- PeriodicStatistics$activity}}
    
output$PeriodData <- renderDataTable(statisticalData,
                                     escape = FALSE, selection = 'none', 
                                     editable  = list(target = 'cell',disable = list(columns = c(1,2,3,4,5,6,7,8))))
})

####### Periodogram #

observeEvent(input$perFun,{
  if (input$perFun == "ls"){
    updateSliderInput(session, 'periodogramValue', label= "Oversampling", value  = 32, min = 2, max = 128, step = 1)
  }
  if (input$perFun == "chi-sq"){
    updateSliderInput(session, 'periodogramValue', label= "Time resolution", value  = 0.1, min = 0.05, max = 1, step = 0.05)
  }
})

################################ PLOTS #######################################

observeEvent(input$startanalysis,{
  
  req(damData$dt)
  
  binSize(input$movingAverage)
  
  update_periodT()
  updateFigures()
  
  PeriodicRepresentationsData()
  ActivityRepresentationsData()
  
  updateTabsetPanel(session, 'tabs', selected = "Full chronogram")
  updateTabsetPanel(session, 'tabs', selected = "Actogram")
})


#Update graphs and Data
observeEvent(input$updateAesthetics,{
  req(damData$dt)
  
  settings <- settingsTable()
  settings[1,2] <- l_period()
  settings[2,2] <- l_hours()
  settingsTable(settings)
  
    update_periodT()
    
    updateLabels(input$tabs)
    updateFigures()
    
    updateActivityFigures()
    updateSleepFigures()
    
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

#Actogram
output$actogram <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- Figures$actogram

  return(fig)

}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

#Double plot Actogram per Channel
output$doublePlotActogram <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- Figures$doublePlot

  return(fig)



}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

#Chronogram over multiple days
output$chronogram <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- Figures$chronogram

  return (fig)

}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

#Chronogram for 1 day
output$chronogram1day <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- Figures$chronogram24h


  return(fig)
  
}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

# #Periodogram
output$periodogram <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  
  fig <- Figures$periodogram


  return(fig)

}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

#Cumulative activity graph
output$cumAct <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )

  fig <- Figures$cumActivity


  return (fig)

}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

#Sleep
output$sleep <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )

  fig <- Figures$sleep


  return(fig)
}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)


##### Save time continuous graphics values #####

shinyjs::disable("saveData")
shinyjs::disable("saveFigures")

observe({
  req(damData$dt)

  if (nrow(damData$dt)>0){
    shinyjs::enable("saveData")
  }
  else{
    shinyjs::disable("saveData")
  }
  
output$saveData <- downloadHandler(
  filename = function(){
    paste0(input$tabs,".xlsx")
  },
  content = function(file){
    
    wb <- createWorkbook(type="xlsx")
    
    if (input$tabs == "Periodogram"){
      
      settings <- settingsTable()[6:9,]
      if(settings[1,2] == "ls"){
        settings[1,2] <- "Lomb-Scargle"
        settings[2,1] <- "Oversampling"}
      else{
        settings[1,2] <- "Chi-square"
        settings[2,1] <- "Resolution (h)"
      }
      sheet <- createSheet(wb, "Settings")
      addDataFrame(settings, sheet=sheet, startColumn=1, row.names=FALSE)
      
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(PeriodicStatistics$periodogram, sheet=sheet, startColumn=1, row.names=FALSE)
      
      sheet <- createSheet(wb, "Animals")
      addDataFrame(PeriodicData$periodogram, sheet=sheet, startColumn=1, row.names=FALSE)
      
      #Create organized data sheet
      Labels <- PeriodicData$periodogram[,'Labels']
      Period <- PeriodicData$periodogram[,'period']
      Power <- PeriodicData$periodogram[,'power']
      periodDF <- data.frame(Labels,Period)
      powerDF <- data.frame(Labels,Power)
      joinedDataPeriod <- periodDF %>%group_by(Labels) %>% group_nest()
      joinedDataPower <- powerDF %>%group_by(Labels) %>% group_nest()
      
      periodData <- data.frame()
      powerData <- data.frame()
      
      for (i in 1:nrow(joinedDataPeriod)){
        periodData <- cbind.fill(periodData, (unlist(joinedDataPeriod[i,'data'])))
        powerData <- cbind.fill(powerData, (unlist(joinedDataPower[i,'data'])))
      }
      colnames(periodData)<- unlist(joinedDataPeriod[,'Labels'])
      colnames(powerData)<- unlist(joinedDataPower[,'Labels'])
      

      sheet <- createSheet(wb, "Period")
      addDataFrame(periodData, sheet=sheet, startColumn=1, row.names=FALSE)
      sheet <- createSheet(wb, "Power")
      addDataFrame(powerData, sheet=sheet, startColumn=1, row.names=FALSE)
    }
    else{
        
      # If data is not calculated
      if (is.null(PeriodicStatistics$activity)){
        ActivityRepresentationsData()
      }
      
      # Get data from each graph
      if (input$tabs =="Cumulative Activity"){
        
        data <- PeriodicData$cumActivity
        statistics <- PeriodicStatistics$cumActivity}
      else{
        if (input$tabs =="Sleep Chronogram"){
          data <- PeriodicData$sleep
          statistics <- PeriodicStatistics$sleep
        }
        else{
          data <- PeriodicData$activity
          statistics  <- PeriodicStatistics$activity
        }
      }
        
      
      settings <- rbind(settingsTable()[1:3,],settingsTable()[5,])
      sheet <- createSheet(wb, "Settings")
      addDataFrame(settings, sheet=sheet, startColumn=1, row.names=FALSE)
      
      sheet = createSheet(wb, "Statistics")
      addDataFrame(statistics, sheet=sheet, startColumn=1, row.names=FALSE)
      
      # Create sheets
      Labels <- data[,'Labels']
      uniqueLabels <- unique(Labels)
      if (length (uniqueLabels)>1){
        start <- match(uniqueLabels[1],Labels)
        for (i in 2:length(uniqueLabels)){

          sheet <- createSheet(wb, checkSymbolsExcel(uniqueLabels[i-1]))
          end <- match(uniqueLabels[i],Labels)-1
          addDataFrame(data[start:end,], sheet=sheet)
          start <- end+1
          }
        sheet <- createSheet(wb, checkSymbolsExcel(uniqueLabels[length(uniqueLabels)]))
        addDataFrame(data[start:nrow(data),], sheet=sheet)
      }
      else{
          sheet <- createSheet(wb, checkSymbolsExcel(uniqueLabels[1]))
          addDataFrame(data, sheet=sheet, startColumn=1, row.names=FALSE)
        }
        
      #Create organized data sheet
      summaryDF <- data[,6:7]
      joinedData <- summaryDF %>%group_by(Time_min) %>% group_nest()
      dataDF <- data.frame(matrix(unlist(joinedData[,'data']), nrow=nrow(joinedData[,'data']), byrow=TRUE))
      labels <- unique(paste(data[,'Labels'],data[,'File',],data[,'Channels'],sep = "_"))
      for (i in 1:ncol(dataDF)){
        names(dataDF)[i] <- labels[i]
      }
      summaryData <- data.frame (joinedData[,1],dataDF)

      sheet <- createSheet(wb, "Summary")
      addDataFrame(summaryData, sheet=sheet, startColumn=1, row.names=FALSE)
        
      }
      
    saveWorkbook(wb, file)
    }
  )
})
observe({
  req(damData$dt)

  if (nrow(damData$dt)>0){
    shinyjs::enable("saveFigures")
  }
  else{
    shinyjs::disable("saveFigures")
  }

  output$saveFigures <- downloadHandler(

    filename = function(){
      paste0(input$tabs,input$periodicFigures)
    },
    content = function(file){
      
      if (input$tabs == "Actogram"){
        fig <- Figures$actogram
      }
      else{
        if (input$tabs == "Double plot actogram"){
          fig <- Figures$doublePlot
        }
        else{
          if (input$tabs == "Full chronogram"){
            fig <- Figures$chronogram
          }
          else{
            if (input$tabs == "Average LD cycle chronogram"){
              fig <- Figures$chronogram24h
            }
            else{
              if (input$tabs == "Periodogram"){
                fig <- Figures$periodogram
              }
              else{
                if (input$tabs == "Cumulative Activity"){
                  fig <- Figures$cumActivity
                }
                else{
                  if (input$tabs == "Sleep Chronogram"){
                    fig <- Figures$sleep
                  }
                }
              }
            }
          }
        }
      }

      ggsave(filename = file, plot = fig,
             width = round(input$periodicWidth/97), height= round(input$periodicHeight/97))
    }
  )

})

