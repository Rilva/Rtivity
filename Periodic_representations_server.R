
###########################  FUNCTIONS #################################

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
      if (input$pages == "Activity bouts statistics"){
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

#xLabel of continuous graphs
xAxis <- function (fig){
  
  req(input$xTime)
  req(input$x0)
  req(is.numeric(input$ticks_distance))
  
  # If no labels have been inputted, use the labels imposed by the scale_x functions
  
  x_label <- paste0("Time (",input$xTime,")")
  if (!input$xLabel == ""){
    x_label <- input$xLabel
  }
  
  maximumTime <- max(damData$dt[,periodT])
  minimumTime <- min(damData$dt[,periodT])

  #Update x axis according to the time in analysis
  
  if (input$xTime == "days"){
    max_days <- ceiling(maximumTime/86400)
    updateSliderInput(session,'x0', max = floor(max_days/2))
    fig <- fig + scale_x_continuous(name = x_label, breaks = seq(from = input$tick0*86400, to = max_days*86400, by = input$ticks_distance*86400), labels = sec_to_day)
  }
  if (input$xTime == "hour"){
    max_hour <- floor(maximumTime/3600)
    updateSliderInput(session,'x0', max = round(max_hour/2))
    fig <- fig + scale_x_continuous(name = x_label, breaks = seq(from = input$tick0*3600, to = max_hour*3600, by = input$ticks_distance*3600), labels = sec_to_hour)
  }
  if (input$xTime == "min"){
    max_min <- floor(maximumTime/60)
    updateSliderInput(session,'x0', max = round(max_min/2))
    fig <- fig + scale_x_continuous(name = x_label, breaks = seq(from = input$tick0*60, to = max_min*60, by = input$ticks_distance*60), labels = sec_to_min)
  }
  if (input$xTime == "sec"){
    max_sec <- floor(maximumTime)
    updateSliderInput(session,'x0', max = round(max_sec/2))
    fig <- fig + scale_x_continuous(name = x_label, breaks = seq(from = input$tick0, to = max_sec, by = input$ticks_distance))
  }
  
  return (fig)
}

#yaxis of continuous graphs
yAxis <- function(fig){
  
  #Graph y label
  if (input$yLabel == ""){
    if (input$tabs == "Periodogram"){
      fig <- fig + ylab("Power")
    }
    else{
      if (input$tabs == "Cumulative Activity"){
        fig <- fig + ylab("Cumulative activity")
      }
      else{
        if(input$tabs == "Sleep Chronogram"){
          fig <- fig + ylab(paste("Mean sleep time ratio per",input$movingAverage,"minutes"))
        }
        else{
          fig <- fig + ylab(paste("Mean activity per",input$movingAverage, "minutes"))
        }
      }
    }
  }
  else{
    fig <- fig + ylab(input$yLabel)
  }
  
  if (input$tabs == "Chronogram" | input$tabs == "Daily Chronogram" | input$tabs == "Cumulative Activity"){
    fig <- fig + coord_cartesian(ylim = c(input$y_min,input$y_max))}
  if (input$tabs == "Sleep Chronogram"){
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
plotLabels <- function(fig){
  
  #Light dark annotations
  if (input$lightDark == TRUE){
    fig <- fig + stat_ld_annotations(l_duration = hours(floor(l_hours())), period = hours(l_period()))
  }
  
  #X and Y axis update
  fig <- xAxis(fig)
  fig <- yAxis(fig)
  
  #Graph title
  if (input$graphTitle == ""){
    fig <- fig + ggtitle(input$tabs)
  }
  else{
    fig <- fig + ggtitle(input$graphTitle)
  }
  
  if (input$individualPlot == 'individual'){
    fig <- fig + facet_grid(~ interaction(labels,order)) }
  
  #Remove background from figures
  fig <- whiteBackground(fig)
  
  return(fig)
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

#Save figures
saveFig <- reactiveVal()

#Save statistic data
saveData <- reactiveVal()

#save Periodogram variable
savePer <- reactiveValues(dt = NULL)

########################################################################

### Update data variables according to the time-continuous inputs
observe({

  req(damData$dt)
  req(input$sleepTime)

  #Zero time and zeitgeber (or data) time
  if (is.numeric(input$x0)){
    if (input$xTime == "days"){
      sumTime <- days(input$x0)}
    if (input$xTime == "hour"){
      sumTime <- hours(input$x0)}
    if (input$xTime == "min"){
      sumTime <- mins(input$x0)}
    if (input$xTime == "sec"){
      sumTime <- input$x0}
  }
  else{
    sumTime <- 0
  }

  if(input$dataTime=="normal"){
    damData$dt[,'periodT' := (damData$dt[,'t']-min(damData$dt[,'t'])+sumTime)]
  }
  else{
    damData$dt[,'periodT' := (damData$dt[,'t']+sumTime)]
  }

  damData$dt[, experimentDay := floor(periodT/(l_period()*3600))] #Experiment day
  damData$dt[, day_night := ((periodT-experimentDay*l_period()*3600)/3600)<l_hours()] #Day or night time


  if (input$activityBoxTime == "Day"){
    damData$dt[, 'activityBoxPlot_time' := floor(damData$dt[,'t']/(input$activityGroupBoxTime * l_period()*3600))]}
  if (input$activityBoxTime == "Hour"){
    damData$dt[, 'activityBoxPlot_time' := floor(damData$dt[,'t']/(input$activityGroupBoxTime *3600))]}
  if (input$activityBoxTime == "Min"){
    damData$dt[, 'activityBoxPlot_time' := floor(damData$dt[,'t']/(input$activityGroupBoxTime * 60))]}

  if (input$boutBoxTime == "Day"){
    damData$dt[, 'boutBoxPlot_time' := floor(damData$dt[,'t']/(input$boutGroupBoxTime * l_period()*3600))]}
  if (input$boutBoxTime == "Hour"){
    damData$dt[, 'boutBoxPlot_time' := floor(damData$dt[,'t']/(input$boutGroupBoxTime * 3600))]}
  if (input$boutBoxTime == "Min"){
    damData$dt[, 'boutBoxPlot_time' := floor(damData$dt[,'t']/(input$boutGroupBoxTime * 60))]}

  damData$dt[,Data_labels := interaction(File, labels,region_id, sep=" - "), meta =T]
  damData$dt[,Data := interaction(labels,order,sep=" - "), meta = T]
})

#Change DAM data if sleep time changes
observeEvent(input$sleepTime,{

  #get DAM data
  req (damData$dt)
  data <- link_dam_metadata(Conditions$df,Directory()) #linking
  damData$dt <- load_dam(data, FUN = sleep_dam_annotation, min_time_immobile = 60*input$sleepTime) #load dam data

  MinTime(TRUE)

  if (nrow(damData$dt)>60){
    enable("boutAnalysis")
  }

})

#X scale - update according to changes in x variables
observeEvent(input$x0,{

  req(damData$dt)
  req(is.numeric(input$x0))

  update_periodT()

})
observeEvent(input$xTime,{

  req(damData$dt)
  req(is.numeric(input$x0))

  update_periodT()

  maximumTime <- max(damData$dt[,periodT])
  minimumTime <- min(damData$dt[,periodT])

  diffTime = maximumTime-minimumTime

  #Update ticks_distance variable according to the x time scale
  if (input$xTime == "days"){
    updateNumericInput(session, 'ticks_distance','Distance between ticks', round(floor(diffTime/86400)/5), min = 0.25)
  }
  else{
    if (input$xTime == "hour"){
      updateNumericInput(session, 'ticks_distance','Distance between ticks', round(floor(diffTime/3600)/5), min = 0.5)
    }
    else{
      if (input$xTime == "min"){
        updateNumericInput(session, 'ticks_distance','Distance between ticks', round(floor(diffTime/60)/5), min = 0.5)
      }
      else{
        updateNumericInput(session, 'ticks_distance','Distance between ticks', round(diffTime/5), min = 1)
      }
    }
  }

})
observeEvent(input$dataTime,{

  req(damData$dt)
  req(is.numeric(input$x0))

  update_periodT()

})

#Update radio buttons of x ticks
observe({

  req(damData$dt)
  req("t" %in% colnames(damData$dt))

  maximumTime <- max(damData$dt[,t])
  minimumTime <- min(damData$dt[,t])

  diffTime <- maximumTime-minimumTime

  if (diffTime > 3*86400){
  updateRadioButtons(session,'xTime','', c("Days" = "days","Hours" = "hour"), selected = "days", inline = TRUE)}
  else{
    if (diffTime >86400){
      updateRadioButtons(session,'xTime','', c("Days" = "days","Hours" = "hour"), selected = "hour", inline = TRUE)
    }
    else{
      if (diffTime >12*3600){
        updateRadioButtons(session,'xTime','', c("Hours" = "hour","Minutes" = "min"), selected = "hour", inline = TRUE)
      }
      else{
        if(diffTime > 3600){
          updateRadioButtons(session,'xTime','', c("Hours" = "hour","Minutes" = "min"), selected = "min", inline = TRUE)
        }
        else{
          updateRadioButtons(session,'xTime','', c("Minutes" = "min","Seconds" = "sec"), selected = "min", inline = TRUE)
        }
      }
    }}
})

#Don't let the ticks distance variable be lower or equal than 0
observeEvent(input$ticks_distance,{

  if (is.numeric(input$ticks_distance) & input$ticks_distance <=0){
    updateNumericInput(session, 'ticks_distance','Distance between ticks', 1)
  }

})

########################## GRAPHICS AESTETHICS ##############################


#Get user input aestethics
observe ({

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
    updateSelectInput(session,"linetype2", selected = graphsAestethics$df[n,'lineType'])
    updateColourInput(session,"linecolor2", value = graphsAestethics$df[n,'lineColor'], allowTransparent = TRUE)
    updateSelectInput(session,"linetype3", selected = graphsAestethics$df[n,'lineType'])
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
  if (input$pages == "Periodic representations"){
    changeAestethics$selectedCondition <- input$addedConditions
    changeAestethics$selectedLine <- input$linetype
    changeAestethics$selectedColor <- input$linecolor
  }
  if (input$pages == "Activity and sleep statistics"){
    changeAestethics$selectedCondition <- input$addedConditions2
    changeAestethics$selectedLine <- input$linetype2
    changeAestethics$selectedColor <- input$linecolor2
  }
  if (input$pages == "Activity bouts statistics"){
    changeAestethics$selectedCondition <- input$addedConditions3
    changeAestethics$selectedLine <- input$linetype3
    changeAestethics$selectedColor <- input$linecolor3
  }

})


################################ PLOTS #######################################

#Actogram
output$actogram <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  req(input$dataTime)

  fig <- ggetho(damData$dt,aes(x = periodT, y = Data,z = activity),
                summary_FUN = sum,summary_time_window = mins(input$movingAverage))

  if (input$tile_bar == "bar"){
    fig <- fig + stat_bar_tile_etho()
  }
  else{
    fig <- fig + stat_tile_etho()
  }

  fig <- plotLabels(fig)
  fig <- fig + labs(z = "Activity")

  saveFig(fig)

  return(fig)

}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

#Double plot Actogram per Channel
output$doublePlotActogram <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  req(input$dataTime)

  fig <- ggetho(damData$dt,aes(x = t, z = activity),multiplot = 2, multiplot_period = hours(l_period()), summary_FUN = sum,
                summary_time_window = mins(input$movingAverage))

  if (input$tile_bar == "bar"){
    fig <- fig + stat_bar_tile_etho()
  }
  else{
    fig <- fig + stat_tile_etho()
  }

  fig <-plotLabels(fig+ facet_wrap( ~ Data))

  saveFig(fig)
  return(fig)



}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

#Chronogram over multiple days
output$chronogram <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  req(input$dataTime)

  fig <- ggetho(damData$dt,aes(x = periodT, y = activity, colour=Data, linetype = Data),summary_FUN = sum,
                summary_time_window = mins(input$movingAverage))

  fig <- plotLabels(fig + stat_pop_etho(method = input$errorChronogram))

  #Change graphs aestethics
  fig <- fig + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])

  #Add errors
  if (input$chronogramError == "TRUE"){
    fig <- fig + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  else{
    fig <- fig + scale_fill_manual(values =  alpha(rep(NA, 100)), na.value = NA)
  }

  saveFig(fig)
  return (fig)

}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

#Chronogram for 1 day
output$chronogram1day <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  req(input$dataTime)

  #Divide data per day, otherwise the representation will represent the sum of the data from all days
  n <- as.numeric(ceiling((max(damData$dt[,'t'])-min(damData$dt[,'t']))/(l_period()*3600)))
  damData$dt[,'activity':= damData$dt[,'activity']/n]

  fig <- ggetho(damData$dt,aes(x = periodT, y = activity, colour=Data, linetype = Data),summary_FUN = sum,
                summary_time_window = mins(input$movingAverage), time_wrap = hours(l_period()))

  #Reset the data to the original values
  damData$dt[,'activity':= damData$dt[,'activity']*n]


  fig <- plotLabels(fig+stat_pop_etho(method = input$errorChronogram))

  #Change graph aestethics
  fig<- fig + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])

  #Adjust to errors
  if (input$chronogramError == "TRUE"){
    fig <- fig + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  else{
    fig <- fig + scale_fill_manual(values =  rep(NA, 100), na.value = NA)
  }

  saveFig(fig)

  return(fig)
}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

# #Periodogram
output$periodogram <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  req(input$dataTime)

  #Represent the periodogram according to the selected function
  if (input$perFun=='chi-sq'){
    per_dt <- periodogram(activity, damData$dt, FUN = chi_sq_periodogram,
                          period_range = c(hours(input$minPer), hours(input$maxPer)), time_resolution = mins(10))
  }
  if (input$perFun=='fourier'){
    per_dt <- periodogram(activity, damData$dt, FUN = fourier_periodogram,
                          period_range = c(hours(input$minPer), hours(input$maxPer)))
  }
  if (input$perFun=='ls'){
    per_dt <- periodogram(activity, damData$dt, FUN = ls_periodogram,
                          period_range = c(hours(input$minPer), hours(input$maxPer)),oversampling = 32)
  }

  per_dt <- find_peaks(per_dt)

  #Create periodogram
  fig<- ggperio(per_dt, aes(period, power, colour=Data, linetype = Data)) +
    stat_pop_etho(method = input$errorChronogram) +
    scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])

  fig <-plotLabels(fig)

  #X scale

  x_label <- paste("Period (h)")
  if (!input$xLabel == ""){
    x_label <- input$xLabel
  }

  if(is.numeric(input$ticks_distance) & input$ticks_distance >0){
    fig <- fig + scale_x_continuous(name = x_label, breaks = seq(from = input$minPer*3600, to = input$maxPer*3600, by = input$ticks_distance*3600), labels = sec_to_hour)
  }
  if (input$showPeriods){
    fig <- fig + mean(geom_peak(peak_rank = 1))
  }

  #Errors aestethics
  if (input$chronogramError == "TRUE"){
    fig <- fig + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  else{
    fig <- fig + scale_fill_manual(values =  rep(NA, 100), na.value = NA)
  }

  saveData(find_peaks(per_dt))
  saveFig(fig)

  return(fig)

}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

#Cumulative activity graph
output$cumAct <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  req(input$dataTime)

  #Create cumulative activity graph
  fig <- ggetho(damData$dt,aes(x = periodT, y = auc, colour=Data, linetype = Data),summary_FUN = max,
                summary_time_window = mins(input$movingAverage))

  fig <- plotLabels(fig + stat_pop_etho(method = input$errorChronogram))

  fig <- fig + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])

  #Errors aestethics
  if (input$chronogramError == "TRUE"){
    fig <- fig + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  else{
    fig <- fig + scale_fill_manual(values =  rep(NA, 100), na.value = NA)
  }

  saveFig(fig)

  return (fig)

}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)

#Sleep
output$sleep <- renderPlot({

  validate(
    need (nrow(damData$dt)>0,"No data selected")
  )
  req(input$dataTime)
  req('periodT' %in% colnames(damData$dt))

  fig <- ggetho(damData$dt,aes(x = periodT, y = asleep, colour=Data, linetype = Data),summary_FUN = mean,
                summary_time_window = mins(input$movingAverage))

  fig <- plotLabels(fig + stat_pop_etho(method = input$errorChronogram))

  fig <- fig + scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])

  if (input$chronogramError == "TRUE"){
    fig <- fig + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  else{
    fig <- fig + scale_fill_manual(values = rep(NA, 100), na.value = NA)
  }

  saveFig(fig)

  return(fig)
}, width = function() input$periodicWidth,
height = function() input$periodicHeight,
res = 96)


#Save time continuous graphics values

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

  if (input$tabs == "Periodogram"){
    saveData()[,period := period/3600]

    peaks <- saveData()[which(peak==1),]

    File <- peaks[,File, meta=T]
    Labels <- peaks[,labels,meta=T]
    Channels <- peaks[,region_id,meta=T]
    Start_date <- peaks[,start_datetime,meta=T]
    End_date <- peaks[,stop_datetime,meta=T]

    df <- data.frame(File,Labels,Channels,Start_date,End_date, peaks[,2:5])
    df <- df[order(Labels),]
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

    step <- input$movingAverage /(max(damData$dt[,timeDiff],na.rm=TRUE)/60)

    start <- which(is.nan(damData$dt[,timeDiff]))
    end <- c(start[2:length(start)]-1,nrow(damData$dt))

    for (i in 1:length(start)){
      for (j in seq(start[i],end[i]-step,step)){
        File <-c(File,damData$dt[j,file])
        Labels <- c(Labels,damData$dt[j,labels])
        Order <-c(Order,damData$dt[j,order])
        Channels <- c(Channels, damData$dt[j,channels])
        Start_date <- c(Start_date, damData$dt[,start_datetime,meta=T][i])
        End_date <- c(End_date, damData$dt[,stop_datetime,meta=T][i])
        Time_min <- c(Time_min,damData$dt[j,periodT]/60)
        Activity <- c(Activity,sum(damData$dt[j:(j+step-1),activity]))
        Cumulative_activity <- c(Cumulative_activity,max(damData$dt[j:(j+step-1),auc]))
        Asleep <- c(Activity,sum(damData$dt[j:(j+step-1),asleep]))
      }
    }

    if (input$tabs == "Cumulative Activity"){

      df <- data.frame(File,Labels,Channels,Start_date,End_date, Time_min, Cumulative_activity)}
    else{
      if (input$tabs == "Sleep Chronogram"){
        df <- data.frame(File,Labels,Channels,Start_date,End_date, Time_min, Asleep)}
      else{
      df <- data.frame(File,Labels,Channels,Start_date,End_date, Time_min, Activity)
      }
    }

    df <- df[order(interaction(Labels,Order)),]

  }

  

  output$saveData <- downloadHandler(
    filename = function(){
      paste0(input$tabs,".xlsx")
    },
    content = function(file){
      
      wb <- createWorkbook(type="xlsx")
      
        Labels <- df[,'Labels']
        uniqueLabels <- unique(Labels)
        if (length (uniqueLabels)>1){
          start <- match(uniqueLabels[1],Labels)
          for (i in 2: length(uniqueLabels)){
      
            sheet = createSheet(wb, uniqueLabels[i-1])
            end <- match(uniqueLabels[i],Labels)-1
            addDataFrame(df[start:end,], sheet=sheet)
            start <- end+1
          }
          sheet = createSheet(wb, uniqueLabels[length(uniqueLabels)])
          addDataFrame(df[start:nrow(df),], sheet=sheet)
        }
        else{
          sheet = createSheet(wb, uniqueLabels[1])
          addDataFrame(df, sheet=sheet, startColumn=1, row.names=FALSE)
        }
      
        if (input$tabs != "Periodogram"){
          summaryDF <- df[,6:7]
          joinedData <- summaryDF %>%group_by(Time_min) %>% group_nest()
      
          dataDF <- data.frame(matrix(unlist(joinedData[,'data']), nrow=nrow(joinedData[,'data']), byrow=TRUE))
      
          labels <- unique(paste(df[,'Labels'],df[,'File',],df[,'Channels'],sep = "_"))
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
})

# observeEvent(input$saveFigures,{
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

      ggsave(filename = file, plot = saveFig(),
             width = round(input$periodicWidth/97), height= round(input$periodicHeight/97))
    }
  )

})

