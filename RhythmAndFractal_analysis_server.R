
################################ VARIABLES ##################################

CircadianFigures <- reactiveValues(periodogram = NULL, period= NULL, power = NULL, 
                                   phase = NULL, phasePerDay = NULL, 
                                   iv = NULL, ivPerDay = NULL, ra = NULL, raPerDay = NULL,
                                   is = NULL)

CircadianFiguresTitles <- reactiveValues( periodogram = "Periodogram", period = "Period", power = "Periodogram power", 
                                          phase = "Activity phase",phasePerDay = "Activity phase",
                                          iv = "Intradaily Variation",ivPerDay = "Intradaily Variation",
                                          ra = "Relative Amplitude", raPerDay = "Relative Amplitude",
                                          is = "Interdaily Stability")

CircadianFiguresXlabels <- reactiveValues(periodogram = "Period (h)", period = NULL, power = NULL, 
                                          phase = NULL, phasePerDay = "Time",
                                          iv = NULL,ivPerDay = "Time",
                                          ra = NULL, raPerDay = "Time",
                                          is = NULL)

CircadianFiguresYlabels <- reactiveValues(periodogram = "Power", period = "Period (h)", power = "Power", 
                                          phase = "Acrophase", phasePerDay = "Acrophase",
                                          iv = "Intradaily Variation",ivPerDay = "Intradaily Variation",
                                          ra = "Relative Amplitude", raPerDay = "Relative Amplitude",
                                          is = "Interdaily Stability")

CircadianData <- reactiveValues(periodogram = NULL, period = NULL, power= NULL,
                                phase = NULL, phasePerDay = NULL, iv = NULL, isPerDay = NULL,
                                ra = NULL, raPerDay = NULL, is = NULL)

CircadianStatistics <- reactiveValues(periodogram = NULL, period = NULL, power = NULL,
                                      phase = NULL, phasePerDay = NULL, iv = NULL, ivPerDay = NULL,
                                      ra = NULL, raPerDay = NULL, is = NULL)


####### DAF
DAFdata <- reactiveValues(ScalingExponents = NULL)

DAFstatisticsData <- reactiveVal(NULL)

DAFfigure <- reactiveVal(NULL)

DAFfigureTitle <- reactiveVal("Fractal analysis")
DAFfigureXlabel <- reactiveVal("Time scale n (h)")
DAFfigureYlabel <- reactiveVal("Detrending fluctuation F(n)")


################################ Functions ##################################

#Present statistics
PeriodogramReport <- function(Data, x=TRUE){
  
  req(nrow(Data)>0)
  if(x==TRUE){
    colnames(Data)[2] <- 'Labels'
    conditions <- aggregate(yPlot ~ xPlot, data=Data, FUN=mean)[,1]
    
    Condition <- str_split_fixed(conditions, " - ",2)[,1]
    
    N  <- aggregate(yPlot ~ xPlot, data=Data, FUN=length)[,2]
    Data_Mean <- round(aggregate(yPlot ~ xPlot, data=Data, FUN=mean)[,2],3)
    Data_SEM <- round(aggregate(yPlot ~ xPlot, data=Data, FUN=se)[,2],3)
    Data_SD <- round(aggregate(yPlot ~ xPlot, data=Data, FUN=sd)[,2],3)
    Data_Median <- round(aggregate(yPlot ~ xPlot, data=Data, FUN=median)[,2],3)
    Data_1st_Quartile <- round(aggregate(yPlot ~ xPlot, data=Data, FUN=quantile)[,2][,2],3)
    Data_3rd_Quartile <- round(aggregate(yPlot ~ xPlot, data=Data, FUN=quantile)[,2][,4],3)
  
    return(data.frame(Condition, N, Data_Mean, Data_SEM, Data_SD, Data_Median, Data_1st_Quartile, Data_3rd_Quartile))
    
    }
  else{
  colnames(Data)[2] <- 'Labels'
  conditions <- aggregate(yPlot ~ interaction(xPlot,Labels,sep = " - "), data=Data, FUN=mean)[,1]
  
  Condition <- str_split_fixed(conditions, " - ",2)[,2]
  Label <-  str_split_fixed(conditions, " - ",2)[,1]
  
  N  <- aggregate(yPlot ~ interaction(xPlot,Labels,sep = " - "), data=Data, FUN=length)[,2]
  Data_Mean <- round(aggregate(yPlot ~ interaction(xPlot,Labels,sep = " - "), data=Data, FUN=mean)[,2],3)
  Data_SEM <- round(aggregate(yPlot ~ interaction(xPlot,Labels,sep = " - "), data=Data, FUN=se)[,2],3)
  Data_SD <- round(aggregate(yPlot ~ interaction(xPlot,Labels,sep = " - "), data=Data, FUN=sd)[,2],3)
  Data_Median <- round(aggregate(yPlot ~ interaction(xPlot,Labels,sep = " - "), data=Data, FUN=median)[,2],3)
  Data_1st_Quartile <- round(aggregate(yPlot ~ interaction(xPlot,Labels,sep = " - "), data=Data, FUN=quantile)[,2][,2],3)
  Data_3rd_Quartile <- round(aggregate(yPlot ~ interaction(xPlot,Labels,sep = " - "), data=Data, FUN=quantile)[,2][,4],3)
  
  }
  return(data.frame(Condition, Label, N, Data_Mean, Data_SEM, Data_SD, Data_Median, Data_1st_Quartile, Data_3rd_Quartile))
}

PeriodogramData <- function() {
  
  req(damData$dt)
  ##### Periodogram #####
  #Represent the periodogram according to the selected function

  if(max(damData$dt[,t])-min(damData$dt[,t]) > hours(l_period()) & input$minPer<l_period() & input$maxPer > l_period()){
    
    per_dt <- NULL
    if (input$perFun=='chi-sq'){
      tryCatch({per_dt <- periodogram(activity, damData$dt, FUN = chi_sq_periodogram,
                            period_range = c(hours(input$minPer), hours(input$maxPer)), time_resolution = hours(input$periodogramValue))
      },error = function(e) {
        showNotification("Cannot produce periodograms of 0 activity conditions", type = "error", duration = 5)
      }, finally ={})
      }
    if (input$perFun=='ls'){
      tryCatch({per_dt <- periodogram(activity, damData$dt, FUN = ls_periodogram,
                            period_range = c(hours(input$minPer), hours(input$maxPer)),oversampling = input$periodogramValue)
      },error = function(e) {
        showNotification("Cannot produce periodograms of 0 activity conditions", type = "error", duration = 5)
      }, finally ={})
    }
    
    if (!is.null(per_dt)){
      per_dt <- find_peaks(per_dt, n_peaks = 1)
      CircadianData$periodogram <- per_dt
    }
    else{
      CircadianData$periodogram <- NULL
    }
    
  }
  else{
    CircadianData$periodogram <- NULL
  }
}
PeriodAndPower<- function(){
  
  req(CircadianData$periodogram)
  
  #Period
  data <- CircadianData$periodogram[which(peak==1),]

  period <- cbind(data[,labels,meta=T],data[,2]/3600, data[,Data,meta=T])
  colnames(period)[1] <- 'xPlot'
  colnames(period)[2] <- 'yPlot' 
  colnames(period)[3] <- 'Data'
  
  CircadianData$period <- period
  
  power <- cbind(data[,labels,meta=T],data[,3], data[,Data,meta=T])
  colnames(power)[1] <- 'xPlot'
  colnames(power)[2] <- 'yPlot'
  colnames(power)[3] <- 'Data'
  
  CircadianData$power <- power
}
DAFsummary <- function(){
  req(damData$dt)
  
  #Unique ids
  ids <- unique(damData$dt[,id])
  
  #### Statistics variables
  File <- c()
  Labels <- c()
  Channels <- c()
  Start_datetime <- c()
  End_datetime <- c()
  ScalingExponent1 <- c()
  ScalingExponent2 <- c()
  
  #Get scaling expoent of each animal
  for(i in 1:length(ids)){
    #Mean activity scaling exponent
    animalData <- damData$dt[damData$dt[,id] == ids[i] & day_night == TRUE,]
    
    animalDFA1 <- dfa(time.series = animalData[,activity],npoints = input$nFractal,
                      window.size.range=c(mins(input$Fractal_limits[1]),mins(input$TimeScale_limits)),
                      do.plot=FALSE)
    
    animalDFA2 <- dfa(time.series = animalData[,activity],npoints = input$nFractal,
                      window.size.range=c(mins(input$TimeScale_limits),mins(input$Fractal_limits[2])),
                      do.plot=FALSE)
    
    
    if (any(is.nan(animalDFA1$fluctuation.function)==FALSE)){
      estimation1 <- estimate(animalDFA1,do.plot=FALSE)
      ScalingExponent1 <-c(ScalingExponent1,estimation1[1])
    }
    else{
      ScalingExponent1 <- c(ScalingExponent1,NA)
    }
    
    if (any(is.nan(animalDFA2$fluctuation.function)==FALSE)){
      estimation2 <- estimate(animalDFA2,do.plot=FALSE)
      ScalingExponent2 <-c(ScalingExponent2,estimation2[1])
    }
    else{
      ScalingExponent2 <- c(ScalingExponent2,NA)
    }
    
    
    File <- c(File,animalData[,file_info,meta=T][[1]]$file)
    Labels <- c(Labels,animalData[,labels,meta= T])
    Channels <- c(Channels,animalData[,region_id,meta= T])
    Start_datetime <- c(Start_datetime,animalData[,start_datetime,meta= T])
    End_datetime <- c(End_datetime,animalData[,stop_datetime,meta= T])
  }
  
  #Scaling expoents results
  DAFdata$ScalingExponents <- data.frame(File, Labels, Channels, Start_datetime, End_datetime, ScalingExponent1, ScalingExponent2)
  
}

### Statistics tables
PeriodicStatisticsData <- function(){
  
  # Calculate periodogram data
  
    req(CircadianData$periodogram)
    CircadianData$periodogram[,period := period/3600]
    
    peaks <- CircadianData$periodogram[which(peak==1),]
    
    if (nrow(peaks)>0){
      
      File <- peaks[,File, meta=T]
      Labels <- peaks[,labels,meta=T]
      Channels <- peaks[,region_id,meta=T]
      Start_date <- peaks[,start_datetime,meta=T]
      End_date <- peaks[,stop_datetime,meta=T]
      # 
      # #Export data sheet
      aggregateData <- data.frame(File,Labels,Channels,Start_date,End_date, peaks[,2:5])
      # 
      Conditions <- aggregate(period ~ Labels, data=aggregateData, FUN=length)[,1]
      N  <- aggregate(period ~ Labels, data=aggregateData, FUN=length)[,2]
      Period_Mean <- round(aggregate(period ~ Labels, data=aggregateData, FUN=mean)[,2],3)
      Period_SEM <- round(aggregate(period ~ Labels, data=aggregateData, FUN=se)[,2],3)
      Period_SD <- round(aggregate(period ~ Labels, data=aggregateData, FUN=sd)[,2],3)
      Period_Median <- round(aggregate(period ~ Labels, data=aggregateData, FUN=median)[,2],3)
      Period_1st_Quartile <- round(aggregate(period ~ Labels, data=aggregateData, FUN=quantile)[,2][,2],3)
      Period_3rd_Quartile <- round(aggregate(period ~ Labels, data=aggregateData, FUN=quantile)[,2][,4],3)
      Power_Mean <- round(aggregate(power ~ Labels, data=aggregateData, FUN=mean)[,2],3)
      Power_SEM  <- round(aggregate(power ~ Labels, data=aggregateData, FUN=se)[,2],3)
      Power_SD  <- round(aggregate(power ~ Labels, data=aggregateData, FUN=sd)[,2],3)
      Power_Median  <- round(aggregate(power ~ Labels, data=aggregateData, FUN=median)[,2],3)
      Power_1st_Quartile  <- round(aggregate(power ~ Labels, data=aggregateData, FUN=quantile)[,2][,2],3)
      Power_3rd_Quartile  <- round(aggregate(power ~ Labels, data=aggregateData, FUN=quantile)[,2][,4],3)
      
      CircadianStatistics$periodogram <- data.frame(Conditions, N, Period_Mean, Period_SEM, Period_SD, Power_Mean, Power_SEM, Power_SD)
      CircadianStatistics$period <- data.frame(Conditions, N, Period_Mean, Period_SEM, Period_SD, Period_Median, Period_1st_Quartile, Period_3rd_Quartile)
      CircadianStatistics$power <- data.frame(Conditions, N, Power_Mean, Power_SEM, Power_SD, Power_Median, Power_1st_Quartile, Power_3rd_Quartile)
      }
    else{
      CircadianStatistics$periodogram <-  NULL
      CircadianStatistics$period <- NULL
      CircadianStatistics$power <- NULL
    }
  
}
DAFStatistics <- function(){
  req(DAFdata$ScalingExponents)
  
  Results <- DAFdata$ScalingExponents
  #######Fractal summary left
  Conditions <- aggregate(ScalingExponent1 ~ Labels, data=Results, FUN=mean)[,1]
  
  N  <- aggregate(ScalingExponent1 ~ Labels, data=Results, FUN=length)[,2]
  Data_Mean <- round(aggregate(ScalingExponent1 ~ Labels, data=Results, FUN=mean)[,2],3)
  Data_SEM <- round(aggregate(ScalingExponent1 ~ Labels, data=Results, FUN=se)[,2],3)
  Data_SD <- round(aggregate(ScalingExponent1 ~ Labels, data=Results, FUN=sd)[,2],3)
  Data_Median <- round(aggregate(ScalingExponent1 ~ Labels, data=Results, FUN=median)[,2],3)
  Data_1st_Quartile <- round(aggregate(ScalingExponent1 ~ Labels, data=Results, FUN=quantile)[,2][,2],3)
  Data_3rd_Quartile <- round(aggregate(ScalingExponent1 ~ Labels, data=Results, FUN=quantile)[,2][,4],3)
  
  Variable <- "Scaling exponent 1"
  fractal1 <- data.frame(Conditions, Variable, N, Data_Mean, Data_SEM,
                         Data_SD, Data_Median, Data_1st_Quartile,
                         Data_3rd_Quartile)
  
  
  #####Fractal summary right
  Conditions <- aggregate(ScalingExponent2 ~ Labels, data=Results, FUN=mean)[,1]
  
  N  <- aggregate(ScalingExponent2 ~ Labels, data=Results, FUN=length)[,2]
  Data_Mean <- round(aggregate(ScalingExponent2 ~ Labels, data=Results, FUN=mean)[,2],3)
  Data_SEM <- round(aggregate(ScalingExponent2 ~ Labels, data=Results, FUN=se)[,2],3)
  Data_SD <- round(aggregate(ScalingExponent2 ~ Labels, data=Results, FUN=sd)[,2],3)
  Data_Median <- round(aggregate(ScalingExponent2 ~ Labels, data=Results, FUN=median)[,2],3)
  Data_1st_Quartile <- round(aggregate(ScalingExponent2 ~ Labels, data=Results, FUN=quantile)[,2][,2],3)
  Data_3rd_Quartile <- round(aggregate(ScalingExponent2 ~ Labels, data=Results, FUN=quantile)[,2][,4],3)
  
  Variable <- "Scaling exponent 2"
  
  fractal2 <- data.frame(Conditions, Variable, N, Data_Mean, Data_SEM,
                         Data_SD, Data_Median, Data_1st_Quartile,
                         Data_3rd_Quartile)
  
  DAFstatisticsData(rbind(fractal1,fractal2))
}


# Relative amplitude calculus
RAmp <- function(data){
  
  M10 = max(roll(data, 10 * length(data)/l_period()))
  L5 = min(roll(data, 5 * length(data)/l_period()))
  relaamp = (M10 - L5)/(M10 + L5)
  return(relaamp)
}

roll = function(day.counts,k){
  kvec = rollapplyr(day.counts, k, function(x) mean(x,na.rm = T), fill = NA)
  kvec = kvec[!is.na(kvec)]
  return(kvec)
}

#Calculate acrophase, IV, RA and IS data
Phase_IV_RA_IS <- function() {
  req(damData$dt)
  
  #Channel ids
  ids <- unique(damData$dt[,id]) 
  
  #Variables to output
  Files <- c()
  Labels <- c()
  Order <- c()
  Channels <- c()
  Start_dates <- c()
  End_dates <- c()
  Day <- c()
  Acrophase <- c()
  ivData <- c()
  raData <- c()

  dataAct <- data.frame()
  ISactivity <- data.frame()
  isData <- data.frame()
  
  withProgress(message = 'Phase, IV, IS and RA analysis', value = 0, {
    for (i in 1:length(ids)){
      
      #Data from animal identified by id
      animalData <- damData$dt[damData$dt[,id] == ids[i],]
      #Days in the experiment
      experimentDays <- unique(animalData[,experimentDay])
  
      # Get data for each variable
      file <- animalData[,File,meta=T]
      label <- animalData[,labels,meta=T]
      order <- animalData[,order,meta=T]
      ch <- animalData[,region_id,meta=T]
      start_time <- animalData[,start_datetime,meta=T]
      end_time <- animalData[,stop_datetime,meta=T]
      
      ISactivity <- c()
      
      #For each day compute the acrophase and the 
      for (day in experimentDays){
      
        t <- animalData[which(animalData[,experimentDay]==day),periodT] #time
        activity <- animalData[which(animalData[,experimentDay]==day),activity] #activity
        
        dayTime <- max(t)-min(t) + (t[2]-t[1])
        
        #Only analyse data of complete days
        if (dayTime/86400 - round(dayTime/86400) == 0){
          #Get data for all day acrophases
          File <- file
          Label <- paste(label,order,sep = " - ")
          Channel <- ch
          Start_date <- as.character(start_time)
          End_date <- as.character(end_time)
          ID <- as.character(ids[i])
          row <- c(File, Label, Channel, Start_date, End_date, ID,day,activity)
          dataAct <- rbind(dataAct,row)
          
          ISactivity <- rbind(ISactivity,activity) #activity organized for IS calculus
          
          #Get data for daily acrophases
          Files <- c(Files, file)
          Labels <- c(Labels, paste(label,order,sep = " - "))
          Channels <- c(Channels, ch)
          Start_dates <- c(Start_dates, as.character(start_time))
          End_dates <- c(End_dates, as.character(end_time))
          Day <- c(Day,day)
          
          #Calculate acrophase
          acro <- tryCatch({ActExtendCosinor(activity, window = 1)$acrotime
          
          },error = function(e) {
            return(NA)
          }, finally ={})
          
          Acrophase <- c(Acrophase,acro)
          
          #Calculate intradaily variability and relative amplitude
          if (sum(activity)==0){
            ivData <- c(ivData, NA)
            raData <- c(raData, NA)
          }
          else{
            ivData <- c(ivData, IV(activity))
            raData <- c(raData, RAmp(activity))
          }
          
        }
      }
      #Calculate interdaily stabiliity
      if(nrow(ISactivity)>0){
        is <- IS(ISactivity)
      }
      else{
        is <- NA
      }
      label<-paste(label,order,sep=" - ")
      row <- data.frame(file,label, ch,start_time,end_time,is)
      isData <- rbind(isData,row)
      
      incProgress(1/length(ids))
    }
  
    #Interdaily stability column names
    colnames(isData) <- c("File","Labels","Channels","Start_dates","End_dates","IS")
    
    periodicData <- data.frame(Files,Labels, Channels,Start_dates,End_dates,Day,Acrophase,ivData,raData)
    colnames(periodicData)[8] <- "IV"
    colnames(periodicData)[9] <- "RA"
    
    ###### Circadian Phase #####
    colnames(dataAct)[6]<-'ID'
    colnames(dataAct)[7]<-'Day'
    
  
    #Turn activity columns into numeric
    for(i in 8:ncol(dataAct)){
      colN <- colnames(dataAct)
      dataAct[,colN[i]] <- as.numeric(dataAct[,colN[i]])
    }
    
    #Calculate acrophase of all data
    Acrophase <- ActExtendCosinor_long(count.data = dataAct[,6:ncol(dataAct)],window=1)
  
    phase <- cbind(dataAct[match(unique(dataAct[,'ID']),dataAct[,'ID']),1:6],Acrophase[,7])
    colnames(phase) <- c("File","Labels","Channels","Start_datetime","End_datetime","ID","Phase")
    phase[,'yPlot']  <- phase[,7]
    phase[,'xPlot']  <- phase[,2]
    phase[,'Data'] <- phase[,2]
    
    CircadianData$phase <- phase
    
    ##### Acrophase per day #####
    phasePerDay <- periodicData[,1:7]
    colnames(phasePerDay) <- c("File","Labels","Channels","Start_datetime","End_datetime","Day","Phase")
    
    phasePerDay[,'yPlot']  <- phasePerDay[,'Phase']
    phasePerDay[,'xPlot']  <- paste('Day',phasePerDay[,'Day'], sep = "_")
    phasePerDay[,'Data'] <- phasePerDay[,'Labels']
    
    CircadianData$phasePerDay <- phasePerDay
  
    #Get IV data
    # 
    intraDailyVariations <- cbind(periodicData[,1:6],periodicData[,8])
    colnames(intraDailyVariations) <- c("File","Labels","Channels","Start_datetime","End_datetime","Day","IV")
    
    intraDailyVariations[,'yPlot'] <- intraDailyVariations[,7]
    intraDailyVariations[,'xPlot'] <- intraDailyVariations[,'Labels']
    intraDailyVariations[,'Data'] <- intraDailyVariations[,'Labels']
    
    yPlot <- aggregate(yPlot ~ interaction(Files,Labels,Channels,sep = "_") , data=intraDailyVariations, FUN=mean)[,2]
    intraDailyVariations <- intraDailyVariations[which(Day == min(Day)),]
    intraDailyVariations[,'yPlot'] <- yPlot
  
    CircadianData$iv <- intraDailyVariations
  
    #IV per day
    intraDailyVariationsPerDay <- cbind(periodicData[,1:6],periodicData[,8])
    colnames(intraDailyVariationsPerDay) <- c("File","Labels","Channels","Start_datetime","End_datetime","Day","IV")
  
    intraDailyVariationsPerDay[,'yPlot'] <- intraDailyVariationsPerDay[,7]
    intraDailyVariationsPerDay[,'xPlot'] <- paste('Day',intraDailyVariationsPerDay[,'Day'],sep = "_")
    intraDailyVariationsPerDay[,'Data'] <- intraDailyVariationsPerDay[,'Labels']
    # 
    CircadianData$ivPerDay <- intraDailyVariationsPerDay
  
    
    #Get RA data
    # 
    relativeAmplitude <- cbind(periodicData[,1:6],periodicData[,9])
    colnames(relativeAmplitude) <- c("File","Labels","Channels","Start_datetime","End_datetime","Day","RA")
    
    relativeAmplitude[,'yPlot'] <- relativeAmplitude[,7]
    relativeAmplitude[,'xPlot'] <- relativeAmplitude[,'Labels']
    relativeAmplitude[,'Data'] <- relativeAmplitude[,'Labels']

    yPlot <- aggregate(yPlot ~ interaction(Files,Labels,Channels,sep = "_") , data=relativeAmplitude, FUN=mean)[,2]
    relativeAmplitude <- relativeAmplitude[which(Day == min(Day)),]
    relativeAmplitude[,'yPlot'] <- yPlot
    
    CircadianData$ra <- drop.levels(relativeAmplitude)
    
    #RA per day
    relativeAmplitudePerDay <- cbind(periodicData[,1:6],periodicData[,9])
    colnames(relativeAmplitudePerDay) <- c("File","Labels","Channels","Start_datetime","End_datetime","Day","RA")
    
    relativeAmplitudePerDay[,'yPlot'] <- relativeAmplitudePerDay[,7]
    relativeAmplitudePerDay[,'xPlot'] <- paste('Day',relativeAmplitudePerDay[,'Day'],sep = "_")
    relativeAmplitudePerDay[,'Data'] <- relativeAmplitudePerDay[,'Labels']
    # 
    CircadianData$raPerDay <- relativeAmplitudePerDay
    #
    
    #Get IS data
    # 
    interdailyStability <- isData
    interdailyStability[,'yPlot'] <- interdailyStability[,6]
    interdailyStability[,'xPlot'] <- interdailyStability[,'Labels']
    interdailyStability[,'Data'] <- interdailyStability[,'Labels']
    # 
    CircadianData$is <- interdailyStability
  })
}


##### Create and update Figures 
updatePeriodogramFig <- function(){
  
  req(CircadianData$periodogram)
  #Create periodogram
  fig<- ggperio(CircadianData$periodogram, aes(period, power, colour=Data, linetype = Data)) +
    stat_pop_etho() +
    scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values=graphsAestethics$df[,'lineType'])
  
  
  if (input$showPeriods){
    fig <- fig + geom_peak(peak_rank = 1)
  }
  
  #Add errors
  if (input$PeriodicError == "NA"){
    fig <- fig + scale_fill_manual(values = alpha(rep(NA, 100)), na.value = NA)
  }
  else{
    fig <- fig + stat_pop_etho(method = input$errorChronogram) + scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], 0))
  }
  
  fig <- fig + labs(title = CircadianFiguresTitles$periodogram,
                   x = CircadianFiguresXlabels$periodogram,
                   y = CircadianFiguresYlabels$periodogram)
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)
  
  
  CircadianFigures$periodogram <- fig
}
updatePeriodFig <- function(){
  
  fig <- statisticPlots(CircadianData$period ,input$PeriodicPlot,input$PeriodicError)+
    labs(title = CircadianFiguresTitles$period, x = CircadianFiguresXlabels$period,
         y = CircadianFiguresYlabels$period)
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)
  
  CircadianFigures$period <- fig
}
updatePowerFig <- function(){
  
  fig <- statisticPlots(CircadianData$power ,input$PeriodicPlot,input$PeriodicError)+
    labs(title = CircadianFiguresTitles$power, x = CircadianFiguresXlabels$power,
         y = CircadianFiguresYlabels$power)
  
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)
  
  CircadianFigures$power <- fig
}

updatePeriodogramFigures <- function(){
  req(CircadianData$period)
  updatePeriodogramFig()
  updatePeriodFig()
  updatePowerFig()
}

updatePhaseFig <- function(){
  
  fig <- statisticPlots(CircadianData$phase ,input$PeriodicPlot,input$PeriodicError)+
    labs(title = CircadianFiguresTitles$phase, x = CircadianFiguresXlabels$phase,
         y = CircadianFiguresYlabels$phase)
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)
  
  CircadianFigures$phase <- fig
}
updatePhasePerDayFig <- function(){
  
  fig <- statisticPlots(CircadianData$phasePerDay ,input$PeriodicPlot,input$PeriodicError)+
    labs(title = CircadianFiguresTitles$phasePerDay, x = CircadianFiguresXlabels$phasePerDay,
         y = CircadianFiguresYlabels$phasePerDay)
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)
  
  CircadianFigures$phasePerDay <- fig
}
updateIVFig <- function(){
  
  fig <- statisticPlots(CircadianData$iv ,input$PeriodicPlot,input$PeriodicError)+
    labs(title = CircadianFiguresTitles$iv, x = CircadianFiguresXlabels$iv,
         y = CircadianFiguresYlabels$iv)
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)
  
  CircadianFigures$iv <- fig
}
updateIVPerDayFig <- function(){
  
  fig <- statisticPlots(CircadianData$ivPerDay,input$PeriodicPlot,input$PeriodicError)+
    labs(title = CircadianFiguresTitles$ivPerDay, x = CircadianFiguresXlabels$ivPerDay,
         y = CircadianFiguresYlabels$ivPerDay)
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)
  
  CircadianFigures$ivPerDay <- fig
}
updateRAFig <- function(){
  
  fig <- statisticPlots(CircadianData$ra,input$PeriodicPlot,input$PeriodicError)+
    labs(title = CircadianFiguresTitles$ra, x = CircadianFiguresXlabels$ra,
         y = CircadianFiguresYlabels$ra)
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)
  
  CircadianFigures$ra <- fig
}
updateRAPerDayFig <- function(){
  
  fig <- statisticPlots(CircadianData$raPerDay,input$PeriodicPlot,input$PeriodicError)+
    labs(title = CircadianFiguresTitles$raPerDay, x = CircadianFiguresXlabels$raPerDay,
         y = CircadianFiguresYlabels$raPerDay)
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)
  
  CircadianFigures$raPerDay <- fig
}
updateISFig <- function(){
  
  fig <- statisticPlots(CircadianData$is,input$PeriodicPlot,input$PeriodicError)+
    labs(title = CircadianFiguresTitles$is, x = CircadianFiguresXlabels$is,
         y = CircadianFiguresYlabels$is)
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)
  
  CircadianFigures$is <- fig
}

updatePhaseFigures <- function(){
  req(CircadianData$phase)
  
  updatePhaseFig()
  updatePhasePerDayFig()
  updateIVFig()
  updateIVPerDayFig()
  updateRAFig()
  updateRAPerDayFig()
  updateISFig()
}

updateDAFfig <- function(){
  
  req(damData$dt)
  
  ##### Representation data
  
  ### Regression variables
  yInitial1 <- c()
  yFinal1 <- c()
  xInitial1 <- c()
  xFinal1 <- c()
  
  yInitial2 <- c()
  yFinal2 <- c()
  xInitial2 <- c()
  xFinal2 <- c()
  
  uniqueLabels <- unlist(unique(damData$dt[,'labels',meta=T]))
  uniqueData <- unlist(unique(damData$dt[,'Data',meta=T]))
  
  Labels1 <- c()
  xPlot1 <- c()
  yPlot1 <- c()
  
  Labels2 <- c()
  xPlot2 <- c()
  yPlot2 <- c()
  
  for (i in 1:length(uniqueLabels)){
    #Mean activity scaling expoent
    animalData <- damData$dt[damData$dt[,labels] == uniqueLabels[i] & day_night == TRUE,]
    activityPerLabels <- aggregate(activity ~ interaction(labels,t),
                                   data = animalData,
                                   FUN = mean)
    
    #Note use the mean activity for detrended  fluctuation analysis
    dfa.analysis <- dfa(time.series = activityPerLabels[,'activity'],npoints = input$nFractal,
                        window.size.range=c(mins(input$Fractal_limits[1]),mins(input$TimeScale_limits)), do.plot=FALSE)
    
    Labels1 <- c(Labels1,rep(as.character(uniqueData[i]),input$nFractal))
    xPlot1 <- c(xPlot1,dfa.analysis$window.sizes)
    yPlot1 <- c(yPlot1,dfa.analysis$fluctuation.function)
    
    ### Regression1
    estimation1 <- estimate(dfa.analysis,do.plot=FALSE)
    EstimateAttr1 <- attributes(estimation1)$fitted
    xInitial1 <- c(xInitial1,EstimateAttr1$x[1])
    yInitial1 <- c(yInitial1,EstimateAttr1$y[1])
    xFinal1 <- c(xFinal1,EstimateAttr1$x[length(EstimateAttr1$x)])
    yFinal1 <- c(yFinal1,EstimateAttr1$y[length(EstimateAttr1$y)])
    
    dfa.analysis <- dfa(time.series = activityPerLabels[,'activity'],npoints = input$nFractal,
                        window.size.range=c(mins(input$TimeScale_limits),mins(input$Fractal_limits[2])), do.plot=FALSE)
    
    Labels2 <- c(Labels2,rep(as.character(uniqueData[i]),input$nFractal))
    xPlot2 <- c(xPlot2,dfa.analysis$window.sizes)
    yPlot2 <- c(yPlot2,dfa.analysis$fluctuation.function)
    
    ### Regression2
    estimation2 <- estimate(dfa.analysis,do.plot=FALSE)
    EstimateAttr2 <- attributes(estimation2)$fitted
    xInitial2 <- c(xInitial2,EstimateAttr2$x[1])
    yInitial2 <- c(yInitial2,EstimateAttr2$y[1])
    xFinal2 <- c(xFinal2,EstimateAttr2$x[length(EstimateAttr2$x)])
    yFinal2 <- c(yFinal2,EstimateAttr2$y[length(EstimateAttr2$y)])
  }
  
  Labels <- c(Labels1,Labels2)
  xPlot <- c(xPlot1, xPlot2)
  yPlot <- c(yPlot1, yPlot2)
  
  RepresentationData <- data.frame(Labels, xPlot, yPlot)
  
  
  #Regression lines
  xInitial <- c(xInitial1, xInitial2)/60
  xFinal <- c(xFinal1,xFinal2)/60
  yInitial <- c(yInitial1, yInitial2)
  yFinal <- c(yFinal1, yFinal2)
  
  RegressionLimits <- data.frame(xInitial,xFinal,yInitial,yFinal)
  
  fig <- ggplot(RepresentationData,aes(x=xPlot/60,y=yPlot,colour = Labels, na.rm=TRUE))+
    geom_point(size=1) +
    geom_segment(aes(x = xInitial, y = yInitial,
                     xend = xFinal, yend = yFinal),data = RegressionLimits,
                 colour = rep(graphsAestethics$df[,'lineColor'],2))+
    scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], .8))+
    scale_colour_manual(values = graphsAestethics$df[,'lineColor'])
  
  fig <- whiteBackground(fig, input$titleLetterSize4, input$axisLabelSize4,
                         input$axisNumbersSize4, input$dataLabelSize4)+
    labs(title = DAFfigureTitle(), x = DAFfigureXlabel(), y = DAFfigureYlabel())
  
  DAFfigure(fig)
  
  
}

#####Update Y axis range
updateYPeriodogram <- function() {
  
  if (input$periodogramTabs == "Periodogram"){
    CircadianFigures$periodogram <- CircadianFigures$periodogram + coord_cartesian(ylim = input$yLimitsPeriod)
  }
  else{
    if (input$periodogramTabs == "Period"){
      CircadianFigures$period <- CircadianFigures$period + coord_cartesian(ylim = input$yLimitsPeriod)
    }
    else{
      CircadianFigures$power <- CircadianFigures$power + coord_cartesian(ylim = input$yLimitsPeriod)
    }
  }
}
updateYPhase <- function() {
  
  if (input$phaseTabs == "Phase"){
    CircadianFigures$phase <- CircadianFigures$phase + coord_cartesian(ylim = input$yLimitsPhase)
  }
  else{
    if (input$phaseTabs == "Phase per day"){
      CircadianFigures$phasePerDay <- CircadianFigures$phasePerDay + coord_cartesian(ylim = input$yLimitsPhase)
    }
    else{
      if(input$phaseTabs == "Interdaily stability"){
        CircadianFigures$is <- CircadianFigures$is + coord_cartesian(ylim = input$yLimitsPhase)
      }
      else{
        if(input$phaseTabs == "Intradaily variation"){
          CircadianFigures$iv <- CircadianFigures$iv + coord_cartesian(ylim = input$yLimitsPhase)
        }
        else{
          if(input$phaseTabs == "Intradaily variation per day"){
            CircadianFigures$ivPerDay <- CircadianFigures$ivPerDay + coord_cartesian(ylim = input$yLimitsPhase)
          }
          else{
            if(input$phaseTabs == "Relative amplitude"){
              CircadianFigures$ra <- CircadianFigures$ra + coord_cartesian(ylim = input$yLimitsPhase)
            }
            else{
              CircadianFigures$raPerDay <- CircadianFigures$raPerDay + coord_cartesian(ylim = input$yLimitsPhase)
            }
          }
        }
      }
    }
  }
}

#Update Labels
updatePeriodogramLabels <- function() {
  
  if (input$periodogramTabs == "Periodogram"){
    CircadianFiguresTitles$periodogram <- input$graphTitlePeriod
    CircadianFiguresYlabels$periodogram <- input$yLabelPeriod
    CircadianFiguresXlabels$periodogram <- input$xLabelPeriod    
    }
  else{
    if (input$periodogramTabs == "Period"){
      CircadianFiguresTitles$period <- input$graphTitlePeriod
      CircadianFiguresYlabels$period <- input$yLabelPeriod
      CircadianFiguresXlabels$period <- input$xLabelPeriod        
      }
    else{
      CircadianFiguresTitles$power <- input$graphTitlePeriod
      CircadianFiguresYlabels$power <- input$yLabelPeriod
      CircadianFiguresXlabels$power <- input$xLabelPeriod    
      }
  }
}
updatePhaseLabels <- function(){
  
  if (input$phaseTabs == "Phase"){
    CircadianFiguresTitles$phase <- input$graphTitlePhase
    CircadianFiguresYlabels$phase <- input$yLabelPhase
    CircadianFiguresXlabels$phase <- input$xLabelPhase  
  }
  else{
    if (input$phaseTabs == "Phase per day"){
      CircadianFiguresTitles$phasePerDay <- input$graphTitlePhase
      CircadianFiguresYlabels$phasePerDay <- input$yLabelPhase
      CircadianFiguresXlabels$phasePerDay <- input$xLabelPhase  
    }
    else{
      if(input$phaseTabs == "Interdaily stability"){
        CircadianFiguresTitles$is <- input$graphTitlePhase
        CircadianFiguresYlabels$is <- input$yLabelPhase
        CircadianFiguresXlabels$is <- input$xLabelPhase
      }
      else{
        if(input$phaseTabs == "Intradaily variation"){
          CircadianFiguresTitles$iv <- input$graphTitlePhase
          CircadianFiguresYlabels$iv <- input$yLabelPhase
          CircadianFiguresXlabels$iv <- input$xLabelPhase
        }
        else{
          if(input$phaseTabs == "Intradaily variation per day"){
            CircadianFiguresTitles$ivPerDay <- input$graphTitlePhase
            CircadianFiguresYlabels$ivPerDay <- input$yLabelPhase
            CircadianFiguresXlabels$ivPerDay <- input$xLabelPhase 
          }
          else{
            if(input$phaseTabs == "Relative amplitude"){
              CircadianFiguresTitles$ra <- input$graphTitlePhase
              CircadianFiguresYlabels$ra <- input$yLabelPhase
              CircadianFiguresXlabels$ra <- input$xLabelPhase   
            }
            else{
              CircadianFiguresTitles$raPerDay <- input$graphTitlePhase
              CircadianFiguresYlabels$raPerDay <- input$yLabelPhase
              CircadianFiguresXlabels$raPerDay <- input$xLabelPhase         
            }
          }
        }
      }
    }
  }
}
updateFractalLabels <- function() {
  
  DAFfigureTitle(input$graphTitleFractal)
  DAFfigureXlabel(input$xLabelFractal)
  DAFfigureYlabel(input$yLabelFractal)
}

############################# Data analysis #################################

##################### Restart variables #####################
observeEvent(input$files,{
  
  #####RESET variables
  CircadianData$periodogram <- NULL
  CircadianData$period <- NULL
  CircadianData$power <- NULL
  CircadianData$phase <- NULL
  CircadianData$phasePerDay <- NULL
  CircadianData$iv <- NULL
  CircadianData$ivPerDay <- NULL
  CircadianData$ra <- NULL
  CircadianData$raPerDay <- NULL
  CircadianData$is <- NULL
  DAFdata$ScalingExponents <-NULL
  
  CircadianStatistics$periodogram <- NULL
  CircadianStatistics$period <- NULL
  CircadianStatistics$power <- NULL
  CircadianStatistics$phase <- NULL
  CircadianStatistics$phasePerDay <- NULL
  CircadianStatistics$iv <- NULL
  CircadianStatistics$ivPerDay <- NULL
  CircadianStatistics$ra <- NULL
  CircadianStatistics$raPerDay <- NULL
  CircadianStatistics$is <- NULL
  DAFstatisticsData(NULL)
  
  CircadianFigures$periodogram <- NULL
  CircadianFigures$period <- NULL
  CircadianFigures$power <- NULL
  CircadianFigures$phase <- NULL
  CircadianFigures$phasePerDay <- NULL
  CircadianFigures$iv <- NULL
  CircadianFigures$ivPerDay <- NULL
  CircadianFigures$ra <- NULL
  CircadianFigures$raPerDay <- NULL
  CircadianFigures$is <- NULL
  DAFfigure(NULL)
  
  CircadianFiguresTitles$periodogram <- "Periodogram"
  CircadianFiguresTitles$period <- "Period"
  CircadianFiguresTitles$power <- "Periodogram power"
  CircadianFiguresTitles$phase <- "Activity phase"
  CircadianFiguresTitles$phasePerDay <- "Activity phase"
  CircadianFiguresTitles$iv <- "Intradaily Variation"
  CircadianFiguresTitles$ivPerDay <- "Intradaily Variation"
  CircadianFiguresTitles$ra <- "Relative Amplitude"
  CircadianFiguresTitles$raPerDay <- "Relative Amplitude"
  CircadianFiguresTitles$is <- "Interdaily Stability"
  DAFfigureTitle("Fractal analysis")
  
  CircadianFiguresXlabels$periodogram <- "Period (h)"
  CircadianFiguresXlabels$period <- NULL
  CircadianFiguresXlabels$power <- NULL
  CircadianFiguresXlabels$phase <- NULL
  CircadianFiguresXlabels$phasePerDay <- "Time"
  CircadianFiguresXlabels$iv <- NULL
  CircadianFiguresXlabels$ivPerDay <- "Time"
  CircadianFiguresXlabels$ra <- NULL
  CircadianFiguresXlabels$raPerDay <- "Time"
  CircadianFiguresXlabels$is <- NULL
  DAFfigureXlabel("Time scale n (h)")
  
  CircadianFiguresYlabels$periodogram <- "Power"
  CircadianFiguresYlabels$period <- "Period (h)"
  CircadianFiguresYlabels$power <- "Power"
  CircadianFiguresYlabels$phase <- "Acrophase"
  CircadianFiguresYlabels$phasePerDay <- "Acrophase"
  CircadianFiguresYlabels$iv <- "Intradaily Variation"
  CircadianFiguresYlabels$ivPerDay <- "Intradaily Variation"
  CircadianFiguresYlabels$ra <- "Relative Amplitude"
  CircadianFiguresYlabels$raPerDay <- "Relative Amplitude"
  CircadianFiguresYlabels$is <- "Interdaily Stability"
  DAFfigureYlabel("Detrending fluctuation F(n)")
  
})

observeEvent(input$startanalysis,{
  #####RESET variables
  CircadianData$periodogram <- NULL
  CircadianData$period <- NULL
  CircadianData$power <- NULL
  CircadianData$phase <- NULL
  CircadianData$phasePerDay <- NULL
  CircadianData$iv <- NULL
  CircadianData$ivPerDay <- NULL
  CircadianData$ra <- NULL
  CircadianData$raPerDay <- NULL
  CircadianData$is <- NULL
  DAFdata$ScalingExponents <-NULL
  
  CircadianStatistics$periodogram <- NULL
  CircadianStatistics$period <- NULL
  CircadianStatistics$power <- NULL
  CircadianStatistics$phase <- NULL
  CircadianStatistics$phasePerDay <- NULL
  CircadianStatistics$iv <- NULL
  CircadianStatistics$ivPerDay <- NULL
  CircadianStatistics$ra <- NULL
  CircadianStatistics$raPerDay <- NULL
  CircadianStatistics$is <- NULL
  DAFstatisticsData(NULL)
  
  CircadianFigures$periodogram <- NULL
  CircadianFigures$period <- NULL
  CircadianFigures$power <- NULL
  CircadianFigures$phase <- NULL
  CircadianFigures$phasePerDay <- NULL
  CircadianFigures$iv <- NULL
  CircadianFigures$ivPerDay <- NULL
  CircadianFigures$ra <- NULL
  CircadianFigures$raPerDay <- NULL
  CircadianFigures$is <- NULL
  DAFfigure(NULL)
  
  CircadianFiguresTitles$periodogram <- "Periodogram"
  CircadianFiguresTitles$period <- "Period"
  CircadianFiguresTitles$power <- "Periodogram power"
  CircadianFiguresTitles$phase <- "Activity phase"
  CircadianFiguresTitles$phasePerDay <- "Activity phase"
  CircadianFiguresTitles$iv <- "Intradaily Variation"
  CircadianFiguresTitles$ivPerDay <- "Intradaily Variation"
  CircadianFiguresTitles$ra <- "Relative Amplitude"
  CircadianFiguresTitles$raPerDay <- "Relative Amplitude"
  CircadianFiguresTitles$is <- "Interdaily Stability"
  DAFfigureTitle("Fractal analysis")
  
  CircadianFiguresXlabels$periodogram <- "Period (h)"
  CircadianFiguresXlabels$period <- NULL
  CircadianFiguresXlabels$power <- NULL
  CircadianFiguresXlabels$phase <- NULL
  CircadianFiguresXlabels$phasePerDay <- "Time"
  CircadianFiguresXlabels$iv <- NULL
  CircadianFiguresXlabels$ivPerDay <- "Time"
  CircadianFiguresXlabels$ra <- NULL
  CircadianFiguresXlabels$raPerDay <- "Time"
  CircadianFiguresXlabels$is <- NULL
  DAFfigureXlabel("Time scale n (h)")
  
  CircadianFiguresYlabels$periodogram <- "Power"
  CircadianFiguresYlabels$period <- "Period (h)"
  CircadianFiguresYlabels$power <- "Power"
  CircadianFiguresYlabels$phase <- "Acrophase"
  CircadianFiguresYlabels$phasePerDay <- "Acrophase"
  CircadianFiguresYlabels$iv <- "Intradaily Variation"
  CircadianFiguresYlabels$ivPerDay <- "Intradaily Variation"
  CircadianFiguresYlabels$ra <- "Relative Amplitude"
  CircadianFiguresYlabels$raPerDay <- "Relative Amplitude"
  CircadianFiguresYlabels$is <- "Interdaily Stability"
  DAFfigureYlabel("Detrending fluctuation F(n)")
})

#Update figures if data is deleted
observeEvent(input$deleteAnimals,{
  
  #####RESET variables
  CircadianData$periodogram <- NULL
  CircadianData$period <- NULL
  CircadianData$power <- NULL
  CircadianData$phase <- NULL
  CircadianData$phasePerDay <- NULL
  CircadianData$iv <- NULL
  CircadianData$ivPerDay <- NULL
  CircadianData$ra <- NULL
  CircadianData$raPerDay <- NULL
  CircadianData$is <- NULL
  DAFdata$ScalingExponents <-NULL
  
  CircadianStatistics$periodogram <- NULL
  CircadianStatistics$period <- NULL
  CircadianStatistics$power <- NULL
  CircadianStatistics$phase <- NULL
  CircadianStatistics$phasePerDay <- NULL
  CircadianStatistics$iv <- NULL
  CircadianStatistics$ivPerDay <- NULL
  CircadianStatistics$ra <- NULL
  CircadianStatistics$raPerDay <- NULL
  CircadianStatistics$is <- NULL
  DAFstatisticsData(NULL)
  
  CircadianFigures$periodogram <- NULL
  CircadianFigures$period <- NULL
  CircadianFigures$power <- NULL
  CircadianFigures$phase <- NULL
  CircadianFigures$phasePerDay <- NULL
  CircadianFigures$iv <- NULL
  CircadianFigures$ivPerDay <- NULL
  CircadianFigures$ra <- NULL
  CircadianFigures$raPerDay <- NULL
  CircadianFigures$is <- NULL
  DAFfigure(NULL)
  
})
observeEvent(input$deleteInactivity,{
  
  #####RESET variables
  CircadianData$periodogram <- NULL
  CircadianData$period <- NULL
  CircadianData$power <- NULL
  CircadianData$phase <- NULL
  CircadianData$phasePerDay <- NULL
  CircadianData$iv <- NULL
  CircadianData$ivPerDay <- NULL
  CircadianData$ra <- NULL
  CircadianData$raPerDay <- NULL
  CircadianData$is <- NULL
  DAFdata$ScalingExponents <-NULL
  
  CircadianStatistics$periodogram <- NULL
  CircadianStatistics$period <- NULL
  CircadianStatistics$power <- NULL
  CircadianStatistics$phase <- NULL
  CircadianStatistics$phasePerDay <- NULL
  CircadianStatistics$iv <- NULL
  CircadianStatistics$ivPerDay <- NULL
  CircadianStatistics$ra <- NULL
  CircadianStatistics$raPerDay <- NULL
  CircadianStatistics$is <- NULL
  DAFstatisticsData(NULL)
  
  CircadianFigures$periodogram <- NULL
  CircadianFigures$period <- NULL
  CircadianFigures$power <- NULL
  CircadianFigures$phase <- NULL
  CircadianFigures$phasePerDay <- NULL
  CircadianFigures$iv <- NULL
  CircadianFigures$ivPerDay <- NULL
  CircadianFigures$ra <- NULL
  CircadianFigures$raPerDay <- NULL
  CircadianFigures$is <- NULL
  DAFfigure(NULL)
  
})

######################### Start analysis ###############################

#Change periodogram data
observeEvent(input$periodogramAnalysis,{
  req(damData$dt)
  
  settings <- settingsTable()
  settings[6,2] <- input$perFun
  settings[7,2] <- input$periodogramValue
  settings[8,2] <- input$minPer
  settings[9,2] <- input$maxPer
  settingsTable(settings)
  
  withProgress(message = 'Periodogram analysis', value = 0, {
    #Data analysis
    PeriodogramData()
    incProgress(0.75)
    PeriodAndPower()
    incProgress(0.05)
    
    #Compute figures
    updatePeriodogramFigures()
    incProgress(0.1)
    PeriodicStatisticsData()
    incProgress(0.1)
  })
})

observeEvent(input$phaseAnalysis,{
  req(damData$dt)
  
  withProgress(message = 'Phase analysis', value = 0, {
    #Data analysis
    Phase_IV_RA_IS()
    incProgress(0.85)
    
    #Compute figures
    updatePhaseFigures()
    incProgress(0.15)
  })
})

observeEvent(input$fractalAnalysis,{
  req(damData$dt)
  
  settings <- settingsTable()
  
  settings[14] <- paste(input$Fractal_limits[1],input$TimeScale_limits,sep = " - ")
  settings[15] <- paste(input$TimeScale_limits, input$Fractal_limits[2],sep = " - ")
  settings[16] <- input$nFractal
  
  withProgress(message = 'Fractal analysis', value = 0, {
    #Data analysis
    DAFsummary()
    incProgress(0.75)
    
    #Compute figures
    updateDAFfig()
    incProgress(0.1)
    DAFStatistics()
    incProgress(0.15)
  })
})


############################ Change labels ###############################

#Phase, IV or RA
observeEvent(input$PhaseSummary_cell_edit,{
  
  info <- input$PhaseSummary_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  
  str_replace(v, " ", "_")
  
  changeLabels <- function(phase){
    
    conditions <- aggregate(yPlot ~ interaction(Labels, xPlot,sep = " -- "), data=phase, FUN=mean)[,1]
    Label <-  str_split_fixed(conditions, " -- ",2)[,2]
  
    for (k in 1:nrow(phase)){
      if (phase[k,"xPlot"]==Label[i]){
        phase[k,"xPlot"] <- v
      }
    }
    return(phase)
  }
  
  if (input$phaseTabs == "Phase per day"){
    CircadianData$phasePerDay <- changeLabels(CircadianData$phasePerDay)
    updatePhasePerDayFig()
  }
  else{
    if (input$phaseTabs == "Intradaily variation per day"){
      CircadianData$ivPerDay <- changeLabels(CircadianData$ivPerDay)
      updateIVPerDayFig()
    }
    else{
      if(input$phaseTabs == "Relative amplitude per day"){
        CircadianData$raPerDay <- changeLabels(CircadianData$raPerDay)
        updateRAPerDayFig()
      }
    }
  }
  
})


#################### Update statistical data presented ###################

#Periodogram table
observe({
  
  req(nrow(damData$dt)>0)
  req(nrow(CircadianData$periodogram)>0)
  
  minimum  <- floor(min(CircadianData$power[,'yPlot']))
  maximum <- ceiling(max(CircadianData$power[,'yPlot']))
  step <- 0.1
  
  if (input$periodogramTabs == "Periodogram"){
    period <- CircadianStatistics$periodogram
  }
  else{
    if (input$periodogramTabs == "Period"){
      period <- CircadianStatistics$period
      
      minimum  <- floor(min(CircadianData$period[,'yPlot']))
      maximum <- ceiling(max(CircadianData$period[,'yPlot']))
      step <- 1
    }
    else{
      period <- CircadianStatistics$power
    }
  }
  
  updateSliderInput(session,"yLimitsPeriod", min = minimum, max = maximum, value = c(minimum,maximum),step = step)
  
  output$PeriodogramSummary <- DT::renderDataTable(period,
                                             escape = FALSE, selection = 'none', 
                                             editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
})

#Phase table
observe({
  
  req(nrow(damData$dt)>0)
  req(nrow(CircadianData$phase)>0)
  
  x <- TRUE
  if (input$phaseTabs == "Phase"){
    phase <- CircadianData$phase
  }
  else{
    if (input$phaseTabs == "Phase per day"){
      phase <- CircadianData$phasePerDay
      x<-FALSE
      }
    else{
      if(input$phaseTabs == "Interdaily stability"){
        phase <- CircadianData$is
        }
      else{
        if(input$phaseTabs == "Intradaily variation"){
          phase <- CircadianData$iv
        }
        else{
          if(input$phaseTabs == "Intradaily variation per day"){
            phase <- CircadianData$ivPerDay
            x<-FALSE
          }
          else{
            if(input$phaseTabs == "Relative amplitude"){
              phase <- CircadianData$ra
            }
            else{
              phase <- CircadianData$raPerDay
              x<-FALSE
            }
          }
        }
      }
    }
  }
  
  updateSliderInput(session,"yLimitsPhase", min = floor(min(phase[,'yPlot'])), max = ceiling(max(phase[,'yPlot'])), value = c(floor(min(phase[,'yPlot'])),ceiling(max(phase[,'yPlot']))))
  
  output$PhaseSummary <- DT::renderDataTable(PeriodogramReport(phase,x),
                                                escape = FALSE, selection = 'none',
                                                editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
})

#Fractal table
observe({
  req(nrow(damData$dt)>0)
  req(nrow(DAFstatisticsData())>0)
  
  output$FractalSummary <- DT::renderDataTable(DAFstatisticsData(),
                                                 escape = FALSE, selection = 'none', 
                                                 editable  = list(target = 'cell',disable = list(columns = c(1,3,4,5,6,7,8,9))))
})

####### Periodogram UI #####

observeEvent(input$perFun,{
  if (input$perFun == "ls"){
    updateSliderInput(session, 'periodogramValue', label= "Oversampling", value  = 32, min = 2, max = 128, step = 1)
  }
  if (input$perFun == "chi-sq"){
    updateSliderInput(session, 'periodogramValue', label= "Time resolution", value  = 0.1, min = 0.05, max = 1, step = 0.05)
  }
})


############################# Plots #############################

#Update graphs
observeEvent(input$updateAesthetics4,{
  
  updatePeriodogramLabels()
  updatePhaseLabels()
  updateFractalLabels()
  
  withProgress(message = 'Update data and graphs', value = 0, {
    
    if(!is.null(CircadianFigures$periodogram)){
      updateYPeriodogram()
      updatePeriodogramFigures()
      incProgress(0.2)
      PeriodicStatisticsData()
      incProgress(0.2)
    }
    else{
      incProgress(0.4)
    }
    
    #Compute figures
    if(!is.null(CircadianFigures$phase)){
      updateYPhase()
      updatePhaseFigures()
    }
    incProgress(0.2)
    
    #Compute figures
    if(!is.null(DAFfigure())){
      updateDAFfig()
      incProgress(0.2)
      DAFStatistics()
      incProgress(0.2)
    }
    else{
      incProgress(0.4)
    }
  })
  
})

#Update Text fields
observeEvent(input$periodogramTabs,{
  
  if (input$periodogramTabs == "Periodogram"){
    updateTextInput(session,"graphTitlePeriod", value = CircadianFiguresTitles$periodogram)
    updateTextInput(session, "xLabelPeriod", value = CircadianFiguresXlabels$periodogram)
    updateTextInput(session, "yLabelPeriod", value = CircadianFiguresYlabels$periodogram)
  }
  else{
    if (input$periodogramTabs == "Period"){
      updateTextInput(session,"graphTitlePeriod", value = CircadianFiguresTitles$period)
      updateTextInput(session, "xLabelPeriod", value = CircadianFiguresXlabels$period)
      updateTextInput(session, "yLabelPeriod", value = CircadianFiguresYlabels$period)
    }
    else{
      updateTextInput(session,"graphTitlePeriod", value = CircadianFiguresTitles$power)
      updateTextInput(session, "xLabelPeriod", value = CircadianFiguresXlabels$power)
      updateTextInput(session, "yLabelPeriod", value = CircadianFiguresYlabels$power)
    }
  }
})
observeEvent(input$phaseTabs,{
  
  if (input$phaseTabs == "Phase"){
    updateTextInput(session,"graphTitlePhase", value = CircadianFiguresTitles$phase)
    updateTextInput(session, "xLabelPhase", value = CircadianFiguresXlabels$phase)
    updateTextInput(session, "yLabelPhase", value = CircadianFiguresYlabels$phase)  
    }
  else{
    if (input$phaseTabs == "Phase per day"){
      updateTextInput(session,"graphTitlePhase", value = CircadianFiguresTitles$phasePerDay)
      updateTextInput(session, "xLabelPhase", value = CircadianFiguresXlabels$phasePerDay)
      updateTextInput(session, "yLabelPhase", value = CircadianFiguresYlabels$phasePerDay)
      }
    else{
      if(input$phaseTabs == "Interdaily stability"){
        updateTextInput(session,"graphTitlePhase", value = CircadianFiguresTitles$is)
        updateTextInput(session, "xLabelPhase", value = CircadianFiguresXlabels$is)
        updateTextInput(session, "yLabelPhase", value = CircadianFiguresYlabels$is)   
        }
      else{
        if(input$phaseTabs == "Intradaily variation"){
          updateTextInput(session,"graphTitlePhase", value = CircadianFiguresTitles$iv)
          updateTextInput(session, "xLabelPhase", value = CircadianFiguresXlabels$iv)
          updateTextInput(session, "yLabelPhase", value = CircadianFiguresYlabels$iv)
          }
        else{
          if(input$phaseTabs == "Intradaily variation per day"){
            updateTextInput(session,"graphTitlePhase", value = CircadianFiguresTitles$ivPerDay)
            updateTextInput(session, "xLabelPhase", value = CircadianFiguresXlabels$ivPerDay)
            updateTextInput(session, "yLabelPhase", value = CircadianFiguresYlabels$ivPerDay)          }
          else{
            if(input$phaseTabs == "Relative amplitude"){
              updateTextInput(session,"graphTitlePhase", value = CircadianFiguresTitles$ra)
              updateTextInput(session, "xLabelPhase", value = CircadianFiguresXlabels$ra)
              updateTextInput(session, "yLabelPhase", value = CircadianFiguresYlabels$ra)               }
            else{
              updateTextInput(session,"graphTitlePhase", value = CircadianFiguresTitles$raPerDay)
              updateTextInput(session, "xLabelPhase", value = CircadianFiguresXlabels$raPerDay)
              updateTextInput(session, "yLabelPhase", value = CircadianFiguresYlabels$raPerDay)               }
          }
        }
      }
    }
  }
})
observeEvent(input$fractalAnalysis,{
  
  updateTextInput(session,"graphTitleFractal", value = DAFfigureTitle())
  updateTextInput(session, "xLabelFractal", value = DAFfigureXlabel())
  updateTextInput(session, "yLabelFractal", value = DAFfigureYlabel())  
  
})

## Periodogram
output$periodogram <- renderPlot({

  req(nrow(damData$dt)>0)
  validate(
    need (nrow(CircadianData$periodogram)>0,"No data selected")
  )
  req(!is.null(CircadianFigures$periodogram))

  fig <- CircadianFigures$periodogram

  return(fig)

}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)

##Period graph
output$period <- renderPlot({
  
  validate(
    need (!is.null(CircadianFigures$period),"No data selected")
  )
  req(!is.null(CircadianFigures$period))
  
  fig <- CircadianFigures$period
  
  return(fig)
  
}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)

##Power graph
output$power <- renderPlot({
  
  validate(
    need (!is.null(CircadianFigures$power),"No data selected")
  )
  req(!is.null(CircadianFigures$power))
  
  fig <- CircadianFigures$power
  
  return(fig)
  
}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)

##Phase graph
output$phase <- renderPlot({
  
  validate(
    need (!is.null(CircadianFigures$phase),"No data selected")
  )
  req(!is.null(CircadianFigures$phase))
  
  fig <- CircadianFigures$phase
  
  return(fig)
  
}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)

##Phase per day graph
output$phasePerDay <- renderPlot({
  
  validate(
    need (!is.null(CircadianFigures$phasePerDay),"No data selected")
  )
  req(!is.null(CircadianFigures$phasePerDay))
  
  fig <- CircadianFigures$phasePerDay
  
  return(fig)
  
}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)

##Interdaily stability graph
output$is <- renderPlot({
  
  validate(
    need (!is.null(CircadianFigures$is),"No data selected")
  )
  req(!is.null(CircadianFigures$is))
  
  fig <- CircadianFigures$is
  
  return(fig)
  
}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)

##Intradaily variation graph
output$iv <- renderPlot({
  
  validate(
    need (!is.null(CircadianFigures$iv),"No data selected")
  )
  req(!is.null(CircadianFigures$iv))
  
  fig <- CircadianFigures$iv
  
  return(fig)
  
}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)

##Intradaily variation per day graph
output$ivPerDay <- renderPlot({
  
  validate(
    need (!is.null(CircadianFigures$ivPerDay),"No data selected")
  )
  req(!is.null(CircadianFigures$ivPerDay))
  
  fig <- CircadianFigures$ivPerDay
  
  return(fig)
  
}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)

##Relative amplitude graph
output$ra <- renderPlot({
  
  validate(
    need (!is.null(CircadianFigures$ra),"No data selected")
  )
  req(!is.null(CircadianFigures$ra))
  
  fig <- CircadianFigures$ra
  
  return(fig)
  
}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)

##Relative amplitude per day graph
output$raPerDay <- renderPlot({
  
  validate(
    need (!is.null(CircadianFigures$raPerDay),"No data selected")
  )
  req(!is.null(CircadianFigures$raPerDay))
  
  fig <- CircadianFigures$raPerDay
  
  return(fig)
  
}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)


output$fractalAnalysis <- renderPlot({
  
  req(DAFfigure())
  
  fig <- DAFfigure()
  
  return(fig)
}, width = function() input$periodWidth,
height = function() input$periodHeight,
res = 96)


######################### Statistics table #########################
#Data
observe({
  
  req(nrow(damData$dt)>0)
  req(CircadianData$periodogram)
  output$savePeriodogramData <- downloadHandler(
    
    filename = function(){
      paste0("Periodogram",'.xlsx')
    },
    content = function(file){
      
      wb <- createWorkbook(type="xlsx")
  
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
  
      sheet <- createSheet(wb, "Period Statistics")
      addDataFrame(CircadianStatistics$period, sheet=sheet, startColumn=1, row.names=FALSE)
      
      sheet <- createSheet(wb, "Power Statistics")
      addDataFrame(CircadianStatistics$power, sheet=sheet, startColumn=1, row.names=FALSE)
  
      sheet <- createSheet(wb, "All channels")
      data <- CircadianData$periodogram[which(peak==1),]
      metadata <- data[,,meta=T]
      Files <- metadata[,File]
      Labels <- metadata[,labels]
      Channels <- metadata[,region_id]
      Start_datetime <- metadata[,start_datetime]
      End_datetime <- metadata[,stop_datetime]
      
      exportAllChannels <- data.frame(Files,Labels,Channels,Start_datetime,End_datetime,data[,2:5])
      addDataFrame(exportAllChannels, sheet=sheet, startColumn=1, row.names=FALSE)
  
      data <- CircadianData$periodogram[which(peak==1),]
      
      #Create organized data sheet
      Labels <- data[,'labels',meta=T]
      Period <- data[,'period']
      Power <- data[,'power']
      periodDF <- data.frame(Labels,Period)
      powerDF <- data.frame(Labels,Power)
      
      colnames(periodDF)[1] <- 'Labels'
      colnames(periodDF)[2] <- 'Period'
      colnames(powerDF)[1] <- 'Labels'
      colnames(powerDF)[2] <- 'Power'
      
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
      
      saveWorkbook(wb, file = file)

  })
})
observe({
  
  req(nrow(damData$dt)>0)
  req(CircadianData$phase)
  output$savePhaseData <- downloadHandler(
    
    filename = function(){
      paste0(input$phaseTabs,'.xlsx')
    },
    content = function(file){
      
      wb <- createWorkbook(type="xlsx")
      
      settings <- settingsTable()[1:2,]
      sheet <- createSheet(wb, "Settings")
      addDataFrame(settings, sheet=sheet, startColumn=1, row.names=FALSE)
    
      
      all <- TRUE
      if (input$phaseTabs == "Phase"){
        data <- CircadianData$phase
      }
      else{
        if (input$phaseTabs == "Phase per day"){
          data <- CircadianData$phasePerDay
          all <-  FALSE
        }
        else{
          if(input$phaseTabs == "Interdaily stability"){
            data <- CircadianData$is
          }
          else{
            if(input$phaseTabs == "Intradaily variation"){
              data <- CircadianData$iv
            }
            else{
              if(input$phaseTabs == "Intradaily variation per day"){
                data <- CircadianData$ivPerDay
                all <- FALSE
              }
              else{
                if(input$phaseTabs == "Relative amplitude"){
                  data <- CircadianData$ra
                }
                else{
                  data <- CircadianData$raPerDay
                  all <- FALSE
                }
              }
            }
          }
        }
      }
      
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(PeriodogramReport(data,all), sheet=sheet, startColumn=1, row.names=FALSE)
      
      colnames(data)[1] <- 'Files'
      colnames(data)[2] <- 'Labels'
      data[,'Labels'] <- str_split_fixed(data[,'Data']," - ",2)[,1]
      colnames(data)[3] <- 'Channels'
      colnames(data)[4] <- 'Start_datetime'
      colnames(data)[5] <- 'End_datetime'
      
      xPlot <- data[,"xPlot"]
      yPlot <- data[,"yPlot"]
      data <- data.frame(data[,1:5],xPlot,yPlot)
      
      sheet <- createSheet(wb,"All data")
      addDataFrame(data, sheet=sheet, startColumn=1, row.names=FALSE)
      
      #Create organized data sheet
      if(all == TRUE){
        
        DF <- data.frame(data[,'Labels'],data[,'yPlot'])
        colnames(DF)[1] <- 'Labels'
        colnames(DF)[2] <- 'yPlot'
        joinedData <- DF %>%group_by(Labels) %>% group_nest()
          
        Data <- data.frame()
        for (i in 1:nrow(joinedData)){
          Data <- cbind.fill(Data, (unlist(joinedData[i,'data'])))
        }
        colnames(Data)<- unlist(joinedData[,'Labels'])

        sheet <- createSheet(wb, input$phaseTabs)
        addDataFrame(Data, sheet=sheet, startColumn=1, row.names=FALSE)
      }
      else{
        days <- unique(data[,'xPlot'])
        for (k in 1:length(days)){
          
          DF <- data.frame(data[which(data[,'xPlot']==days[k]),'Labels'],data[which(data[,'xPlot']==days[k]),'yPlot'])
          colnames(DF)[1] <- 'Labels'
          colnames(DF)[2] <- 'yPlot'
          joinedData <- DF %>%group_by(Labels) %>% group_nest()
          
          Data <- data.frame()
          
          for (i in 1:nrow(joinedData)){
            Data <- cbind.fill(Data, (unlist(joinedData[i,'data'])))
          }
          colnames(Data)<- unlist(joinedData[,'Labels'])
          
          sheet <- createSheet(wb, as.character(days[k]))
          addDataFrame(Data, sheet=sheet, startColumn=1, row.names=FALSE)
        }
      }
      
      saveWorkbook(wb, file = file)
      
    })
})
observe({
  
  req(nrow(damData$dt)>0)
  req(DAFdata$ScalingExponents)
  output$saveFractalReport <- downloadHandler(
    
    filename = function(){
      paste0(DAFfigureTitle(),'.xlsx')
    },
    content = function(file){
      
      wb <- createWorkbook(type="xlsx")
      
      settings <- rbind(settingsTable()[1:2,],settingsTable()[14:16,])
      sheet <- createSheet(wb, "Settings")
      addDataFrame(settings, sheet=sheet, startColumn=1, row.names=FALSE)
      
      
      sheet <- createSheet(wb, "Statistics")
      addDataFrame(DAFstatisticsData(), sheet=sheet, startColumn=1, row.names=FALSE)
      
      sheet <- createSheet(wb, "All data")
      addDataFrame(DAFdata$ScalingExponents, sheet=sheet, startColumn=1, row.names=FALSE)
      
      data <-DAFdata$ScalingExponents
      labels <- unique(data['Labels'])
      Se <- c("ScalingExponent1","ScalingExponent2")
      for(k in 1:length(Se)){
          
          DF <- data.frame(data[,'Labels'],data[,Se[k]])
          colnames(DF)[1] <- 'Labels'
          colnames(DF)[2] <- 'ScalingExponent'
          joinedData <- DF %>%group_by(Labels) %>% group_nest()
          
          Data <- data.frame()
          
          for (i in 1:nrow(joinedData)){
            Data <- cbind.fill(Data, (unlist(joinedData[i,'data'])))
          }
          colnames(Data)<- unlist(joinedData[,'Labels'])
          
          sheet <- createSheet(wb, Se[k])
          addDataFrame(Data, sheet=sheet, startColumn=1, row.names=FALSE)
      }
      saveWorkbook(wb, file = file)
      
    })
})

#Figures
observe({
  req(nrow(damData$dt)>0)
  req(CircadianFigures$periodogram)
  
  if (nrow(damData$dt)>0){
    shinyjs::enable("savePeriodogramFig")
  }
  else{
    shinyjs::disable("savePeriodogramFig")
  }
  
  output$savePeriodogramFig <- downloadHandler(
    
    filename = function(){
      paste0(input$periodogramTabs,input$periodogramFigures)
    },
    content = function(file){
      
      if (input$periodogramTabs == "Periodogram"){
        fig <- CircadianFigures$periodogram
      }
      else{
        if (input$periodogramTabs == "Period"){
          fig <- CircadianFigures$period
        }
        else{
          fig <- CircadianFigures$power

        }
      }
      ggsave(filename = file, plot = fig,
             width = round(input$periodWidth/97), height= round(input$periodHeight/97))
    })
})
observe({
  req(nrow(damData$dt)>0)
  req(CircadianFigures$phase)
  
  if (nrow(damData$dt)>0){
    shinyjs::enable("savePhaseFig")
  }
  else{
    shinyjs::disable("savePhaseFig")
  }
  
  output$savePhaseFig <- downloadHandler(
    
    filename = function(){
      paste0(input$phaseTabs,input$PhaseFigures)
    },
    content = function(file){
      
      if (input$phaseTabs == "Phase"){
        fig <- CircadianFigures$phase
        }
      else{
        if (input$phaseTabs == "Phase per day"){
          fig <- CircadianFigures$phasePerDay
        }
        else{
          if(input$phaseTabs == "Interdaily stability"){
            fig <- CircadianFigures$is
          }
          else{
            if(input$phaseTabs == "Intradaily variation"){
              fig <- CircadianFigures$iv
            }
            else{
              if(input$phaseTabs == "Intradaily variation per day"){
                fig <- CircadianFigures$ivPerDay
              }
              else{
                if(input$phaseTabs == "Relative amplitude"){
                  fig <- CircadianFigures$ra
                }
                else{
                  fig <- CircadianFigures$raPerDay
                }
              }
            }
          }
        }
      }
      
      ggsave(filename = file, plot = fig,
             width = round(input$periodWidth/97), height= round(input$periodHeight/97))
    })
})
observe({
  req(nrow(damData$dt)>0)
  req(CircadianFigures$periodogram)
  
  if (nrow(damData$dt)>0){
    shinyjs::enable("savefractalFig")
  }
  else{
    shinyjs::disable("savefractalFig")
  }
  
  output$savefractalFig <- downloadHandler(
    
    filename = function(){
      paste0(input$fractalTabs,input$fractalFig)
    },
    content = function(file){
      
      fig <- DAFfigure()

      ggsave(filename = file, plot = fig,
             width = round(input$periodWidth/97), height= round(input$periodHeight/97))
    })
})