sidebarLayout(
  sidebarPanel (width = 3,
                
                fluidRow(
                  column(12,
                         sliderInput('sleepTime2','Inactivity to consider sleep (minutes)', min = 1, max = 60, value = 5, step = 1)),
                  column(12, 
                         actionButton("sleepAnalysis2", "Update sleep calculus", class = "btn btn-primary",
                                      style="width: 100%; background-color: #90EE90; border-style: none;
                                         font-size:14pt; border-radius: 5px;")),
                ),
                
                
                  HTML(
                    '<h4><b> Conditions definitions </b></h4>'
                  ),
                
                fluidRow(style="margin-top:10px",
                  column(8, 
                         selectInput("addedConditions4","Choose condition", choices = c("None"), selected = "None"))),
                
                fluidRow(
                  column(6, 
                         colourInput("color4", "Line color", "black", allowTransparent = TRUE)),
                  column(6,style = "margin-top:10px",
                         radioButtons("pallete4", "Limited palette?", choices = c("Yes" = "limited", "No" = "square"), selected = "square", inline = TRUE))
                ),
                
                
                  HTML(
                    '<h4><b> Plot definitions </b></h4>'
                  ),
                
                fluidRow(
                  column(12,
                         actionButton('updateSleepStatistics','Update graphs', class = "btn btn-dark", 
                                      style = "width: 100%; border-style: none;
                                         font-size:12pt; border-radius: 5px;"))),
                
                fluidRow(
                  column(6,
                         selectInput('SleepError','Error bars', c("Standard error" = "mean_se", "Standard deviation" = "mean_sdl"), selected = "mean_se"))
                ),
                fluidRow(
                  column(12,
                         radioButtons('SleepPlot','Plot type', c("Bar" = "BarPlot", "Box" = "BoxPlot", "Dot" = "DotPlot", "Mean & Error" = "pointRange"), selected = "BarPlot", inline = TRUE))),
                

                fluidRow(style = "margin-top:10px",
                  column(6,
                         numericInput('SleepGroupBoxTime','Custom graph time',1, min = 1)),
                  column(6, style = "margin-top:10px",
                         radioButtons('SleepBoxTime','', c("Hours" = "Hour", "Days" = "Day"), selected = "Day", inline = TRUE))),

                
                HTML('<h4><b> Letters size </b></h4>'),
  
                fluidRow(
                  column(6,
                         sliderInput("titleLetterSize3","Title", min=6,max = 24,value= 16)),
                  
                  column(6,
                         sliderInput("axisLabelSize3","Axis Labels", min=6,max = 24,value= 14)),
                  column(6,
                         sliderInput("axisNumbersSize3","Axis Numbers", min=6,max = 24,value= 12)),
                  column(6,
                         sliderInput("dataLabelSize3","Data Labels", min=6,max = 24,value= 13))
                ),
                
                HTML('<h4><b> Title and axis definitions</b></h4>'),
  
                fluidRow(
                  column(6, textInput("SleepBoxTitle", "Sleep title","")),
                  column(6, textInput("TST_WASOTitle", "TST or WASO title",""))),
                fluidRow(
                  column(6, textInput("SleepBoxYLabel", "Sleep Y axis","")),
                  column(6, textInput("TST_WASOYLabel", "TST or WASO Y axis",""))),
                fluidRow(  
                  column(6, textInput("SleepBoxXLabel", "Sleep X axis","")),
                  column(6, textInput("TST_WASOXLabel", "TST or WASO Y axis",""))
                ),
                
                fluidRow(
                  column(12,
                         sliderInput('yLimitsTST_WASOTime','TST and WASO y limits',min = 0, max = 12, value = c(0,12), step = 0.5))),
            
                
                HTML('<h4><b> Sleep bouts </b></h4>'),
                
                fluidRow(
                  column(12,
                         sliderInput('yLimitsBoutSleepTime','Sleep bout time y limits',min = 1, max = 200000, value = c(1,200000)))),
                
                fluidRow(
                  column(12,
                         sliderInput('yLimitsSleepLatency','Sleep Latency y limits',min = 1, max = 200000, value = c(1,200000)))),
                
                
                fluidRow(
                  column(6, textInput("SleepTimeBoxTitle", "Sleep bout time title","")),
                  column(6, textInput("SleepLatencyBoxTitle", "Sleep latency title",""))),
                fluidRow(
                  column(6, textInput("SleepTimeBoxYLabel", "Sleep bout time Y axis","")),
                  column(6, textInput("SleepLatencyBoxYLabel", "Sleep latency Y axis",""))),
                fluidRow(
                  column(6, textInput("SleepTimeBoxXLabel", "Sleep bout time X axis","")),
                  column(6, textInput("SleepLatencyBoxXLabel", "Sleep latency X axis",""))
                ),
                
                fluidRow(
                  column(12,
                         sliderInput('SleepWidth','Graph width', min = 500, max = 1200, value = 1200, step = 10))),
                fluidRow(
                  column(12,
                         sliderInput('SleepHeight','Graph Height', min = 250, max = 400, value = 400, step = 10))),
                
  ),
  mainPanel(
    
    tabsetPanel(id = "SleepboxPlotsTabs",
                tabPanel("Sleep ratio per light phase",plotOutput("sleepDayNight")),
                tabPanel("Sleep ratio per day",plotOutput("sleepPerDay")),
                tabPanel("Sleep ratio per day and light phase",plotOutput("sleepDayNightPerDay")),
                tabPanel("Sleep ratio customized time",plotOutput("sleepCustomized"))
                
    ),
    fluidRow(
      column(8,DT::dataTableOutput("SleepSummary"))),
    
    fluidRow(style = "margin-top:10px",
      column(2, style = "margin-top:20px",
             downloadButton("saveSleepFig", "Save image")),
        
      column(2,
             selectInput("SleepFig","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
      column(2,),
      column(2, style = "margin-top:20px",
             downloadButton("saveSleepReport", "Save data"))
    ),
    
    tabsetPanel(id = "TST_WASOPlotsTabs",
                tabPanel("Total Sleep Time",plotOutput("TSTall")),
                tabPanel("Total Sleep Time per day",plotOutput("TSTday")),
                tabPanel("Wake after sleep onset",plotOutput("WASOall")),
                tabPanel("Wake after sleep onset per day",plotOutput("WASOday"))
                
    ),
    fluidRow(
      column(8,DT::dataTableOutput("TST_WASOSummary"))),
    
    fluidRow(style = "margin-top:10px",
             column(2, style = "margin-top:20px",
                    downloadButton("saveTST_WASOFig", "Save image")),
             
             column(2,
                    selectInput("TST_WASOFig","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
             column(2,),
             column(2, style = "margin-top:20px",
                    downloadButton("saveTST_WASOReport", "Save data"))
    ),
    
    
    h3("Sleep bouts", align = "center", style = "font-weight:bold; margin-bottom:10px; border-radius: 5pt;
       background-color:Lavender; padding:7px "),
    
    tabsetPanel(id = "SleepTimePlotsTabs",
                tabPanel("Sleep bout duration per light phase",plotOutput("SleepTimeDayNight")),
                tabPanel("Sleep bout duration per day",plotOutput("SleepTimePerDay")),
                tabPanel("Sleep bout duration per day and light phase",plotOutput("SleepTimeDayNightPerDay")),
                tabPanel("Sleep bout duration customized time",plotOutput("SleepTimeCustomized"))
                
    ),
    fluidRow(
      column(8,DT::dataTableOutput("SleepTimeSummary"))),
    
    fluidRow(style = "margin-top:10px",
             column(2, style = "margin-top:20px",
                    downloadButton("saveSleepTimeFig", "Save image")),
             column(2,
                    selectInput("SleepTimeFig","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
             column(2,),
             column(2, style = "margin-top:20px",
                    downloadButton("saveSleepTimeReport", "Save data")),
    ),
    
    tabsetPanel(id = "SleepLatencyPlotsTabs",
                tabPanel("Sleep latency",plotOutput("SleepLatencyDayNight")),
                tabPanel("Sleep latency per day",plotOutput("SleepLatencyPerDay")),
                tabPanel("Sleep latency customized time",plotOutput("SleepLatencyCustomized"))
                
    ),
    fluidRow(
      column(8,DT::dataTableOutput("SleepLatencySummary"))),
    
    fluidRow(style = "margin-top:10px",
             column(2, style = "margin-top:20px",
                    downloadButton("saveSleepLatencyFig", "Save image")),
             column(2,
                    selectInput("SleepLatencyFig","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
             column(2,),
             column(2, style = "margin-top:20px",
                    downloadButton("saveSleepLatencyReport", "Save data")),
    )
  ))