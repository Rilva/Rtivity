sidebarLayout(
  sidebarPanel(width = 3,
               
               fluidRow(
                 column(12,
                        sliderInput("boutWindow", "Inactivity to consider a movement stop (minutes)", min = 0.5, max = 60, step = 0.5, value = 1)),
                 
                 column(12, 
                        actionButton("boutAnalysis", "Analyse activity bouts", class = "btn btn-primary",
                                     style="width: 100%; background-color: #90EE90; border-style: none;
                                         font-size:14pt; border-radius: 5px;")),
                     ),
               
               
                 HTML(
                   '<h4><b> Conditions definitions </b></h4>'
                 ),
               fluidRow(style="margin-top:10px",
                 column(8, 
                        selectInput("addedConditions3","Choose condition", choices = c("None"), selected = "None"))),
               
               fluidRow(
                 column(6, 
                        colourInput("linecolor3", "Line color", "black", allowTransparent = TRUE)),
                 column(6, style = "margin-top:10px",
                        radioButtons("pallete3", "Limited palette?", choices = c("Yes" = "limited", "No" = "square"), selected = "square", inline = TRUE))
               ),
               
                 HTML(
                   '<h4><b> Plot definitions </b></h4>'
                 ),
                 
               fluidRow(
                 column(12,
                        actionButton('updateBoutsStatistics','Update graphs', class = "btn btn-dark", 
                                     style = "width: 100%; border-style: none;
                                         font-size:12pt; border-radius: 5px;"))),
               fluidRow(
                 column(6,
                        selectInput('boutError','Error bars', c("Standard error" = "mean_se", "Standard deviation" = "mean_sdl"), selected = "mean_se"))
               ),
               
               fluidRow(
                 column(12,
                        radioButtons('boutPlot','Plot type', c("Bar" = "BarPlot", "Box" = "BoxPlot", "Dot" = "DotPlot", "Mean & Error" = "pointRange"), selected = "BarPlot", inline = TRUE))),
               
               
               fluidRow( style = "margin-top:10px",
                 column(6,
                        numericInput('boutGroupBoxTime','Custom graph time',1, min = 1)),
                 column(6,
                        radioButtons('boutBoxTime','', c("Hours" = "Hour", "Days" = "Day"), selected = "Day", inline = TRUE))),
               
               
               
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
               
               HTML('<h4><b> Activity bouts</b></h4>'),
               fluidRow(
                 column(12,
                        sliderInput('yLimitsBoutActivity','Bout activity y limits',min = 1, max = 2000, value = c(1,2000)))),
               fluidRow(
                 column(12,
                        sliderInput('yLimitsBoutTime','Bout time y limits',min = 1, max = 200000, value = c(1,200000)))),
               

               
               fluidRow(
                 column(6, textInput("BoutActivityBoxTitle", "Bout activity title","")),
                 column(6, textInput("BoutTimeBoxTitle", "Bout time title",""))),
               fluidRow(
                 column(6, textInput("BoutActivityBoxYLabel", "Bout activity Y axis","")),
                 column(6, textInput("BoutTimeBoxYLabel", "Bout time Y axis",""))),
               fluidRow(
                 column(6, textInput("BoutActivityBoxXLabel", "Bout activity X axis","")),
                 column(6, textInput("BoutTimeBoxXLabel", "Bout time X axis",""))
               ),
               
               
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
                        sliderInput('boutWidth','Graph width', min = 500, max = 1200, value = 1200, step = 10))),
               fluidRow(
                 column(12,
                        sliderInput('boutHeight','Graph Height', min = 250, max = 400, value = 400, step = 10))),
               
               
               
  ),
  mainPanel(
    
    h3("Activity bouts", align = "center", style = "font-weight:bold; margin-bottom:10px; border-radius: 5pt;
       background-color:Lavender; padding:7px "),
    
    tabsetPanel(id = "boutActivityPlotsTabs",
                tabPanel("Activity per bout per light phase",plotOutput("boutActivityDayNight")),
                tabPanel("Activity per bout per day",plotOutput("boutActivityPerDay")),
                tabPanel("Activity per bout per day and light phase",plotOutput("boutActivityDayNightPerDay")),
                tabPanel("Activity per bout customized time",plotOutput("boutActivityCustomized"))
                
    ),
    fluidRow(
      column(8,DT::dataTableOutput("BoutActivitySummary"))),
    
    fluidRow(style = "margin-top:10px",
      column(2,style = "margin-top:20px",
             downloadButton("saveBoutActivityFig", "Save image")),
      column(2,
             selectInput("BoutActivityFig","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
      column(2,),
      column(2, style = "margin-top:20px",
             downloadButton("saveBoutActivityReport", "Save data")),
    ),
    
    
    tabsetPanel(id = "boutTimePlotsTabs",
                tabPanel("Bout duration per light phase",plotOutput("boutTimeDayNight")),
                tabPanel("Bout duration per day",plotOutput("boutTimePerDay")),
                tabPanel("Bout duration per day and light phase",plotOutput("boutTimeDayNightPerDay")),
                tabPanel("Bout duration customized time",plotOutput("boutTimeCustomized"))
                
    ),
    fluidRow(
      column(8,DT::dataTableOutput("BoutTimeSummary"))),
    
    fluidRow(style = "margin-top:10px",
      column(2, style = "margin-top:20px",
             downloadButton("saveBoutTimeFig", "Save image")),
      column(2,
             selectInput("BoutTimeFig","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
      column(2,),
      column(2, style = "margin-top:20px",
             downloadButton("saveBoutTimeReport", "Save data")),
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
    
  )
)