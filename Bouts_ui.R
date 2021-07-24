sidebarLayout(
  sidebarPanel(width = 3,
               
               fluidRow(
                 column(12, 
                        actionButton("boutAnalysis", "Analyse activity bouts",
                                     style="width: 100%; background-color: #90EE90; border-style: none;
                                         color: black; font-size:14pt; border-radius: 5px;")),
                 column(12,
                        sliderInput("boutWindow", "Bout Window", min = 0.5, max = 60, step = 0.5, value = 1)),
               ),
               
               fluidRow(
                 HTML(
                   '<h4><b> Conditions definitions </b></h4>'
                 ),
                 column(6, 
                        selectInput("addedConditions3","Choose condition", choices = c("None"), selected = "None"))),
               
               fluidRow(
                 column(3, 
                        selectInput("linetype3", "Line type",c("blank", "solid", "dashed", "dotted"), selected = "solid")),
                 column(4, 
                        colourInput("linecolor3", "Line color", "black", allowTransparent = TRUE)),
                 column(5,
                        radioButtons("pallete3", "Limited palette?", choices = c("Yes" = "limited", "No" = "square"), selected = "square", inline = TRUE))
               ),
               
               
               fluidRow(
                 HTML(
                   '<h4><b> Plot definitions </b></h4>'
                 ),
                 column(5,
                        numericInput('boutGroupBoxTime','Custom graph time',1, min = 1)),
                 column(7,
                        radioButtons('boutBoxTime','', c("Minutes" = "Min", "Hours" = "Hour", "Days" = "Day"), selected = "Day", inline = TRUE))),
               
               
               fluidRow(
                 column(12,
                        sliderInput('yLimitsBoutActivity','Bout activity y limits',min = 1, max = 2000, value = c(1,2000)))),
               fluidRow(
                 column(12,
                        sliderInput('yLimitsBoutTime','Bout time y limits',min = 1, max = 200000, value = c(1,200000)))),
               
               fluidRow(
                 column(12,
                        radioButtons('boutPlot','Plot type', c("Bar Plot" = "BarPlot", "Box Plot" = "BoxPlot", "Dot Plot" = "DotPlot", "Mean & Error" = "pointRange"), selected = "BarPlot", inline = TRUE))),
               
               fluidRow(
                 column(6,
                        selectInput('boutError','Error bars', c("Standard error" = "mean_se", "Standard deviation" = "mean_sdl"), selected = "mean_se"))
               ),
               
               fluidRow(
                 column(6,
                        sliderInput("titleLetterSize3","Title letter size", min=6,max = 24,value= 16)),
                 
                 column(6,
                        sliderInput("axisLabelSize3","Axis Labels letter size", min=6,max = 24,value= 14)),
                 column(6,
                        sliderInput("axisNumbersSize3","Axis Numbers letter size", min=6,max = 24,value= 12)),
                 column(6,
                        sliderInput("dataLabelSize3","Data Labels letter size", min=6,max = 24,value= 13))
               ),
               
               fluidRow(
                 HTML('<h4><b> Activity bouts </b></h4>')
               ),
               fluidRow(
                 column(6, textInput("BoutActivityBoxTitle", "Bout activity graph title","")),
                 column(6, textInput("BoutTimeBoxTitle", "Bout time graph title","")),
                 column(6, textInput("BoutActivityBoxYLabel", "Bout activity Y axis label","")),
                 column(6, textInput("BoutTimeBoxYLabel", "Bout time Y axis label","")),
                 column(6, textInput("BoutActivityBoxXLabel", "Bout activity X axis label","")),
                 column(6, textInput("BoutTimeBoxXLabel", "Bout time X axis label",""))
               ),
               
               
               fluidRow(
                 column(12,
                        sliderInput('boutWidth','Graph width', min = 500, max = 1200, value = 1200, step = 10))),
               fluidRow(
                 column(12,
                        sliderInput('boutHeight','Graph Height', min = 250, max = 400, value = 400, step = 10))),
               
               fluidRow(
                 HTML('<h4><b> Sleep bouts </b></h4>')
               ),
               fluidRow(
                 column(12,
                        sliderInput('yLimitsBoutSleepTime','Sleep bout time y limits',min = 1, max = 200000, value = c(1,200000)))),
               
               fluidRow(
                 column(12,
                        sliderInput('yLimitsSleepLatency','Sleep Latency y limits',min = 1, max = 200000, value = c(1,2000)))),
               
               fluidRow(
                 column(6, textInput("SleepTimeBoxTitle", "Sleep bout time graph title","")),
                 column(6, textInput("SleepLatencyBoxTitle", "Sleep latency graph title","")),
                 
                 column(6, textInput("SleepTimeBoxYLabel", "Sleep bout time Y axis label","")),
                 column(6, textInput("SleepLatencyBoxYLabel", "Sleep latency Y axis label","")),
        
                 column(6, textInput("SleepTimeBoxXLabel", "Sleep bout time X axis label","")),
                 column(6, textInput("SleepLatencyBoxXLabel", "Sleep latency X axis label",""))
               ),
               
               
               
  ),
  mainPanel(
    
    tabsetPanel(id = "boutActivityPlotsTabs",
                tabPanel("Mean activity bouts",plotOutput("boutActivityDayNight")),
                tabPanel("Mean activity bouts per day",plotOutput("boutActivityPerDay")),
                tabPanel("Mean activity bouts daytime vs nighttime",plotOutput("boutActivityDayNightPerDay")),
                tabPanel("Mean activity bouts custom time",plotOutput("boutActivityCustomized"))
                
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
                tabPanel("Mean bouts time",plotOutput("boutTimeDayNight")),
                tabPanel("Mean bouts time per day",plotOutput("boutTimePerDay")),
                tabPanel("Mean bouts time daytime vs nighttime",plotOutput("boutTimeDayNightPerDay")),
                tabPanel("Mean bouts time custom time",plotOutput("boutTimeCustomized"))
                
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
    
    tabsetPanel(id = "SleepTimePlotsTabs",
                tabPanel("Sleep bout time",plotOutput("SleepTimeDayNight")),
                tabPanel("Sleep bout time per day",plotOutput("SleepTimePerDay")),
                tabPanel("Sleep bout time daytime vs nighttime",plotOutput("SleepTimeDayNightPerDay")),
                tabPanel("Sleep bout time custom time",plotOutput("SleepTimeCustomized"))
                
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
                tabPanel("Sleep latency custom time",plotOutput("SleepLatencyCustomized"))
                
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