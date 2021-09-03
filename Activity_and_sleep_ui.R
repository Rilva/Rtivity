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
                         selectInput("addedConditions2","Choose condition", choices = c("None"), selected = "None"))),
                
                fluidRow(
                  column(6, 
                         colourInput("linecolor2", "Line color", "black", allowTransparent = TRUE)),
                  column(6,style = "margin-top:10px",
                         radioButtons("pallete2", "Limited palette?", choices = c("Yes" = "limited", "No" = "square"), selected = "square", inline = TRUE))
                ),
                
                
                  HTML(
                    '<h4><b> Plot definitions </b></h4>'
                  ),
                
                fluidRow(
                  column(12,
                         actionButton('updateActivitySleepStatistics','Update graphs', class = "btn btn-dark", 
                                      style = "width: 100%; border-style: none;
                                         font-size:12pt; border-radius: 5px;"))),
                
                fluidRow(
                  column(6,
                         selectInput('error','Error bars', c("Standard error" = "mean_se", "Standard deviation" = "mean_sdl"), selected = "mean_se"))
                ),
                fluidRow(
                  column(12,
                         radioButtons('plot','Plot type', c("Bar" = "BarPlot", "Box" = "BoxPlot", "Dot" = "DotPlot", "Mean & Error" = "pointRange"), selected = "BarPlot", inline = TRUE))),
                

                fluidRow(style = "margin-top:10px",
                  column(6,
                         numericInput('activityGroupBoxTime','Custom graph time',1, min = 1)),
                  column(6, style = "margin-top:10px",
                         radioButtons('activityBoxTime','', c("Hours" = "Hour", "Days" = "Day"), selected = "Day", inline = TRUE))),
                fluidRow(
                  column(12,
                         sliderInput('yLimits','Activity y limits',min = 1, max = 100000, value = c(1,100000)))),
                

                
                HTML('<h4><b> Letters size </b></h4>'),
  
                fluidRow(
                  column(6,
                         sliderInput("titleLetterSize2","Title", min=6,max = 24,value= 16)),
                  
                  column(6,
                         sliderInput("axisLabelSize2","Axis Labels", min=6,max = 24,value= 14)),
                  column(6,
                         sliderInput("axisNumbersSize2","Axis Numbers", min=6,max = 24,value= 12)),
                  column(6,
                         sliderInput("dataLabelSize2","Data Labels", min=6,max = 24,value= 13))
                ),
                
                HTML('<h4><b> Title and axis definitions</b></h4>'),
  
                fluidRow(
                  column(6, textInput("ActivityBoxTitle", "Activity title","")),
                  column(6, textInput("SleepBoxTitle", "Sleep title",""))),
                fluidRow(
                  column(6, textInput("ActivityBoxYLabel", "Activity Y axis","")),
                  column(6, textInput("SleepBoxYLabel", "Sleep Y axis",""))),
                fluidRow(  
                  column(6, textInput("ActivityBoxXLabel", "Activity X axis","")),
                  column(6, textInput("SleepBoxXLabel", "Sleep X axis",""))
                ),
                
                fluidRow(
                  column(12,
                         sliderInput('activityWidth','Graph width', min = 500, max = 1200, value = 1200, step = 10))),
                fluidRow(
                  column(12,
                         sliderInput('activityHeight','Graph Height', min = 250, max = 400, value = 400, step = 10))),
                
  ),
  mainPanel(
    
    tabsetPanel(id = "ActivityboxPlotsTabs",
                tabPanel("Mean activity",plotOutput("activityDayNight")),
                tabPanel("Mean activity per day",plotOutput("activityPerDay")),
                tabPanel("Mean activity daytime vs nighttime",plotOutput("activityDayNightPerDay")),
                tabPanel("Mean activity custom time",plotOutput("activityCustomized"))
                
    ),
    fluidRow(
      column(8,DT::dataTableOutput("ActivitySummary"))),
    
    fluidRow(style = "margin-top:10px",
      column(2, style = "margin-top:20px",
             downloadButton("saveActivityFig", "Save image")),
             # shinySaveButton("saveActivityFig", "Save image", "Save figure as ...",icon = icon("save"), filetype=c("PNG" = "png","JPEG" = "jpeg","TIFF" = "tiff","PDF" = "pdf"))),
      column(2,
             selectInput("ActivityFig","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
      column(2,),
      column(2, style = "margin-top:20px",
             downloadButton("saveActivityReport", "Save data")),
    ),
    
    tabsetPanel(id = "SleepboxPlotsTabs",
                tabPanel("Mean sleep",plotOutput("sleepDayNight")),
                tabPanel("Mean sleep per day",plotOutput("sleepPerDay")),
                tabPanel("Mean sleep daytime vs nighttime",plotOutput("sleepDayNightPerDay")),
                tabPanel("Mean sleep custom time",plotOutput("sleepCustomized"))
                
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
    )
  ))