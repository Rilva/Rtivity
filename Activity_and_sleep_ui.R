sidebarLayout(
  sidebarPanel (width = 3,
                
                fluidRow(
                  HTML(
                    '<h4><b> Conditions definitions </b></h4>'
                  ),
                  column(6, 
                         selectInput("addedConditions2","Choose condition", choices = c("None"), selected = "None"))),
                
                fluidRow(
                  column(3, 
                         selectInput("linetype2", "Line type", choices = c("blank", "solid", "dashed", "dotted"), selected = "solid")),
                  column(4, 
                         colourInput("linecolor2", "Line color", "black", allowTransparent = TRUE)),
                  column(5,
                         radioButtons("pallete2", "Limited palette?", choices = c("Yes" = "limited", "No" = "square"), selected = "square", inline = TRUE))
                ),
                
                fluidRow(
                  HTML(
                    '<h4><b> Plot definitions </b></h4>'
                  ),
                  column(5,
                         numericInput('activityGroupBoxTime','Custom graph time',1, min = 1)),
                  column(7,
                         radioButtons('activityBoxTime','', c("Minutes" = "Min", "Hours" = "Hour", "Days" = "Day"), selected = "Day", inline = TRUE))),
                
                
                fluidRow(
                  column(12,
                         sliderInput('yLimits','Activity y limits',min = 1, max = 100000, value = c(1,100000)))),
                fluidRow(
                  HTML(
                    '<h4><b> Plots </b></h4>'
                  ),
                  column(12,
                         radioButtons('plot','Plot type', c("Bar Plot" = "BarPlot", "Box Plot" = "BoxPlot", "Dot Plot" = "DotPlot", "Mean & Error" = "pointRange"), selected = "BarPlot", inline = TRUE))),
                fluidRow(
                  column(6,
                         selectInput('error','Error bars', c("Standard error" = "mean_se", "Standard deviation" = "mean_sdl"), selected = "mean_se"))
                ),
                fluidRow(
                  column(6,
                         sliderInput("titleLetterSize2","Title letter size", min=6,max = 24,value= 16)),
                  
                  column(6,
                         sliderInput("axisLabelSize2","Axis Labels letter size", min=6,max = 24,value= 14)),
                  column(6,
                         sliderInput("axisNumbersSize2","Axis Numbers letter size", min=6,max = 24,value= 12)),
                  column(6,
                         sliderInput("dataLabelSize2","Data Labels letter size", min=6,max = 24,value= 13))
                ),
                
                fluidRow(
                  column(6, textInput("ActivityBoxTitle", "Activity graph title","")),
                  column(6, textInput("SleepBoxTitle", "Sleep graph title","")),
                  column(6, textInput("ActivityBoxYLabel", "Activity Y axis label","")),
                  column(6, textInput("SleepBoxYLabel", "Sleep Y axis label","")),
                  column(6, textInput("ActivityBoxXLabel", "Activity X axis label","")),
                  column(6, textInput("SleepBoxXLabel", "Sleep X axis label",""))
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
             # shinySaveButton("saveActivityReport", "Save Data", "Save data as ...",icon = icon("save"), filetype='xlsx'))
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
             # shinySaveButton("saveSleepFig", "Save image", "Save figure as ...",icon = icon("save"), filetype=c("PNG" = "png","JPEG" = "jpeg","TIFF" = "tiff","PDF" = "pdf"))),
      column(2,
             selectInput("SleepFig","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
      column(2,),
      column(2, style = "margin-top:20px",
             downloadButton("saveSleepReport", "Save data")),
             # shinySaveButton("saveSleepReport", "Save Data", "Save data as ...",icon = icon("save"), filetype='xlsx'))
    )
  ))