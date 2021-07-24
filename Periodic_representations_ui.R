div(

sidebarLayout(
  sidebarPanel (width = 3,
                
                fluidRow(
                  HTML(
                    '<h4><b> Conditions definitions </b></h4>'
                  ),
                  column(6, 
                         selectInput("addedConditions","Choose condition", choices = c("None"), selected = "None"))),
                fluidRow(
                  column(3, 
                         selectInput("linetype", "Line type", choices = c("blank", "solid", "dashed", "dotted"), selected = "solid")),
                  column(4, 
                         colourInput("linecolor", "Line color", "black", allowTransparent = TRUE)),
                  column(5,
                         radioButtons("pallete", "Limited palette?", choices = c("Yes" = "limited", "No" = "square"), selected = "square", inline = TRUE))
                  
                ),
                
                fluidRow(
                  HTML(
                    '<h4><b> Plot definitions </b></h4>'
                  ),
                  
                  
                  column(6, 
                         radioButtons("chronogramError","Error bars", choices = c("Yes" = "TRUE", "No" = "FALSE"), selected = "TRUE", inline = TRUE)),
                  column(6,
                         selectInput('errorChronogram','Error bars', c("Standard error" = "mean_se", "Standard deviation" = "mean_sdl"), selected = "mean_se"))
                
                  ),
                
                fluidRow(
                  column(6,
                         sliderInput("titleLetterSize","Title letter size", min=6,max = 24,value= 16)),
                  
                  column(6,
                         sliderInput("axisLabelSize","Axis Labels letter size", min=6,max = 24,value= 14)),
                  column(6,
                         sliderInput("axisNumbersSize","Axis Numbers letter size", min=6,max = 24,value= 12)),
                  column(6,
                         sliderInput("dataLabelSize","Data Labels letter size", min=6,max = 24,value= 13))
                ),
                
                fluidRow(
                  HTML('<h4><b> Title and Y axis </b></h4>'),
                  column(6, textInput("graphTitle", "Activity graph title","")),
                  column(6, textInput("yLabel", "Activity Y axis label",""))
                ),
                
                fluidRow(
                  column(6, numericInput('y_min','Minimum Y', NA, min = 0)),
                  column(6, numericInput('y_max','Maximum Y', NA, min = 0)),
                ),
                
                fluidRow(
                  HTML('<h4><b> X axis </b></h4>'),
                  column(6, textInput("xLabel", "X axis label","")),
                  column(6,
                         radioButtons("dataTime", "X time", choices = c("Zeitgeber" = "zt", "Data acquisition" = "normal"), selected = "zt", inline = TRUE)),
                  ),
                fluidRow(column(3,
                         numericInput('x0','X0 time', 0, min = 0)),
                  column(style="margin-top:10px",9,
                         radioButtons('xTime','', c("Days" = "days","Hours" = "hour", "Minutes" = "min","Seconds" = "sec"), selected = "days", inline = TRUE))
                  
                  ),
                
                fluidRow(style="margin-top:10px",
                         column(6,
                                numericInput('ticks_distance','Distance between ticks', 1, min = 0.25)),
                         column(6,
                                numericInput('tick0','Start ticks at', 0, min = 0))),
                
                
                fluidRow(
                  column(12,
                         sliderInput('periodicWidth','Graph width', min = 500, max = 1200, value = 1200, step = 10))),
                fluidRow(
                  column(12,
                         sliderInput('periodicHeight','Graph Height', min = 250, max = 400, value = 400, step = 10)))),
  
  
  
  
  mainPanel(
    
    div ( style = "background-color:#F5F5F5; padding: 10pt;
                                   border-radius: 5px; border-style:groove; font-size: 11pt; margin-bottom: 10px;",
          fluidRow(
            
            column(3,
                   sliderInput('movingAverage','Activity bin size (minutes)', min = 1, max = 180, value = 30, step = 1)),
            column(3, sliderInput('sleepTime','Inactivity to consider sleep (minutes)', min = 1, max = 180, value = 10, step = 1)),
            
            column(style = "margin-top:5px",2,
                   selectInput("perFun",
                               'Periodogram Function',
                               c('Lomb-Scargle' = 'ls','Chi-squared' = 'chi-sq', 'Fourier' = 'fourier'),
                               selected = 'mean')),
            column(style = "margin-top:5px",1,
                   numericInput('minPer','Min (h)' , 18, min = 1)),
            column(style = "margin-top:5px",1,
                   numericInput('maxPer','Max (h)' , 32, min = 1)),
            column(style = "margin-top:27px", 2, checkboxInput('showPeriods',HTML('<b>Show period points</b>') , FALSE))
          ),
          
          fluidRow(
              column(3,
                     radioButtons("tile_bar","Actogram tile or bar",choices=c("Tile"="tile","Bar"="bar"), selected = "tile", inline = TRUE)),
              column(3, checkboxInput("lightDark",HTML('<b>LD annotations</b>'), FALSE)),
              column(3,
                     radioButtons("individualPlot",'Plot conditions',
                                  c('Altogether' = 'together', 'Separate' = 'individual'), selected = 'together',inline = TRUE)),
              )
    ),
    
    #Plot panel
    tabsetPanel(id = "tabs",
                tabPanel("Actogram",plotOutput("actogram")),
                tabPanel("Double plot actogram",plotOutput("doublePlotActogram")),
                tabPanel("Chronogram", plotOutput("chronogram")),
                tabPanel("Daily Chronogram", plotOutput("chronogram1day")),
                tabPanel("Periodogram", plotOutput("periodogram")),
                tabPanel("Cumulative Activity", plotOutput("cumAct")),
                tabPanel("Sleep Chronogram", plotOutput("sleep"))
    ),
    fluidRow(style = "margin-top:10px",
      column(2, style = "margin-top:20px",
             downloadButton("saveFigures", "Save figures")),
             # shinySaveButton("saveFigures", "Save figures", "Save figure as ...",icon = icon("save"), filetype=c("PNG" = "png","JPEG" = "jpeg","TIFF" = "tiff","PDF" = "pdf"))),
      column(2,
             selectInput("periodicFigures","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
      column(2),
      column(2, style = "margin-top:20px",
             downloadButton("saveData", "Save data")))
             # shinySaveButton("saveData", "Save data", "Save data as ...",icon = icon("save"), filetype='xlsx'))),
    
  ))
)