div(

sidebarLayout(
  sidebarPanel (width = 3,
                
                
                  HTML(
                    '<h4><b> Conditions definitions </b></h4>'
                  ),
                
                fluidRow(
                  column(8, 
                         selectInput("addedConditions2","Choose condition", choices = c("None"), selected = "None")),
                
                  column(4, 
                         selectInput("linetype", "Line type", choices = c("blank", "solid", "dashed", "dotted"), selected = "solid"))),
                fluidRow(  
                  column(6, 
                         colourInput("color2", "Line color", "black", allowTransparent = TRUE)),
                  column(6,style = "margin-top:10px",
                         radioButtons("pallete2", "Limited palette?", choices = c("Yes" = "limited", "No" = "square"), selected = "square", inline = TRUE))
                  
                ),
                
                
                HTML('<h4><b> Plot definitions </b></h4>'),
                
                fluidRow(
                  column(12,
                         actionButton('updateAesthetics','Update graphs', class = "btn btn-dark", 
                                      style = "width: 100%; border-style: none;
                                         font-size:12pt; border-radius: 5px;"))),
                
                fluidRow(style="margin-top:10px",
                  column(6,
                         selectInput('errorChronogram','Error bars', c("Standard error" = "mean_se", "Standard deviation" = "mean_sdl", "None" = NA), selected = "mean_se")),
                  column(6,style = "margin-top:10px",
                         radioButtons("lightDark",HTML('<b>LD annotations</b>'), choices=c("Yes"=TRUE,"No"=FALSE), selected = TRUE, inline = TRUE)),
                  ),
                
                fluidRow(
                  column(5,
                         radioButtons("tile_bar",HTML('<b>Actogram</b>'),choices=c("Tile"="tile","Bar"="bar"), selected = "tile", inline = TRUE)),
                  column(7,
                         radioButtons("individualPlot",'Plot conditions',
                                      c('Altogether' = 'together', 'Separate' = 'individual'), selected = 'together',inline = TRUE)),
                ),
                
                HTML('<h4><b> Letters size </b></h4>'),
  
                fluidRow(
                  column(6,
                         sliderInput("titleLetterSize6","Title", min=6,max = 24,value= 16)),
                  column(6,
                         sliderInput("axisLabelSize6","Axis Labels", min=6,max = 24,value= 14)),
                  column(6,
                         sliderInput("axisNumbersSize6","Axis Numbers", min=6,max = 24,value= 12)),
                  column(6,
                         sliderInput("dataLabelSize6","Data Labels", min=6,max = 24,value= 13))
                ),
                
                HTML('<h4><b> Title and axis definitions</b></h4>'),
                fluidRow(
                  column(12, textInput("graphTitle", "Graph title","")),
                ),
                
                fluidRow(
                  column(12, textInput("yLabel", "Y axis label",""))),
                
                fluidRow(
                  column(6, numericInput('y_min','Minimum Y', NA, min = 0)),
                  column(6, numericInput('y_max','Maximum Y', NA, min = 0)),
                ),
                
                fluidRow(
                  column(12, textInput("xLabel", "X axis label",""))),
                
                fluidRow(
                  column(12,
                         radioButtons("dataTime", "X axis time", choices = c("Zeitgeber" = "zt", "Data acquisition" = "normal"), selected = "zt", inline = TRUE))),
                
                fluidRow(column(3,
                         numericInput('x0','X0', 0, min = 0)),
                  column(style="margin-top:10px",9,
                         radioButtons('xTime','X time scale', c("Days" = "days","Hours" = "hour", "Mins" = "min","Secs" = "sec"), selected = "days", inline = TRUE))
                  
                  ),
                
                fluidRow(style="margin-top:10px",
                         column(7,
                                numericInput('ticks_distance','Distance between X ticks', 1, min = 0.25)),
                         column(5,
                                numericInput('tick0','Start X ticks at', 0, min = 0))),
                
                
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
            column(2),
            column(4,
                   sliderInput('sleepTime','Inactivity to consider sleep (minutes)', min = 1, max = 60, value = 5, step = 1)),
            column(4,
                   sliderInput('movingAverage','Bin size (minutes)', min = 1, max = 180, value = 60, step = 1)),
            ),
          
          fluidRow(
            column(2),
            column(8, 
                   actionButton("sleepAnalysis", "Update sleep calculus and graphs", class = "btn btn-primary",
                                style="width: 100%; border-style: none;
                                         font-size:14pt; border-radius: 5px;")),
          ),
    ),
    
    #Plot panel
    tabsetPanel(id = "tabs",
                tabPanel("Actogram",plotOutput("actogram")),
                tabPanel("Double plot actogram",plotOutput("doublePlotActogram")),
                tabPanel("Full chronogram", plotOutput("chronogram")),
                tabPanel("Average LD cycle chronogram", plotOutput("chronogram1day")),
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
             downloadButton("saveData", "Save data"))),
    
  ))
)