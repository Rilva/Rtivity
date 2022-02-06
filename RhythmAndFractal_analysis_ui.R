div(
  
  sidebarLayout(
    sidebarPanel (width = 3,
                  
                  
                  HTML(
                    '<h4><b> Conditions definitions </b></h4>'
                  ),
                  
                  fluidRow(
                    column(8, 
                           selectInput("addedConditions5","Choose condition", choices = c("None"), selected = "None")),
                    
                    column(4, 
                           selectInput("linetype2", "Line type", choices = c("blank", "solid", "dashed", "dotted"), selected = "solid"))),
                  fluidRow(  
                    column(6, 
                           colourInput("color5", "Line color", "black", allowTransparent = TRUE)),
                    column(6,style = "margin-top:10px",
                           radioButtons("pallete5", "Limited palette?", choices = c("Yes" = "limited", "No" = "square"), selected = "square", inline = TRUE))
                    
                  ),
                  
                  
                  HTML('<h4><b> Plot definitions </b></h4>'),
                  
                  fluidRow(
                    column(12,
                           actionButton('updateAesthetics4','Update graphs', class = "btn btn-dark", 
                                        style = "width: 100%; border-style: none;
                                         font-size:12pt; border-radius: 5px;"))),
                  
                  fluidRow(style="margin-top:10px",
                           column(6,
                                  selectInput('PeriodicError','Error bars', c("Standard error" = "mean_se", "Standard deviation" = "mean_sdl", "None" = NA), selected = "mean_se"))),
                  
                  fluidRow(
                    column(12,
                           radioButtons('PeriodicPlot','Plot type', c("Bar" = "BarPlot", "Box" = "BoxPlot", "Dot" = "DotPlot", "Mean & Error" = "pointRange"), selected = "BarPlot", inline = TRUE))),
                  
                  
                  HTML('<h4><b> Letters size </b></h4>'),
                  
                  fluidRow(
                    column(6,
                           sliderInput("titleLetterSize4","Title", min=6,max = 24,value= 16)),
                    column(6,
                           sliderInput("axisLabelSize4","Axis Labels", min=6,max = 24,value= 14)),
                    column(6,
                           sliderInput("axisNumbersSize4","Axis Numbers", min=6,max = 24,value= 12)),
                    column(6,
                           sliderInput("dataLabelSize4","Data Labels", min=6,max = 24,value= 13))
                  ),
                  
                  HTML('<h4><b> Title and axis definitions</b></h4>'),
                  fluidRow(
                    column(12, textInput("graphTitlePeriod", "Periodogram title","")),
                  ),
                  
                  fluidRow(
                    column(12, textInput("yLabelPeriod", "Y axis label",""))),
                  fluidRow(
                    column(12, textInput("xLabelPeriod", "X axis label",""))),
                  
                  fluidRow(
                    column(12, sliderInput('yLimitsPeriod','Period and power y limits',min = 1, max = 48, value = c(1,48)))
                  ),
                  
                  fluidRow(
                    column(6, textInput("graphTitlePhase", "Phase title","")),
                    column(6, textInput("graphTitleFractal", "Fractal title","")),
                  ),
                  fluidRow(
                    column(6, textInput("yLabelPhase", "Y axis label","")),
                    column(6, textInput("yLabelFractal", "Y axis label","")),
                  ),
                  fluidRow(
                    column(6, textInput("xLabelPhase", "X axis label","")),
                    column(6, textInput("xLabelFractal", "X axis label","")),
                  ),
                  
                  fluidRow(
                    column(12, sliderInput('yLimitsPhase','Phase, IS, IV and RA limits',min = 1, max = 24, value = c(1,24), step = 0.1))
                  ),

                  fluidRow(
                    column(12,
                           sliderInput('periodWidth','Graph width', min = 500, max = 1200, value = 1200, step = 10))),
                  fluidRow(
                    column(12,
                           sliderInput('periodHeight','Graph Height', min = 250, max = 400, value = 400, step = 10)))),
    
    
    
    
    mainPanel(
      
      div ( style = "background-color:#F5F5F5; padding: 10pt;
                                   border-radius: 5px; border-style:groove; font-size: 11pt; margin-bottom: 10px;",
            
            HTML('<h3><b>Periodogram settings</b></h3>'),
            
            fluidRow(
                     column(1),
                     column(style = "margin-top:5px",3,
                            selectInput("perFun",
                                        ' Function',
                                        c('Lomb-Scargle' = 'ls','Chi-squared' = 'chi-sq'),
                                        # c('Lomb-Scargle' = 'ls','Chi-squared' = 'chi-sq', 'Fourier' = 'fourier'),
                                        selected = 'ls')),
                     column(3,
                            sliderInput('periodogramValue','Oversampling', min = 1, max = 128, value = 32, step = 1)),
                     # column(3, sliderInput('sleepTime','Inactivity to consider sleep (minutes)', min = 1, max = 60, value = 5, step = 1)),
                     
                     column(style = "margin-top:5px",1,
                            numericInput('minPer','Min (h)' , value = 18, min = 1)),
                     column(style = "margin-top:5px",1,
                            numericInput('maxPer','Max (h)' , value = 32, min = 1)),
                     column(style = "margin-top:27px", 3, checkboxInput('showPeriods',HTML('<b>Show period points</b>') , FALSE))
            ),
            
            HTML('<h3><b>Fractal settings</b></h3>'),
            
            fluidRow(
              
              column(1),
              column(3,
                     sliderInput('Fractal_limits','Time-scale limits', min = 0.1, max = 24, value = c(0.1,8), step = 0.1)),
              column(3,
                     sliderInput('TimeScale_limits','Time-scale division', min = 0.1, max = 24, value = 1.5, step = 0.1)),
              column(2,
                     sliderInput('nFractal','Number of points', min = 10, max = 50, value = 30, step = 1)),
              column(2, 
                     checkboxInput('twoScaling',HTML('<b>Two scaling exponents</b>'), FALSE))
                     
              ),
            
            fluidRow(
              column(4, 
                     actionButton("periodogramAnalysis", "Periodogram analysis", class = "btn btn-primary",
                                  style="width: 100%; border-style: none;
                                         font-size:14pt; border-radius: 5px;")),
              column(4, 
                     actionButton("phaseAnalysis", "Phase, IS, IV and RA analysis", class = "btn btn-primary",
                                  style="width: 100%; border-style: none;
                                         font-size:14pt; border-radius: 5px;")),
              column(4, 
                     actionButton("fractalAnalysis", "Fractal analysis", class = "btn btn-primary",
                                  style="width: 100%; border-style: none;
                                         font-size:14pt; border-radius: 5px;")),
            )
            
      ),
      
      #Plot panel
      tabsetPanel(id = "periodogramTabs",
                  tabPanel("Periodogram", plotOutput("periodogram")),
                  tabPanel("Period", plotOutput("period")),
                  tabPanel("Power",plotOutput("power")),
      ),
      
      fluidRow(HTML("<h4><b>Statistical data</b></h4>")),
      fluidRow(DT::dataTableOutput("PeriodogramSummary")),
      
      fluidRow(style = "margin-top:10px",
               column(2, style = "margin-top:20px",
                      downloadButton("savePeriodogramFig", "Save figures")),
               column(2,
                      selectInput("periodogramFigures","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
               column(2),
               column(2, style = "margin-top:20px",
                      downloadButton("savePeriodogramData", "Save data"))),
      
      tabsetPanel(id = "phaseTabs",
                  tabPanel("Phase",plotOutput("phase")),
                  tabPanel("Phase per day",plotOutput("phasePerDay")),
                  tabPanel("Interdaily stability",plotOutput("is")),
                  tabPanel("Intradaily variation",plotOutput("iv")),
                  tabPanel("Intradaily variation per day",plotOutput("ivPerDay")),
                  tabPanel("Relative amplitude",plotOutput("ra")),
                  tabPanel("Relative amplitude per day",plotOutput("raPerDay"))
      ),
      
      
      fluidRow(HTML("<h4><b>Statistical data</b></h4>")),
      fluidRow(DT::dataTableOutput("PhaseSummary")),
      
      fluidRow(style = "margin-top:10px",
               column(2, style = "margin-top:20px",
                      downloadButton("savePhaseFig", "Save figures")),
               column(2,
                      selectInput("PhaseFigures","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
               column(2),
               column(2, style = "margin-top:20px",
                      downloadButton("savePhaseData", "Save data"))),
      
      tabsetPanel(id = "fractalTabs",
                  tabPanel("Fractal analysis",plotOutput("fractalAnalysis")),
      ),
      
      
      
      fluidRow(
        column(8,DT::dataTableOutput("FractalSummary"))),
      
      fluidRow(style = "margin-top:10px",
               column(2, style = "margin-top:20px",
                      downloadButton("savefractalFig", "Save image")),
               column(2,
                      selectInput("fractalFig","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
               column(2,),
               column(2, style = "margin-top:20px",
                      downloadButton("saveFractalReport", "Save data")),
      ),
      
    ))
)