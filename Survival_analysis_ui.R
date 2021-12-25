div(
         sidebarLayout(
           sidebarPanel(width = 3,
                        
                        HTML(
                          '<h4><b> Conditions definitions </b></h4>'
                        ),
                        
                        fluidRow(
                          column(8, 
                                 selectInput("addedConditions","Choose condition", choices = c("None"), selected = "None"))),
                          
                        fluidRow(  
                          column(6, 
                                 colourInput("color", "Line color", "black", allowTransparent = TRUE)),
                          column(6,style = "margin-top:10px",
                                 radioButtons("pallete", "Limited palette?", choices = c("Yes" = "limited", "No" = "square"), selected = "square", inline = TRUE))
                          
                        ),
                        
                        
                        HTML('<h4><b> Plot definitions </b></h4>'),
                        
                        fluidRow(
                          column(12,
                                 actionButton('updateSurvivalGraphs','Update graphs', class = "btn btn-dark", 
                                              style = "width: 100%; border-style: none;
                                         font-size:12pt; border-radius: 5px;"))),
                        
                        fluidRow(style="margin-top:10px",
                                 column(6,
                                        selectInput('errorSurvival','Error bars', c("Standard error" = "mean_se", "Standard deviation" = "mean_sdl", "None" = NA), selected = "mean_se")),
                        
                                 radioButtons('SurvivalPlot','Survival plot type', c("Bar" = "BarPlot", "Box" = "BoxPlot", "Mean & Error" = "pointRange"),
                                              selected = "BarPlot", inline = TRUE)),
                        
                        HTML('<h4><b> Letters size </b></h4>'),
                        
                        fluidRow(
                          column(6,
                                 sliderInput("titleLetterSize","Title", min=6,max = 24,value= 16)),
                          column(6,
                                 sliderInput("axisLabelSize","Axis Labels", min=6,max = 24,value= 14)),
                          column(6,
                                 sliderInput("axisNumbersSize","Axis Numbers", min=6,max = 24,value= 12)),
                          column(6,
                                 sliderInput("dataLabelSize","Data Labels", min=6,max = 24,value= 13))
                        ),
                        
                        HTML('<h4><b> Title and axis definitions</b></h4>'),
                        fluidRow(
                          column(12, textInput("SurvivalTitle", "Graph title","")),
                        ),
                        
                        fluidRow(
                          column(12, textInput("yLabelSurvival", "Y axis label",""))),
                        
                        fluidRow(
                          column(12, textInput("xLabelSurvival", "X axis label",""))),
                        
                        fluidRow(
                          column(12, sliderInput('ySurvival','Y limits',  min=0,max = 150,value= c(0,100))),
                        ),
                        
                        fluidRow(
                          column(12,
                                 sliderInput('SurvivalWidth','Graph width', min = 500, max = 1200, value = 1200, step = 10))),
                        fluidRow(
                          column(12,
                                 sliderInput('SurvivalHeight','Graph Height', min = 250, max = 1500, value = 400, step = 10)))
                        
           ),
           
           
           mainPanel(
                     
                     div ( style = "background-color:#F5F5F5; padding: 10pt;
                                   border-radius: 5px; border-style:groove; font-size: 11pt; margin-bottom: 10px;",
                           
                           fluidRow(style="margin-top:10px",
                                    
                                    column(4, numericInput('deadTime','Inactivity to consider death (h)' , 12)),
                                    column(4, style = "margin-top:27px; margin-bottom:10px",
                                           actionButton("evaluateDeath","Evaluate dead animals", class = "btn btn-success",
                                                        style="border-style: none;
                                                  font-size:12pt; border-radius: 5px; width: 100%")),
                                    column(4, style="margin-top:25px",
                                           checkboxInput("clean_data","Show death analysis"))),
                           
                           fluidRow(style = "margin:20px",
                                    DT::dataTableOutput("DeadTubes")),
                           
                           fluidRow(style = "margin-top:20px",
                             column(3, style = "margin-bottom:27px",
                                    downloadButton("saveDeath", "Save death analysis")),
                             column(2, style = "margin-bottom:27px",
                                    downloadButton("saveDeathFigure", "Save figures")),
                             column(1, style = "margin-bottom:10px",
                                    selectInput("DeathFig","",choices = c(".png",".jpg",".tiff"), selected = ".png")),
                             
                            column(3,actionButton("deleteInactivity","Remove inactivity data", class="btn btn-warning", style="width = 300px; background-color: #FF6347; 
                        border-style: none; color: white; font-size:11pt; border-radius: 5px; width:100%")),
                            
                            column(3,actionButton("deleteAnimals","Remove dead animals", class="btn btn-warning", style="width = 300px; background-color: #FF6347; 
                        border-style: none; color: white; font-size:11pt; border-radius: 5px; width:100%"))),
                     
                           ),
                     
                     HTML("<h4><b> Dead animals </h4></b>"),
                     fluidRow(DT::dataTableOutput("survivalSummary")),
                     
                     tabsetPanel(id = "deadTabs",
                                 
                                 tabPanel("Actogram per channel",plotOutput("allChannels")),
                                 tabPanel("Dead animals",plotOutput("deadChannels")),
                                 tabPanel("Survival", plotOutput("Survival")),
                                 tabPanel("Survival over time", plotOutput("SurvivalTime")),
      
                     ),
                     
         )
         )
)