div(
div ( style = "background-color:#F5F5F5; padding: 10pt;
                                   bord43er-radius: 5px;border-style: groove; font-size: 11pt",
      
      sidebarLayout(
        mainPanel(
          
          fluidRow(
            column(1),
            
            #Get directory of monitor files
            column(3,
                   fileInput("files","Choose files", multiple = TRUE, accept = "text/plain")
            ),
            
            column(3, 
                   selectInput("selectedFile","File selection", choices = "-")),
            
            column(3,
                   fileInput("importConditions","Import metadata",accept = ".xlsx")
            )
          ),
          
          fluidRow(style = "margin-top:10px",
                   column(1),
                   column(2,style ="margin-right:0",
                          dateInput("Start_date", 
                                    "Start Date")
                   ),
                   column(3, style="margin-left:0",
                          timeInput("start_time", "Start time", value = strptime("00:00:00", "%T"))
                   ),
                   column(2,
                          dateInput("Finish_date", 
                                    "Finish Date")
                   ),
                   column(3,
                          timeInput("finish_time", "Finish time", value = strptime("23:59:59", "%T"))
                   )),
          
          fluidRow( style = "margin-top:10px",
                    column(1),
                    column(3, timeInput("zeitgeberTime", "Light onset time (ZT0)", 
                                        value = strptime("08:00:00", "%T"))),
                    column(3, sliderInput('l_period','LD cycle period (hours)', min = 1, max = 48, value = 24, step = 0.5)),
                    column(3, sliderInput('l_hours','Light hours' , value = 14, max = 48, min=1, step=0.5))
          )
        ),
        sidebarPanel(style = "background:none; border:none",
                     fluidRow(
                       column(12,
                              tableOutput("FilesAdded"))
                     )
        )
      ),
      
),


#Start and Finish dates to analyze data
div ( style = "padding: 10pt; margin-top: 10px; font-size: 11pt",
      
      
      #Conditions
      
      tabsetPanel(id="animals",
        tabPanel("Add Conditions",
                 
                 style = "padding: 10pt; margin-top: 10px; font-size: 12pt",
                 
                 #Channels pannel
                 sidebarLayout(
                   
                   sidebarPanel(
                     
                     fluidRow(
                       column(6,
                              textInput("condition","Condition", width = '400px')),
                       column(6, style = "margin-top:27px",
                              actionButton("addcondition","Add Condition", 
                                  style="width: 165px; background-color: royalblue; border-style: none;
                                         color: white; font-size:11pt; border-radius: 5px;"))),
                     
                     DT::dataTableOutput("Channels", width = "80%"),
                     
                     tags$style("#Channels {
                                font-size:10px;
                                height:20px};"),
                     
                     tags$style ("#Channels .paginate_button { display: none}")
                   ),
                   
                   #Data panel
                   mainPanel( style = "border:none",
                              
                              fluidRow(
                                DT::dataTableOutput("Data")),
                              fluidRow(
                                column(style = "margin-bottom:10px", 2,
                                       # actionButton("saveMetadata", "Save metadata")
                                       downloadButton("saveMetadata", "Save Metadata")
                                       # shinySaveButton("saveMetadata", "Save Metadata", "Save file as ...", filetype=list(xlsx="xlsx"))
                                )))
                 ),
                 fluidRow(
                   actionButton("startanalysis","Start Analysis", 
                        style="width: 100%; background-color: #90EE90; border-style: none;
                              color: black; font-size:14pt; border-radius: 5px;")
                 )
        ),
        
        tabPanel("Remove dead animals",
                 style = "padding: 10pt; margin-top: 10px; font-size: 12pt",
                 
                 sidebarLayout(
                   sidebarPanel(
                     fluidRow(style="margin-top:10px",
                              
                              column(4, numericInput('deadTime','Death inactivity time' , 24)),
                              
                              column(4, style="margin-top:10px",
                                     radioButtons("removeWindow","", c("Minutes" = "mins", "Hours" = "hours"), selected = "hours", inline = TRUE)),
                              
                              column(3, style = "margin-top:27px",
                                     actionButton("remove","Find dead Larvae", style="width = 300px; 
                        border-style: none; font-size:11pt; border-radius: 5px;"))),
                     
                     fluidRow(DT::dataTableOutput("DeadTubes")),
                   
                     fluidRow(
                       column(6,actionButton("deleteLarvae","Remove dead Larvae", style="width = 300px; background-color: #FF6347; 
                        border-style: none; color: white; font-size:11pt; border-radius: 5px;")),
                       column(6,actionButton("deleteInactivity","Remove inactivity data", style="width = 300px; background-color: #FF6347; 
                        border-style: none; color: white; font-size:11pt; border-radius: 5px;")))),
                   
                 
                   mainPanel(
                     fluidRow(
                              column(2,),
                              
                       column(3, style="margin-top:10px",
                              checkboxInput("clean_data","Show data without inactivity")),
                       column(6,
                              radioButtons("Death_graphs","", c("All animals"="all", "Dead animals"="dead", "Live animals"="alive"), selected = "all", inline=TRUE)),

                     ),
                     plotOutput("checkChannels"))
                 )
        )
        
      ))
)
