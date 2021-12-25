div(
div(
  tags$head(tags$style("#Data_treatment {font-size:18px; font-weight: bold}")),
  tabsetPanel(id="Data_treatment",
              
              
              tabPanel("Choose data",
                       
                       style = "background-color:#F5F5F5; padding: 10pt;
                                   border-style: groove;border-width: 2px;font-size: 11pt",
                       
                       sidebarLayout(
                         mainPanel(
                           
                           fluidRow(
                             column(1),
                             
                             #Get directory of monitor files
                             column(3,
                                    fileInput("files","Choose files", multiple = TRUE, accept = "text/plain")
                             ),
                             
                             column(4, 
                                    selectInput("selectedFile", "File selection", choices = "-")),
                             
                             column(3,
                                    fileInput("importConditions", "Import metadata",accept = ".xlsx")
                             )
                           ),
                           
                           fluidRow(style = "margin-top:10px",
                                    column(1),
                                    column(2,style ="margin-right:0",
                                           dateInput("Start_date", 
                                                     "Start Date")
                                    ),
                                    column(4, style="margin-left:0",
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
                                     column(3, sliderInput('l_hours','Light hours' , value = 12, max = 24, min=1, step=0.5))
                           )
                         ),
                         sidebarPanel(style = "background:none; border:none",
                                      fluidRow(
                                        column(12,
                                               tableOutput("FilesAdded"))
                                      )
                         )
                       )
              ),
              tabPanel("Missing value imputation",
                       id ="missingDataDiv",
                       style = "background-color:#F5F5F5; padding: 10pt;
                                   border-style: groove;border-width: 2px;font-size: 11pt",
              
                       sidebarLayout(
                         mainPanel(
                           fluidRow(
                             column(1),
                             column(4,
                                    fileInput("FileToEvaluate","Choose file with missing values", multiple = FALSE, accept = "text/plain")
                             ),
                             column(3,style = "margin-top:25px",actionButton("evaluateData","Interpolate missing values", class = "btn btn-success")),
                             column(3,style = "margin-top:25px",
                                    downloadButton("saveInputtedData", "Save file with imputted missing data"))),
                           
                         ),
                         sidebarPanel(style = "background:none; border:none",
                                      fluidRow(
                                        column(12, style = "margin-top:10px",
                                               textOutput("fileCheck"))
                                      )
                         ))
                       
                       )
  )
  
),


#Start and Finish dates to analyze data
div ( style = "padding: 10pt; margin-top: 10px; font-size: 11pt",
    
                 
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
                     
           tags$style ("#Channels .paginate_button { display: none}"),
                     
           fluidRow(
             actionButton("startanalysis","Start Analysis", class = "btn btn-success",
                          style="width: 100%; background-color: #90EE90; border-style: none;
                          font-size:14pt; border-radius: 5px;margin-top:20px")
            )
         ),
                   
         #Data panel
         mainPanel( style = "border:none",
                              
                    fluidRow(
                      DT::dataTableOutput("Data")),
                    fluidRow(
                      column(style = "margin-bottom:10px", 2,
                             downloadButton("saveMetadata", "Save Metadata")
                      )))
         ),
        
      )
)
