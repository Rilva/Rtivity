div(
  div ( style = "background-color:#F5F5F5; padding: 10pt;
                                   bord43er-radius: 5px; font-size: 11pt",
        
        sidebarLayout(
          mainPanel(
            fluidRow(
              column(1),
              column(4,
                     fileInput("FileToEvaluate","Choose file to analyze", multiple = FALSE, accept = "text/plain")
              ),
              column(3,style = "margin-top:25px",actionButton("evaluateData","Missing data imputation", class = "btn btn-success")),
              column(3,style = "margin-top:25px",
                     downloadButton("saveInputtedData", "Save file with imputted missing data"))),
            
          ),
          sidebarPanel(style = "background:none; border:none",
                       fluidRow(
                         column(12,
                                 textOutput("fileCheck"))
                       )
          ))))