div(
  div ( style = "background-color:#F5F5F5; padding: 10pt;
                                   bord43er-radius: 5px; font-size: 11pt",
        
        sidebarLayout(
          mainPanel(
            fluidRow(
              column(1),
              column(4,
                     fileInput("FileToConvert","Choose file to convert bins", multiple = FALSE, accept = "text/plain")
              ),
              column(3,style = "margin-top:25px",actionButton("evaluateBins","Analyze bins", class = "btn btn-success"))),
            
          ),
          sidebarPanel(style = "background:none; border:none",
                       fluidRow(
                         column(12,
                                 textOutput("BinsInfo"))
                       )
          ))))