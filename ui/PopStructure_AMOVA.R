tabPanel ("Variance Partitioning (AMOVA)",
          
          fluidRow(
          
          box(title="Perform AMOVA",
              solidHeader = TRUE,
              status="success",
              width = 12,
              #includeMarkdown("https://github.com/pbraileyjones/easyRpopgen/blob/main/www/flavour_text/ft_AMOVA.Rmd"),
              actionButton("AMOVAGOGO","Perform AMOVA")),
          ),
          
          fluidRow(
            
            box(title = "AMOVA results",
                solidHeader = TRUE,
                status="success",
                width=12,
                div(style = 'overflow-x: scroll',
                    withSpinner(DT::dataTableOutput("amovatab"))
                )
            )
            
          ), #end of fluidroq
          
          fluidRow(
            
            box(title = "AMOVA piechart",
                solidHeader=TRUE,
                status="success",
                width = 12,
                
                div(style = 'overflow-x: scroll',
                    withSpinner(plotlyOutput("amovapie"))
                ),
                
                )
            
          )
          
)