tabPanel ("Variance Partitioning (AMOVA)",
          
          fluidRow(
          
          tabBox(title="Analysis AMOVA",
              width = 12,
              
              tabPanel(title = "Perform AMOVA Analysis",
              
              fluidRow(column(width = 12,
              
              includeMarkdown("www/PopulationStructure/PopulationStructure_AMOVA_RMD.Rmd"),
              )),
              
              fluidRow(column(width=12,
              actionButton("AMOVAGOGO","Perform AMOVA")
              ))
              
          ),
          
          tabPanel(title = "Methods and Code",
                   
                   fluidRow(
                     column(width = 12,
                            includeMarkdown("www/PopulationStructure/PopulationStructure_AMOVA_Methods_RMD.Rmd"),
                            
                            )
                   )
                   
                   )
          
          )
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
                
                
                
                fluidRow(
                  column(width = 6,
                         h4("Text Options"),
                         tags$hr()
                  ),
                  column(width = 4,
                         h4 ("Figure Options"),
                         tags$hr()
                  ),
                  column(width = 2,
                         h4 ("Download"),
                         tags$hr()
                  )
                  
                ), #end of fluid row
                
                fluidRow(
                  
                  column(width = 2,
                         
                        numericInput('amova_perc', 'Percentage label size', 
                                      value = 7, min = NA, max = NA, 
                                      step = 1, width = NULL),
                  
                  numericInput('amova_percpos', 'Percentage label position', 
                               value = 1.6, min = NA, max = NA, 
                               step = 0.05, width = NULL)
                         
                         
                      
                         
                  ),
                  
                  column(width = 2,
                         
                         textInput("amova_titletext", "Title text", value = ""),
                         
                         numericInput('amova_title', 'Title Size', 
                                      value = 25, min = NA, max = NA, 
                                      step = 1, width = NULL)
                                         ),
                  
                  
                  column(width = 2,
                         
                         textInput("amova_legendtext", "Legend text", value = "Comparison"),
                         
                         numericInput('amova_legend', 'Legend Text Size', 
                                      value = 15, min = NA, max = NA, 
                                      step = 1, width = NULL)
                         
                  ),
                  
                  column(width = 2,
                         
                         selectInput("amova_legendposition", label = ("Legend Position"), 
                                     choices = list("top" = "top",
                                                    "bottom" = "bottom",
                                                    "left" = "left",
                                                    "right" = "right"),
                                     selected = "right"),
                         
                         numericInput('amova_X', 'X Dimensions (pixels)', 
                                      value = 800, min = NA, max = NA, 
                                      step = 1, width = NULL)
                  ),
                  
                  column(width = 2,
                         
                         selectInput("amova_colpal", label = ("Color Palette"), 
                                     choices = list("Viridis palette default" = "D", 
                                                    "Viridis palette magma" = "magma", 
                                                    "Viridis palette inferno" = "inferno", 
                                                    "Viridis palette plasma" = "plasma", 
                                                    "Viridis palette cividis" = "cividis",
                                                    
                                                    "colorbrewer palette Set 1" = "scalecol1",
                                                    "colorbrewer palette Set 2" = "scalecol2",
                                                    "colorbrewer palette Set 3" = "scalecol3",
                                                    
                                                    "colorbrewer palette Blues" = "scalecolblues",
                                                    "colorbrewer palette Greens" = "scalecolgreens",
                                                    "colorbrewer palette Greys" = "scalecolgreys",
                                                    "colorbrewer palette Reds" = "scalecolreds",
                                                    
                                                    "colorbrewer palette Spectral" = "scalecolspectral"
                                                    ), 
                                     selected = "scalecol1",
                                     selectize = FALSE),
                         numericInput('amova_Y', 'Y Dimensions (pixels)', 
                                      value = 600, min = NA, max = NA, 
                                      step = 1, width = NULL)
                         
                  ),
                  column(width = 2,
                         
                         uiOutput("DL_amovabutton"),
                         
                         tags$br(),
                         
                         uiOutput("DL_amovabutton_tiff"),
                         
                         
                  )
                  
                  
                  
                  
                ), #end of fluidRow
                
                fluidRow(column(width = 12,
                
                div(style = 'overflow-x: scroll',
                    withSpinner(plotOutput("amovapie"))
                )))
                
                )
            
          )
          
)