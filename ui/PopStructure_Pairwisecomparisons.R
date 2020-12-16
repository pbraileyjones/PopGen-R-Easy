tabPanel ("Population Comparisons (Fst etc.)",
          
          fluidRow(box(title = "Pairwise Distances",
                       solidHeader = TRUE,
                       status="success",
                       width=12,
                       includeMarkdown("Abstracts/Pairwise_Abstract.Rmd"),
                       ) #end of box
          
          ), #end of fluidrow
          
          
          
          
          fluidRow(tabBox(title = "Fixation Index (Fst) Distance",
                          width=12,
                          
                          tabPanel(title = "Genetic Distance Matrix",
                                   
                                   fluidRow(
                                     
                                     column(width = 6,
                                            
                                            actionButton("pwfstGO", "Calculate pairwise distance")
                                     
                                            ) #end of column
                                     
                                   ), # End of fluid row
                                   
                                   tags$hr(),
                                   
                                   fluidRow(
                                     column(width = 12,
                                            
                                            div(style = 'overflow-x: scroll',
                                                withSpinner(DT::dataTableOutput("pwfst"))
                                            )
                                     )
                                   )
                                   
                          ), #End of tabpanel
                          
                          tabPanel("Distance Tree",
                                   
                                   fluidRow(
                                     column(width = 12,
                                            
                                            div(style = 'overflow-x: scroll',
                                            withSpinner(
                                              plotOutput("pwfst.tree", width = "100%")
                                            ) #End 'withSpinner
                                            )
                                     ) #end of column
                                   ) #end of fluidRow
                          ) #End of tab panel "Distance Tree"
                          
          )#end of box
          ), #End of fluid row
          
          
          fluidRow(tabBox(title = "Other genetic distances",
                       width=12,
                       
                       tabPanel(title = "Genetic Distance Matrix",
                       
                       fluidRow(
                       
                         column(width = 6,
                                
                       selectInput(
                         "gendist_other",
                         "Select genetic distance",
                         choices = c(
                           "Nei's standard distance (Ds)" = "Ds",
                           "Cavalli-Sforza choird distance (Dch)"= "Dch",
                           "Nei's Da distance (Da)" = "Da",
                           "Nei's minimum distance (Dm)" = "Dm",
                           "Roger's distance (Dr)" = "Dr",
                           "Prevosti's distance (Cp)" = "Cp",
                           "Sanghvi's distance (X2)" = "X2"
                         ),
                         selected = NULL,
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL)
                       ) #end of column
                       
                       ), # End of fluid row
                       
                       tags$hr(),
                       
                       fluidRow(
                         column(width = 12,
                       
                              div(style = 'overflow-x: scroll',
                                  withSpinner(DT::dataTableOutput("pwd"))
                              )
                         )
                       )
                       
                       ), #End of tabpanel
                       
                       tabPanel("Distance Tree",
                                
                                fluidRow(
                                  column(width = 12,
                                         div(style = 'overflow-x: scroll',
                                             withSpinner(
                                               plotOutput("ds.tree", width = "100%")
                                    )
                                    ) #End 'withSpinner
                                  ) #end of column
                                  ) #end of fluidRow
                                ) #End of tab panel "Distance Tree"
                       
          )#end of box
          )


          
) #end of tabpanel
