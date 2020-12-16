
        navbarPage("Ordinations",
                   tabPanel ("Principal Components Analysis (PCA)",
                             fluidRow(
                               box(title = "Principal Component Analysis",
                                   solidHeader = TRUE,
                                   status = "success",
                                   width= 12,
                                   includeMarkdown("Abstracts/PCA_abstract.Rmd"))
                             ),
                             
                             
                             fluidRow(
                               box(title = "PCA Plot Customization",
                                   solidHeader = TRUE,
                                   status = "success",
                                   width = 12,
                                   
                                   fluidRow(
                                     column(width = 4,
                                            h4("Text Sizes"),
                                            tags$hr()
                                     ),
                                     column(width = 2,
                                            h4 ("Figure Dimensions"),
                                            tags$hr()
                                     ),
                                     column(width = 3,
                                            h4 ("Point Options"),
                                            tags$hr()
                                     ),
                                     column(width = 3,
                                            h4 ("Download"),
                                            tags$hr()
                                     )
                                     
                                   ), #end of fluid row
                                 
                                   
                                 fluidRow(
                                   
                                   column(width = 2,
                                          
                                          numericInput('PCA_title', 'Title', 
                                                       value = 25, min = NA, max = NA, 
                                                       step = 1, width = NULL),

                                          numericInput('PCA_axistitle', 'Axis Title', 
                                                       value = 20, min = NA, max = NA, 
                                                       step = 1, width = NULL),
                                          numericInput('PCA_axistext', 'Axis Text', 
                                                       value = 15, min = NA, max = NA, 
                                                       step = 1, width = NULL),
                                   ),
                                   
                                   column(width = 2,
                                          
                                          
                                          numericInput('PCA_legend', 'Legend Text', 
                                                       value = 15, min = NA, max = NA, 
                                                       step = 1, width = NULL),
                                          selectInput("PCA_legendposition", label = ("Legend Position"), 
                                                      choices = list("top" = "top",
                                                                     "bottom" = "bottom",
                                                                     "left" = "left",
                                                                     "right" = "right"),
                                                      selected = "bottom")
                                          
                                   ),
                                   
                                   column(width = 2,
                                          
                                          numericInput('PCA_X', 'X (pixels)', 
                                                       value = 1000, min = NA, max = NA, 
                                                       step = 1, width = NULL),
                                          numericInput('PCA_Y', 'Y (pixels)', 
                                                       value = 500, min = NA, max = NA, 
                                                       step = 1, width = NULL)
                                   ),
                                   
                                   column(width = 3,
                                          
                                          selectInput("PCA_colpal", label = ("Color Palette"), 
                                                      choices = list("Viridis palette default" = "D", 
                                                                     "Viridis palette magma" = "magma", 
                                                                     "Viridis palette inferno" = "inferno", 
                                                                     "Viridis palette plasma" = "plasma", 
                                                                     "Viridis palette cividis" = "cividis"), 
                                                      selected = "D",
                                                      selectize = FALSE),
                                          numericInput('PCA_pointsize', 'Point Size', 
                                                       value = 2, min = NA, max = NA, 
                                                       step = 1, width = NULL),
                                          numericInput('PCA_alpha', 'Point Alpha', 
                                                       value = 1, min = 0, max = 1, 
                                                       step = 0.1, width = NULL),
                                          
                                          
                                   ),
                                   column(width = 3,
                                          
                                          uiOutput("DL_PCA_button"),
                                          
                                          tags$br(),
                                          
                                          uiOutput("DL_PCA_button_tiff"),
                                          
                                          tags$br(),
                                            
                                            uiOutput("DL_PCA_centroids_button"),
                                            
                                            tags$br(),
                                            
                                            uiOutput("DL_PCA_centroids_button_tiff")
                                            
                                          
                                   )
                                 
                               )
                               
                               
                             )
                             ),#end of fluidRow
                             
                             
                             
                             
                             
                             
                             
                             
                             fluidRow(
                               box(title="PCA Plot Generation",
                                   
                                   solidHeader = TRUE,
                                   status = "success",
                                   width = 4,
                                   
                                   actionButton(inputId = "PCAScreeGO", label = "Plot PCA Scree Plot"),
                                   actionButton(inputId = "PCAGO", label = "Perform PCA"),
                                   
                                   
                                   uiOutput("sliderPCA"),
                                   h4("Plotting Options"),
                                   uiOutput("colorPCA"),
                                   uiOutput("shapePCA"),
                                   sliderInput("PCAx", "x-axis", min = 1, max= 20, value = 1, step = 1),
                                   sliderInput("PCAy", "y-axis", min = 2, max= 20, value = 2, step = 1),
                                   awesomeCheckbox(
                                     inputId = "statellipsePCA",
                                     label = "Show normal data ellipsis", 
                                     value = TRUE,
                                     status = "success"
                                   ),
                                   awesomeCheckbox(
                                     inputId = "spiderPCA",
                                     label = "Show spider", 
                                     value = FALSE,
                                     status = "success"
                                   ),
                                   awesomeCheckbox(
                                     inputId = "showcentroidPCA",
                                     label = "Show centroid", 
                                     value = FALSE,
                                     status = "success"
                                   ),
                                   awesomeCheckbox(
                                     inputId = "plotcentroidonlyPCA",
                                     label = "Add centroid-only plot", 
                                     value = FALSE,
                                     status = "success"
                                   )
                                   
                                   
                                   
                                   

                               ),
                               
                               
                               tabBox(width = 8,
                                      
                                      tabPanel("PCA Plot",
                                               
                                               div(style = 'overflow-x: scroll; overflow-y: visible',
                                                   
                                                       withSpinner(plotOutput("PCAplot")),
                                                   ),
                                               
                                               
                                               uiOutput("PCAcentroid")
                                      ), #end of tabPanel
                                      tabPanel("Scree Plot",
                                               
                                               h4("Scree Plot of variance explained by each principal component"),
                                               
                                               div(style = 'overflow-x: scroll',
                                                   withSpinner(plotOutput("PCAscree2"))
                                               ),
                                               
                                               br(),
                                               
                                               fluidRow(
                                                 column(width = 2,
                                                        uiOutput("DL_PCA_scree_button")
                                                        ),
                                                 column(width = 2,
                                                        uiOutput("DL_PCA_scree_button_tiff")
                                                        )
                                                 
                                               ),
                                             
                                               hr(),
                                               
                                               h4("Table displaying variance explained by each principal component"),
                                               
                                               div(style = 'overflow-x: scroll',
                                                   withSpinner(dataTableOutput("PCAScree_tab"))
                                               )
                                               
                                               
                                      ) #end of tabPanel
                               )
                               
                             ) #End of fluidRow
                             
                             
                            
                   ), #end of tabPanel
                   
                   
                   
                   ############################################
                   ############################################
                   ######PRINCIPAL CO-ORDINATE ANALYSIS########
                   ############################################
                   ############################################
                   
                   tabPanel ("Principal Co-ordinates Analysis (PCoA)",
                             fluidRow(
                               box(title = "Principal Co-ordinates Analysis",
                                   solidHeader = TRUE,
                                   status = "success",
                                   width = 12,
                                   includeMarkdown("Abstracts/PCoA_abstract.Rmd"))
                               ), #end of fluidRow
                             
                             fluidRow(
                               box(title = "PCoA Plot Customization",
                                   solidHeader = TRUE,
                                   status = "success",
                                   width = 12,
                                   
                                   fluidRow(
                                     column(width = 4,
                                            h4("Text Sizes"),
                                            tags$hr()
                                     ),
                                     column(width = 2,
                                            h4 ("Figure Dimensions"),
                                            tags$hr()
                                     ),
                                     column(width = 3,
                                            h4 ("Point Options"),
                                            tags$hr()
                                     ),
                                     column(width = 3,
                                            h4 ("Download"),
                                            tags$hr()
                                     )
                                     
                                   ), #end of fluid row
                                   
                                   
                                   fluidRow(
                                     
                                     column(width = 2,
                                            
                                            numericInput('PCoA_title', 'Title', 
                                                         value = 25, min = NA, max = NA, 
                                                         step = 1, width = NULL),
                                            
                                            numericInput('PCoA_axistitle', 'Axis Title', 
                                                         value = 20, min = NA, max = NA, 
                                                         step = 1, width = NULL),
                                            numericInput('PCoA_axistext', 'Axis Text', 
                                                         value = 15, min = NA, max = NA, 
                                                         step = 1, width = NULL),
                                     ),
                                     
                                     column(width = 2,
                                            
                                            
                                            numericInput('PCoA_legend', 'Legend Text', 
                                                         value = 15, min = NA, max = NA, 
                                                         step = 1, width = NULL),
                                            selectInput("PCoA_legendposition", label = ("Legend Position"), 
                                                        choices = list("top" = "top",
                                                                       "bottom" = "bottom",
                                                                       "left" = "left",
                                                                       "right" = "right"),
                                                        selected = "bottom")
                                            
                                     ),
                                     
                                     column(width = 2,
                                            
                                            numericInput('PCoA_X', 'X (pixels)', 
                                                         value = 1000, min = NA, max = NA, 
                                                         step = 1, width = NULL),
                                            numericInput('PCoA_Y', 'Y (pixels)', 
                                                         value = 500, min = NA, max = NA, 
                                                         step = 1, width = NULL)
                                     ),
                                     
                                     column(width = 3,
                                            
                                            selectInput("PCoA_colpal", label = ("Color Palette"), 
                                                        choices = list("Viridis palette default" = "D", 
                                                                       "Viridis palette magma" = "magma", 
                                                                       "Viridis palette inferno" = "inferno", 
                                                                       "Viridis palette plasma" = "plasma", 
                                                                       "Viridis palette cividis" = "cividis"), 
                                                        selected = "D",
                                                        selectize = FALSE),
                                            numericInput('PCoA_pointsize', 'Point Size', 
                                                         value = 2, min = NA, max = NA, 
                                                         step = 1, width = NULL),
                                            numericInput('PCoA_alpha', 'Point Alpha', 
                                                         value = 1, min = 0, max = 1, 
                                                         step = 0.1, width = NULL),
                                            
                                            
                                     ),
                                     column(width = 3,
                                            
                                            uiOutput("DL_PCoA_button"),
                                            
                                            tags$br(),
                                            
                                            uiOutput("DL_PCoA_button_tiff"),
                                            
                                            tags$br(),
                                            
                                            uiOutput("DL_PCoA_centroids_button"),
                                            
                                            tags$br(),
                                            
                                            uiOutput("DL_PCoA_centroids_button_tiff")
                                            
                                            
                                     )
                                     
                                   )
                                   
                                   
                               )
                             ),#end of fluidRow
                             
                             
                             
                             
                             
                             
                             
                             
                             fluidRow(
                               box(title="PCoA Plot Generation",
                                   
                                   solidHeader = TRUE,
                                   status = "success",
                                   width = 4,
                                   
                                   actionButton(inputId = "PCoAScreeGO", label = "Plot PCoA Scree Plot"),
                                   actionButton(inputId = "PCoAGO", label = "Perform PCoA"),
                                   
                                   
                                   #uiOutput("sliderPCoA"),
                                   h4("Plotting Options"),
                                   uiOutput("colorPCoA"),
                                   uiOutput("shapePCoA"),
                                   sliderInput("PCoAx", "x-axis", min = 1, max= 20, value = 1, step = 1),
                                   sliderInput("PCoAy", "y-axis", min = 2, max= 20, value = 2, step = 1),
                                   awesomeCheckbox(
                                     inputId = "statellipsePCoA",
                                     label = "Show normal data ellipsis", 
                                     value = TRUE,
                                     status = "success"
                                   ),
                                   awesomeCheckbox(
                                     inputId = "spiderPCoA",
                                     label = "Show spider", 
                                     value = FALSE,
                                     status = "success"
                                   ),
                                   awesomeCheckbox(
                                     inputId = "showcentroidPCoA",
                                     label = "Show centroid", 
                                     value = FALSE,
                                     status = "success"
                                   ),
                                   awesomeCheckbox(
                                     inputId = "plotcentroidonlyPCoA",
                                     label = "Add centroid-only plot", 
                                     value = FALSE,
                                     status = "success"
                                   )
                                   
                                   
                                   
                                   
                                   
                               ),
                               
                               
                               tabBox(width = 8,
                                      
                                      tabPanel("PCoA Plot",
                                               
                                               div(style = 'overflow-x: scroll; overflow-y: visible',
                                                   
                                                   withSpinner(plotOutput("PCoAplot")),
                                               ),
                                               
                                               
                                               uiOutput("PCoAcentroid")
                                      ), #end of tabPanel
                                      tabPanel("Scree Plot",
                                               
                                               h4("Scree Plot of variance explained by each principal co-ordinate"),
                                               
                                               div(style = 'overflow-x: scroll',
                                                   withSpinner(plotOutput("PCoAscree2"))
                                               ),
                                               
                                               br(),
                                               
                                               fluidRow(
                                                 column(width = 2,
                                                        uiOutput("DL_PCoA_scree_button")
                                                 ),
                                                 column(width = 2,
                                                        uiOutput("DL_PCoA_scree_button_tiff")
                                                 )
                                                 
                                               ),
                                               
                                               hr(),
                                               
                                               h4("Table displaying variance explained by each principal co-ordinate"),
                                               
                                               div(style = 'overflow-x: scroll',
                                                   withSpinner(dataTableOutput("PCoAscree_tab"))
                                               )
                                               
                                               
                                      ) #end of tabPanel
                               )
                               
                             )
                             
                             
                   ) #end of 'PcoA' tabPanel
                   
                   
                   
                   
                   ###############################################
                   ###############################################
                   ######FACTORIAL CORRESPONDENCE ANALYSIS########
                   ###############################################
                   ###############################################
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
#                   tabPanel ("Factorial Correspondence Analysis (FCA)",
#                             fluidRow(
#                               box(title ="Factorial Correspondence Analysis",
#                                   solidHeader = TRUE,
#                                   status = "success",
#                                   width = 12,
#                                   includeMarkdown("Abstracts/FCA_abstract.Rmd"))
#                             ),
#                             fluidRow(
#                               box(title ="FCA Plot",
#                                   solidHeader = TRUE,
#                                   status = "success",
#                                   width=12,
#                                   withSpinner(plotlyOutput("fcaplot")))
#                             )
#                   )
                   
                   
        )#END OF NAVBAR PAGE FOR 'ORDINATIONS'
