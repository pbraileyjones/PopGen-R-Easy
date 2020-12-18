
        navbarPage(title = "Genetic Diversity",
                   
                   tabPanel("Population Calculations",
                            
                            uiOutput("stratabasedanalysis"),
                            
                            fluidRow(
                              
                              tabBox(title="Genetic Multiplicity",
                                  width = 12,
                                  
                                  tabPanel(title = "Description",
                                           
                                           h5("")
                                           
                                           ),
                                  
                                  tabPanel(title = "Summary of Multiplicity",
    
                                  div(style = 'overflow-x: scroll',
                                      withSpinner(DT::DTOutput("mult.tab"))
                                  )
                                  ), #end of tabPanel
                                  
                                  tabPanel("Other Mutliplicity Measures",
                                           div(style = 'overflow-x: scroll',
                                                   withSpinner(DT::DTOutput("mult.tab2"))
                                               )       
                                           ),
                                  
                                  tabPanel(title = "Statistical Analysis: Ar",
                                           
                                           div(style = 'overflow-x: scroll',
                                               
                                               withSpinner(DT::dataTableOutput("Arstats")),
                                               uiOutput("renderARposthoc"))
                                           
                                           ),
                                  tabPanel(title = "Statistical Analysis: pAr",
                                           
                                           div(style = 'overflow-x: scroll',
                                               
                                               withSpinner(DT::dataTableOutput("pArstats")),
                                               uiOutput("renderpARposthoc"))
                                           ),
                                  tabPanel(title = "Methods, Code and References",
                                           
                                           fluidRow(
                                               column(width = 12,
                                                      includeMarkdown("www/GeneticVariation/GeneticVariation_Multiplicity_Methods.Rmd")
                                               )))
                              ) #end of tabBox
                            ), #end of fluidRow
                              
                            
                            fluidRow(
                              
                              tabBox(title = "Genetic Diversity",
                                     width = 12,
                                  
                                     tabPanel(title = "Description",
                                              
                                              h5("")
                                              
                                     ),
                                     
                                     tabPanel(title = "Summary of Diversity",
                                     
                                  div(style = 'overflow-x: scroll',
                                      withSpinner(DT::dataTableOutput("divtab"))
                                  )
                                     ), #end of tabPanel
                                  
                                  tabPanel(title = "Statistical Analysis: FIS",
                                           
                                           uiOutput("renderFIScomp")
                                           
                                           ), 
                                  tabPanel(title = "Statistical Analysis: Ho",
                                           
                                           div(style = 'overflow-x: scroll',
                                               withSpinner(DT::dataTableOutput("Hostat")),
                                               uiOutput("renderHostat"))
                                           
                                           ),
                                  tabPanel(title = "Statistical Analysis: He",
                                           
                                           div(style = 'overflow-x: scroll',
                                               withSpinner(DT::dataTableOutput("Hestat")),
                                               uiOutput("renderHestat"))
                                           
                                           ),
                                  tabPanel(title = "Methods, Code and References",
                                           
                                           fluidRow(
                                               column(width = 12,
                                                      includeMarkdown("www/GeneticVariation/GeneticVariation_Diversity_Methods.Rmd")
                                                      )))
                                  )
                              
                              
                            ), # end of fluid row
                            
                            fluidRow(
                              
                              tabBox(title = "Allelic richness rarefaction curves",
                                     width=12,
                                     
                                     tabPanel("Description",
                                              
                                              h5("The number of alleles and private alleles (A and Ap) found in a population depends on the sampling
                                                 depth of individuals considered, as increasing the sampling effort also
                                                 increases the chance of finding new alleles. Ar and pAr are therefore used as a 
                                                 special case of A which corrects for sample size differences between populations
                                                 using rarefacton"),
                                              
                                              h5("Rarefied allelic richness values are summarized previously on this page. In this tab
                                                  you can view and download the rarefaction curves associated with each population. These curves show the rarefied Ar and pAr
                                                   values per opulation for each sample size point up until the minimum population sampling depth in the dataset. You can choose to calculate 
                                                 this for each sequential increase in sampling depth (which can take a minute or two), or for a subset of sampling depth (30 evenly distributed sampling 
                                                 depth points between zero and the minimum sampling depth) to increase the speed of calculation in exchange for a loss in precision.")
                                              ),
                                     
                                     tabPanel(title = "Allelic Richness",
                                             
                                              fluidRow(
                                                column(width = 2,
                                                       h4("Plot Options"),
                                                       tags$hr()),
                                                column(width = 4,
                                                       h4("Text Options"),
                                                       tags$hr()
                                                ),
                                                column(width = 2,
                                                       h4 ("Figure Options"),
                                                       tags$hr()
                                                ),
                                                column(width = 2,
                                                       h4 ("Color Palette"),
                                                       tags$hr()
                                                ),
                                                column(width = 2,
                                                       h4 ("Download"),
                                                       tags$hr()
                                                )
                                                
                                              ), #end of fluid row
                                              
                                              fluidRow(
                                                
                                                column(width = 2,
                                                       actionButton("rarecurveGO", "Render curve"),
                                                       
                                                       hr(),
                                                       br(),
                                                       
                                                       awesomeRadio(inputId = "arsteps" , 
                                                                    label = "Calculation steps" ,
                                                                    status = "success",
                                                                    choices = c("Calculate all points (smoother)" = "all",
                                                                                "Calculate a subset of points (faster)" = "partial")),
                                                       awesomeRadio(inputId = "Arcurve_error" , 
                                                                    label = "Error values" ,
                                                                    status = "success",
                                                                    choices = c("Standard Error (whiskers)" = "se_bar",
                                                                                "Standard Error (ribbon)" = "se_col",
                                                                                "Standard Deviation (whiskers)" = "sd_bar",
                                                                                "Standard Deviation (ribbon)" = "sd_col",
                                                                                "None" = "none"
                                                                    ),
                                                                    selected = "se_bar"),
                                                                    
                                                       ),
                                                
                                                column(width = 2,
                                                       
                                                       textInput("Arcurve_titletext", "Title text", value = "Allelic Richness Rarefaction Curves"),
                                                       
                                                       numericInput('Arcurve_title', 'Title Size', 
                                                                    value = 25, min = NA, max = NA, 
                                                                    step = 1, width = NULL),
                                                       numericInput('Arcurve_axistitle', 'Axis Title Size', 
                                                                    value = 20, min = NA, max = NA, 
                                                                    step = 1, width = NULL)
                                                ),
                                                
                                                column(width = 2,
                                                       
                                                       textInput("Arcurve_legendtext", "Legend text", value = "Population"),
                                                       
                                                       numericInput('Arcurve_axistext', 'Axis Text Size', 
                                                                    value = 15, min = NA, max = NA, 
                                                                    step = 1, width = NULL),
                                                       numericInput('Arcurve_legend', 'Legend Text Size', 
                                                                    value = 15, min = NA, max = NA, 
                                                                    step = 1, width = NULL)
                                                       
                                                ),
                                                
                                                column(width = 2,
                                                       
                                                       selectInput("Arcurve_legendposition", label = ("Legend Position"), 
                                                                   choices = list("top" = "top",
                                                                                  "bottom" = "bottom",
                                                                                  "left" = "left",
                                                                                  "right" = "right"),
                                                                   selected = "bottom"),
                                                       
                                                       numericInput('Arcurve_X', 'X Dimensions (pixels)', 
                                                                    value = 800, min = NA, max = NA, 
                                                                    step = 1, width = NULL),
                                                       numericInput('Arcurve_Y', 'Y Dimensions (pixels)', 
                                                                    value = 600, min = NA, max = NA, 
                                                                    step = 1, width = NULL)
                                                ),
                                                
                                                column(width = 2,
                                                       
                                                       selectInput("Arcurve_colpal", label = ("Color Palette"), 
                                                                   choices = list("Viridis palette default" = "D", 
                                                                                  "Viridis palette magma" = "magma", 
                                                                                  "Viridis palette inferno" = "inferno", 
                                                                                  "Viridis palette plasma" = "plasma", 
                                                                                  "Viridis palette cividis" = "cividis"), 
                                                                   selected = "D",
                                                                   selectize = FALSE),
                                                       
                                                ),
                                                column(width = 2,
                                                       
                                                       uiOutput("DL_ArRareCurvebutton"),
                                                       
                                                       tags$br(),
                                                       
                                                       uiOutput("DL_ArRareCurvebutton_tiff"),
                                                       
                                                       
                                                )
                                                
                                                
                                                
                                                
                                              ),
                                              
                                              hr(),
                                              
                                              div(style = 'overflow-x: scroll',
                                              
                                              withSpinner(plotOutput("ArRareCurve")),
                                              
                                              )
                                              
                                              ), #end of tabpanel
                                     
                                     
                                     
                                     
                                     
                                     tabPanel(title = "Private Allelic Richness",
                                              
                                              fluidRow(
                                                  column(width = 2,
                                                         h4("Plot Options"),
                                                         tags$hr()),
                                                  column(width = 4,
                                                         h4("Text Options"),
                                                         tags$hr()
                                                  ),
                                                  column(width = 2,
                                                         h4 ("Figure Options"),
                                                         tags$hr()
                                                  ),
                                                  column(width = 2,
                                                         h4 ("Color Palette"),
                                                         tags$hr()
                                                  ),
                                                  column(width = 2,
                                                         h4 ("Download"),
                                                         tags$hr()
                                                  )
                                                  
                                              ), #end of fluid row
                                              
                                              fluidRow(
                                                  
                                                  column(width = 2,
                                                         actionButton("prarecurveGO", "Render curve"),
                                                         
                                                         hr(),
                                                         br(),
                                                         
                                                         awesomeRadio(inputId = "parsteps" , 
                                                                      label = "Calculation steps" ,
                                                                      status = "success",
                                                                      choices = c("Calculate all points (smoother)" = "all",
                                                                                  "Calculate a subset of points (faster)" = "partial")),
                                                         awesomeRadio(inputId = "pArcurve_error" , 
                                                                      label = "Error values" ,
                                                                      status = "success",
                                                                      choices = c("Standard Error (whiskers)" = "se_bar",
                                                                                  "Standard Error (ribbon)" = "se_col",
                                                                                  "Standard Deviation (whiskers)" = "sd_bar",
                                                                                  "Standard Deviation (ribbon)" = "sd_col",
                                                                                  "None" = "none"
                                                                      ),
                                                                      selected = "se_bar"),
                                                         
                                                  ),
                                                  
                                                  column(width = 2,
                                                         
                                                         textInput("pArcurve_titletext", "Title text", value = "Private Allelic Richness Rarefaction Curves"),
                                                         
                                                         numericInput('pArcurve_title', 'Title Size', 
                                                                      value = 25, min = NA, max = NA, 
                                                                      step = 1, width = NULL),
                                                         numericInput('pArcurve_axistitle', 'Axis Title Size', 
                                                                      value = 20, min = NA, max = NA, 
                                                                      step = 1, width = NULL)
                                                  ),
                                                  
                                                  column(width = 2,
                                                         
                                                         textInput("pArcurve_legendtext", "Legend text", value = "Population"),
                                                         
                                                         numericInput('pArcurve_axistext', 'Axis Text Size', 
                                                                      value = 15, min = NA, max = NA, 
                                                                      step = 1, width = NULL),
                                                         numericInput('pArcurve_legend', 'Legend Text Size', 
                                                                      value = 15, min = NA, max = NA, 
                                                                      step = 1, width = NULL)
                                                         
                                                  ),
                                                  
                                                  column(width = 2,
                                                         
                                                         selectInput("pArcurve_legendposition", label = ("Legend Position"), 
                                                                     choices = list("top" = "top",
                                                                                    "bottom" = "bottom",
                                                                                    "left" = "left",
                                                                                    "right" = "right"),
                                                                     selected = "bottom"),
                                                         
                                                         numericInput('pArcurve_X', 'X Dimensions (pixels)', 
                                                                      value = 800, min = NA, max = NA, 
                                                                      step = 1, width = NULL),
                                                         numericInput('pArcurve_Y', 'Y Dimensions (pixels)', 
                                                                      value = 600, min = NA, max = NA, 
                                                                      step = 1, width = NULL)
                                                  ),
                                                  
                                                  column(width = 2,
                                                         
                                                         selectInput("pArcurve_colpal", label = ("Color Palette"), 
                                                                     choices = list("Viridis palette default" = "D", 
                                                                                    "Viridis palette magma" = "magma", 
                                                                                    "Viridis palette inferno" = "inferno", 
                                                                                    "Viridis palette plasma" = "plasma", 
                                                                                    "Viridis palette cividis" = "cividis"), 
                                                                     selected = "D",
                                                                     selectize = FALSE),
                                                         
                                                  ),
                                                  column(width = 2,
                                                         
                                                         uiOutput("DL_pArRareCurvebutton"),
                                                         
                                                         tags$br(),
                                                         
                                                         uiOutput("DL_pArRareCurvebutton_tiff"),
                                                         
                                                         
                                                  )
                                                  
                                                  
                                                  
                                                  
                                              ),
                                              
                                              hr(),
                                              
                                              
                                              div(style = 'overflow-x: scroll',
                                              withSpinner(plotOutput("pArRareCurve"))
                                              ),
                                              
                                              uiOutput("DLpArRareCurvebutton")
                                              
                                              ), #end of tabpanel
                                     
                                     tabPanel(title = "Methods, Code and References",
                                              
                                              fluidRow(
                                                  column(width = 12,
                                                         includeMarkdown("www/GeneticVariation/GeneticVariation_Multiplicity_RarefactionCurves_Methods.Rmd")
                                                  )))
                                     
                                     ) #end of tabbox
                              
                               #end of tabbox
                              
                            ), #end of fluidrow
                            
                            
                            
                            fluidRow(
                              
                              tabBox(title="Linkage Disequilibrium",
                                  width=12,
                                 
                                  tabPanel(title = "Population Level Calculation",
                                  
                                  fluidRow(
                                      
                                      column(width = 2,
                                             awesomeRadio(inputId = "LD_cc" , 
                                                          label = "Perform clone correction" ,
                                                          status = "success",
                                                          choices = c("Yes" = "yes",
                                                                      "No" = "no"))
                                             ),
                                  column(width = 2, 
                                  numericInput('LDpop_perm', 'Number of permutations', 
                                               value = 999, min = 0, max = NA, 
                                               step = 1, width = NULL),
                                  ),
                                  
                                  column(width = 2,
                                        
                                         actionButton("LDpopGO", "Calculate Linkage Disequilibrium")
                                  )
                                  
                                  
                                  ),
                                  
                                  hr(),
                                  fluidRow(column(width = 12,
                                  div(style = 'overflow-x: scroll',
                                  withSpinner(DT::dataTableOutput("LDpop"))
                                  )))
                                  ),
                                  tabPanel(title = "Methods, Code and References",
                                           
                                           fluidRow(
                                               column(width = 12,
                                                      includeMarkdown("www/GeneticVariation/GeneticVariation_LinkageDisequilibrium_Methods.Rmd")
                                               )))
                                  
                                  
                                  
                                  )
                              
                            )
                            ), #end of tabpanel
                   tabPanel("Background Information",
                            
                            box(title = NULL,
                                width=12,
                                
                                fluidRow(
                                    column(width = 12,
                                includeMarkdown("www/GeneticVariation/GeneticVariation_Background.Rmd")
                                    ) #end of column
                                ) #end of fluidRow
                            ), #end of box "Allele Frequencies"
                            
                            
                            
                            ) #end of tabPanel ("Background Information")
        ) #end of navbar page
        
        
