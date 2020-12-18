#snapclust + DAPC
tabPanel("Admixture Analysis: Snapclust + DAPC",

fluidRow(
  box(title="Background",
      width= 12,
      solidHeader = TRUE,
      status = "success",
      
      includeMarkdown("www/PopulationStructure/PopulationStructure_SnapclustDAPC_RMD.Rmd")
      
  ) #end of box
), #end of fluidrow

fluidRow(
  
  box(title="Snapclust Analysis: Determine optimal number of clusters (k)",
      solidHeader=TRUE,
      status="success",
      width=12,
    
      fluidRow(
        
        column(width=6,
               
               includeMarkdown("Abstracts/snapclust_choosek.Rmd"),
               
               hr(),
               
               actionButton("snapclustoptkGO", "Plot AIC / BIC"),
               
               hr(),br(),
               
               sliderInput("snapclustk", "Define optimum k",
                      min = 1, max = 20,
                      value = 1, step = 1)
               
               ), #end of column
        
        column(width=6,
               
               fluidRow(
                 column(width=12,
               withSpinner(plotOutput("snapclustoptk")))
               ), #end of fluidrow
               
               fluidRow(
                 column(width =6,
                   uiOutput("DL_aicbic_button")
                 ),
                 column(width = 6,
                        uiOutput("DL_aicbic_button_tiff")
                        )
               )
               
               
               ) #end of column
 
      ) #end of fluidrow
    
  )#end of box 'Snapclust analysis'
  
), #end of fluidrow


conditionalPanel("input.snapclustk >1",

fluidRow(
  
  box(title = "Snapclust maximum likelihood assignment composition plots",
      solidHeader=TRUE,
      status="success",
      width =12,
      
      #Plot editing titles
      
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
               h4 ("Color Palette"),
               tags$hr()
               ),
        column(width = 3,
               h4 ("Download"),
               tags$hr()
               )

              ), #end of fluid row
      
      fluidRow(
        
        column(width = 2,
               
               numericInput('snapclust_compoplot_title', 'Title', 
                            value = 25, min = NA, max = NA, 
                            step = 1, width = NULL),
               numericInput('snapclust_compoplot_facet', 'Facet Text', 
                            value = 20, min = NA, max = NA, 
                            step = 1, width = NULL),
               numericInput('snapclust_compoplot_axistitle', 'Axis Title', 
                            value = 20, min = NA, max = NA, 
                            step = 1, width = NULL)
        ),
        
        column(width = 2,
          
          numericInput('snapclust_compoplot_axistext', 'Axis Text', 
                       value = 15, min = NA, max = NA, 
                       step = 1, width = NULL),
          numericInput('snapclust_compoplot_legend', 'Legend Text', 
                       value = 15, min = NA, max = NA, 
                       step = 1, width = NULL)
          
        ),
        
        column(width = 2,
               
               selectInput("snapclust_compoplot_legendposition", label = ("Legend Position"), 
                           choices = list("top" = "top",
                                          "bottom" = "bottom",
                                          "left" = "left",
                                          "right" = "right"),
                           selected = "bottom"),
               
               numericInput('snapclust_compoplot_X', 'X (pixels)', 
                            value = 1600, min = NA, max = NA, 
                            step = 1, width = NULL),
               numericInput('snapclust_compoplot_Y', 'Y (pixels)', 
                            value = 400, min = NA, max = NA, 
                            step = 1, width = NULL)
        ),
        
        column(width = 3,
               
               selectInput("snapclust_compoplot_colpal", label = ("Color Palette"), 
                           choices = list("Viridis palette default" = "D", 
                                          "Viridis palette magma" = "magma", 
                                          "Viridis palette inferno" = "inferno", 
                                          "Viridis palette plasma" = "plasma", 
                                          "Viridis palette cividis" = "cividis"), 
                           selected = "D",
                           selectize = FALSE),
               
               ),
        column(width = 3,
               
               uiOutput("DL_snapclust_compoplot_button"),
               
               tags$br(),
               
               uiOutput("DL_snapclust_compoplot_button_tiff"),
               
               
               )
        
        
        
        
      ),
      
      fluidRow(
        column(width = 12,
               div(style = 'overflow-x: scroll',
            withSpinner(plotOutput("snapclustcompoplot"))
            )
        ) #end of column
      
      )
      
      ) #end of box
  
), #end of fluidRow



fluidRow(
  
  tabBox(title="Snapclust maximum likelihood assignments",
         width=12,
         
         tabPanel("All Assignments",
           
                  br(),
                  
           downloadButton("downloadsnapassign", "Download Assignments"),
           
           br(),hr(),br(),
           
           div(style = 'overflow-x: scroll',
               withSpinner(DT::dataTableOutput("snapclustassignments"))
           
         )
         ), #end of tabpanel
         
         tabPanel("Admixed individuals",
                  
                  h5("Admixture is the sharing of genetic material between isolated 
                  population clusters, resulting from interbreeding. One of the 
                  purposes of population clustering analysis such as snapclust 
                  is to estimate the level of admixture between populations / within
                  individuals of each population cluster. 
                  There is no standard percentage of genetic material shared in 
                  an individual between a number of populations to determine 
                  whether whether an individual is admixed / hybridized so we 
                  have left this option open to the user to determine their own 
                  cutoff value. We have set this at 50, meaning that if an individual
                  has less than 50 % membership probability assigned to the 
                  population cluster in which they have the greatest membership 
                     probability assignment (and therefore no majority membership to one cluster), 
                     then they are classed as admixed."),
                  
                  hr(),
                  
                  numericInput('snapclust_admixcutoff', 'Admixture Cut-off (% probability of membership)', 
                               value = 50, min = NA, max = NA, 
                               step = 1, width = NULL),
                  
                  br(),
                  
                  div(style = 'overflow-x: scroll',
               withSpinner(DT::dataTableOutput("snapclustadmix"))
               
           )
         ) #end of tabpanel
  ) #end of tabbox
  
), #end of fluidrow

fluidRow(
  
  box(title="Snapclust Comparisons",
      solidHeader=TRUE,
      status="success",
      width=12,
      
      fluidRow(
        
        column(width = 6,
               
               div(style = 'overflow-x: scroll',
               withSpinner(DT::dataTableOutput("snapclustxoriginaltab"))
               )
               
               ), #end of column
        
        column(width = 6,
               
               withSpinner(plotOutput("snapclustxoriginalplot"))
               
               ) #end of column
        
        
      ) #end of internal fluidrow
      
      ) #end of box
  
), #end of fluidrow

#DAPC Analysis Outputs

fluidRow(
  
  tabBox(title = "Discriminate analysis of Principal Components (DAPC)",
         width = 12,
         
         tabPanel("Description"),
         
         tabPanel("Principal Component Determination",
                  
                  fluidRow(
                    column(width = 6,
                           "Information"
                           ),
                    
                    column(width = 6,
                           withSpinner(plotOutput("DAPCalpha"))
                           )
                    ) #end of fluidRow
                  ), #end of tabPanel
         
         tabPanel("Posterior Probabilities",
                  fluidRow(
                  column(width = 6,
                         div(style = 'overflow-x: scroll',
                             withSpinner(DT::dataTableOutput("DAPCass")))
                         ), 
                  column(width = 6,
                         
                         )
                  ) #end of fluidRow
                  ), #end of tabPanel
         
         tabPanel("Snapclust Comparison",
                  
                  fluidRow(
                    column(width = 6,
                           withSpinner(dataTableOutput("DAPCSnapcomptab"))
                           ),
                    column(width = 6,
                           withSpinner(plotOutput("DAPCSnapcompplot"))
                    )
                  ) #end of fluidRow
                  ) #end of tabPanel
        
  ) #end of tabBox
  
), #end of fluidRow



fluidRow(
  
  column(width = 5,
         
  fluidRow(
    
    box(title = "Plot Options",
        solidHeader=TRUE,
        status = "success",
        width=12,
        
        fluidRow(column(width = 12,
          h4(" Scatter Plot Visualization Options"),
                 tags$hr())
                 ), #end of fluidRow 
        
        fluidRow(
          column(width = 6,
                 numericInput('DAPC_point', 'Point Size', 
                              value = 2, min = NA, max = NA, 
                              step = 1, width = NULL)),
          column(width = 6,
                 numericInput('DAPC_transparent', 'Point alpha', 
                              value = 1, min = 0, max = 1, 
                              step = 0.05, width = NULL))
        ), #end of fluidRow
        
        fluidRow(
          column(width = 6,
                 awesomeCheckbox(
                   inputId = "statellipseDAPC",
                   label = "Show normal data ellipses", 
                   value = FALSE,
                   status = "success")),
          column(width = 6,
                 awesomeCheckbox(
                   inputId = "spiderDAPC",
                   label = "Show spider",
                   value = FALSE,
                   status = "success"))
        ), #end of fluidRow
        
        fluidRow(
          column(width = 6,
                 awesomeCheckbox(
                   inputId = "showcentroidDAPC",
                   label = "Show centroid",
                   value = FALSE,
                   status = "success")),
          column(width = 6,
                 awesomeCheckbox(
                   inputId = "showhvlineDAPC",
                   label = "Show zero intersections",
                   value = FALSE,
                   status = "success")
                 )
          ), #End of fluidRow  
        
        fluidRow(
          column(width = 12,
          h4("Text Options"),
                 tags$hr())
        ), #end of fluidRow 
         
        fluidRow(
          column(width = 6,
                 numericInput('DAPC_axistitle', 'Axis Title', 
                     value = 20, min = NA, max = NA, 
                     step = 1, width = NULL)),
          column(width = 6,
                 numericInput('DAPC_axistext', 'Axis Text', 
                              value = 15, min = NA, max = NA, 
                              step = 1, width = NULL))
        ), #end of fluidRow
        
        fluidRow(
          column(width = 6,
                 numericInput('DAPC_legend', 'Legend Text', 
                              value = 20, min = NA, max = NA,
                              step = 1, width = NULL)),
          column(width = 6,
                 selectInput("DAPC_legendposition", label = ("Legend Position"), 
                    choices = list("top" = "top",
                                   "bottom" = "bottom",
                                   "left" = "left",
                                   "right" = "right"),
                    selected = "right"))
        ), #end of fluidRow
        
        fluidRow(column(width = 12,
          h4("Figure Dimensions"),
                 tags$hr())
        ), #end of fluidRow 
        
        fluidRow(
          column(width = 6,
                 numericInput('DAPC_X', 'X (pixels)', 
                              value = 850, min = NA, max = NA, 
                              step = 1, width = NULL)),
          column(width = 6,
                 numericInput('DAPC_Y', 'Y (pixels)',
                              value = 400, min = NA, max = NA,
                              step = 1, width = NULL))
        ), #end of fluidRow
        
        fluidRow(column(width = 12,
          h4("Download"),
                 tags$hr())
        ), #end of fluidRow 
        
        fluidRow(
          column(width = 6,
                 uiOutput("DL_DAPC_scatter_button")),
          column(width = 6,
                 uiOutput("DL_DAPC_scatter_button_tiff"))
        ), #end of fluidRow
        
        br(),
        
        fluidRow(
          column(width=6,
                 uiOutput("DL_DAPC_density_button")),
          column(width = 6,
                 uiOutput("DL_DAPC_density_button_tiff"))
        ) #end of fluidRow
        
        ) #end of box
    
  ) #End of fluidRow
  
  ), #End of column
  
  
  column(width = 7,
  
  box(title = "DAPC Figures",
      solidHeader = TRUE,
      status="success",
      width = 12,
      
      fluidRow(
        column(width = 12,
               h3("DAPC Density Plot")
        )),
      
      br(),
      
      fluidRow(
        column(width = 12,
               div(style = 'overflow-x: scroll',
                   uiOutput("DAPCdensityrender")))),
      
      br(),
      hr(),
      br(),
      
      fluidRow(
        column(width = 12,
               h3("DAPC Scatter Plot")
        )),
                       
      fluidRow(
        column(width = 12,
               div(style = 'overflow-x: scroll',
                   uiOutput("DAPCscatterrender"))))
      ) #end of box
  )
  
) #end of fluidrow


), #End of conditional Panel snapclustk > 1

uiOutput("beforeyoucontinue")
                 
) #End of tabpanel