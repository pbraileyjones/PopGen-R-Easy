tabPanel("Admixture Analysis: STRUCTURE",
         
         fluidRow(
           
           box(title="About STRUCTURE",
               solidHeader = TRUE,
               status="success",
               width=12,
               
               includeMarkdown("Abstracts/STRUCTURE_abstract.Rmd")
               
           ) #end of box
         ), #end of fluidrow
         
         fluidRow(
           
           box(title = "Download Structure File",
               solidHeader = TRUE,
               status = "success",
               width = 12,
               
               h5("To analyse structure output you will first need to download a structure file 
                  to run this analysis separately as structure is a standalone program that is 
                  difficult to run from within the R environment. This may be integrated in to a future build of this app. "),
               
               br(),
               
               downloadButton("downloadstructure", "Download Structure File")
           ), #end of box
           
           box(title = "Import Structure Output Files",
               solidHeader = TRUE,
               status = "success",
               width = 12,
               
               h5("After running structure analysis across numerous values of 'k' (i.e. potential population clusters), 
                  and across multiple repeats, you should have a large number of files within the output folder."),
               
               h5("It is important that the individuals / a priori population hypotheses match between the genind object
                   saved within the shiny app and the STRUCTURE output files uploaded here. If these do not match then the 
                  following plots cannot be generated, or will be generated incorrectly. All you need to do is ensure that
                  the correct GenAlEx or STRUCTURE file associated with your output data is imported in the 'Data Import' tab."),
               
               fileInput("csvs", h5("Select all files from the structure run output folder to import"),
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv",
                                    "str",
                                    "stru")),
               
               sliderInput("structuretestk", label = h5("What range of k values did you test?"), min = 1, 
                           max = 25, value = c(1, 2))
               
           ) #end of box
           
         ), #End of fluidrow
         
         fluidRow(
           
           box(title = "Determine number of populations (k)",
               solidHeader = TRUE,
               status = "success",
               width = 12,
               
               withSpinner(plotOutput("evannostruc")),
               
               uiOutput("optclustslider")
               
           ) #end of box
           
         ), #end of fluidrow
         
         
         fluidRow(
           
           box(title="Visualization Options for Composition Plots (Optimal k)",
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
                        h4 ("Color Palette"),
                        tags$hr()
                 )
                 
               ), #end of fluidRow
               
               
               fluidRow(
                 
                 column(width = 2,
                        
                        numericInput('STRUCTURE_optk_title', 'Title', 
                                     value = 25, min = NA, max = NA, 
                                     step = 1, width = NULL),
                        numericInput('STRUCTURE_optk_facet', 'Facet Text', 
                                     value = 20, min = NA, max = NA, 
                                     step = 1, width = NULL),
                        numericInput('STRUCTURE_optk_axistitle', 'Axis Title', 
                                     value = 20, min = NA, max = NA, 
                                     step = 1, width = NULL)
                 ),
                 
                 column(width = 2,
                        
                        numericInput('STRUCTURE_optk_legend', 'Legend Text', 
                                     value = 15, min = NA, max = NA, 
                                     step = 1, width = NULL),
                        numericInput('STRUCTURE_optk_axistext', 'Axis Text', 
                                     value = 20, min = NA, max = NA, 
                                     step = 1, width = NULL)
                        
                 ),
                 
                 column(width = 2,
                        
                        selectInput("STRUCTURE_optk_legendposition", label = ("Legend Position"), 
                                    choices = list("top" = "top",
                                                   "bottom" = "bottom",
                                                   "left" = "left",
                                                   "right" = "right"),
                                    selected = "bottom"),
                        
                        numericInput('STRUCTURE_optk_X', 'X (pixels)', 
                                     value = 1500, min = NA, max = NA, 
                                     step = 1, width = NULL),
                        numericInput('STRUCTURE_optk_Y', 'Y (pixels)', 
                                     value = 400, min = NA, max = NA, 
                                     step = 1, width = NULL)
                 ),
                 
                 column(width = 3,
                        
                        selectInput("STRUCTURE_optk_colpal", label = ("Color Palette"), 
                                    choices = list("Viridis palette default" = "D", 
                                                   "Viridis palette magma" = "magma", 
                                                   "Viridis palette inferno" = "inferno", 
                                                   "Viridis palette plasma" = "plasma", 
                                                   "Viridis palette cividis" = "cividis"), 
                                    selected = "D",
                                    selectize = FALSE)
                 ),
               ),  #end of fluidrow
               
           ) #end of box
           
         ), #end of fluidrow
         
         
         fluidRow(
           
           tabBox(title = "Composition Plots (optimal k)",
                  width = 12,
                  
                  tabPanel(title = "By Individual / Sample (Optimal k)",
                    
                           div(style = 'overflow-x: scroll',
                           withSpinner(plotOutput("structureoptimal"))
                           )
                    
                  ), #end of tabPanel
                  
                  tabPanel( title = "By Population Hypothesis (Optimal k)",
                    
                            div(style = 'overflow-x: scroll',
                            withSpinner(plotOutput("structureoptimalmeans"))
                            )
                    
                            
                  ) #end of tabPanel
                  
                  
                  
                  )
           
         ), #end of fluidrow
         
         fluidRow(
           
           box(title="Visualization Options for Composition Plots (k range)",
               solidHeader = TRUE,
               status = "success",
               width = 12,
               
               fluidRow(
                 column(width = 3,
                        h4("K range"),
                        tags$hr()),
                 
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
                 )
                 
                 ), #end of fluidRow
                
               
               fluidRow(
                 
                 column(width = 3,
                        
                        uiOutput("strucvisrangeslider"),
                        actionButton("badscienceGO", "Render Plots")
                        
                        ),
                 
                 column(width = 2,
                        
                        numericInput('STRUCTURE_range_title', 'Title', 
                                     value = 25, min = NA, max = NA, 
                                     step = 1, width = NULL),
                        numericInput('STRUCTURE_range_facet', 'Facet Text', 
                                     value = 20, min = NA, max = NA, 
                                     step = 1, width = NULL),
                        numericInput('STRUCTURE_range_axistitle', 'Axis Title', 
                                     value = 20, min = NA, max = NA, 
                                     step = 1, width = NULL)
                 ),
                 
                 column(width = 2,
                        
                        numericInput('STRUCTURE_range_legend', 'Legend Text', 
                                     value = 15, min = NA, max = NA, 
                                     step = 1, width = NULL),
                        numericInput('STRUCTURE_range_axistext', 'Axis Text', 
                                     value = 20, min = NA, max = NA, 
                                     step = 1, width = NULL)
                        
                 ),
                 
                 column(width = 2,
                        
                        selectInput("STRUCTURE_range_legendposition", label = ("Legend Position"), 
                                    choices = list("top" = "top",
                                                   "bottom" = "bottom",
                                                   "left" = "left",
                                                   "right" = "right"),
                                    selected = "bottom"),
                        
                        numericInput('STRUCTURE_range_X', 'X (pixels)', 
                                     value = 1000, min = NA, max = NA, 
                                     step = 1, width = NULL),
                        numericInput('STRUCTURE_range_Y', 'Y (pixels)', 
                                     value = 1200, min = NA, max = NA, 
                                     step = 1, width = NULL)
                 ),
                 
                 column(width = 3,
                        
                        selectInput("STRUCTURE_range_colpal", label = ("Color Palette"), 
                                    choices = list("Viridis palette default" = "D", 
                                                   "Viridis palette magma" = "magma", 
                                                   "Viridis palette inferno" = "inferno", 
                                                   "Viridis palette plasma" = "plasma", 
                                                   "Viridis palette cividis" = "cividis"), 
                                    selected = "D",
                                    selectize = FALSE)
                        ),
                 ),  #end of fluidrow
               
           ) #end of box
           
         ), #end of fluidrow
        
         
         
         fluidRow(
           
           tabBox(title = "Composition Plots (k range)",
                  width = 12,
                  
                  tabPanel(title = "By Individual",
                           
                           div(style = 'overflow-x: scroll',
                           withSpinner(plotOutput("struccompoplot"))
                           )
                    
                  ), #end of tabPanel
                  
                  tabPanel(title = "By Population Hypothesis",
                           
                           div(style = 'overflow-x: scroll',
                           withSpinner(plotOutput("struclistmeanpop"))
                           )
                    
                  ) #end of tabPanel
                  
           ) #End of tabBox
           
         ), #end of fluidRow
         
         fluidRow(
           
           box(title= "STRUCTURE Assignments",
               solidHeader = TRUE,
               status="success",
               width = 8,
               
               div(style = 'overflow-x: scroll',
               DT::dataTableOutput("STRUCTUREassignments")
               )
               
               ), #end of box
           
           box(title = "Download Figures",
               solidHeader = TRUE,
               status="success",
               width=4,
               
               uiOutput("DL_evannostructure_button"),
               uiOutput("DL_evannostructure_button_tiff"),
               uiOutput("DL_STRUCTURE_optk_ind_button"),
               uiOutput("DL_STRUCTURE_optk_ind_button_tiff"),
               uiOutput("DL_STRUCTURE_optk_pop_button"),
               uiOutput("DL_STRUCTURE_optk_pop_button_tiff"),
               uiOutput("DL_STRUCTURE_rangek_ind_button"),
               uiOutput("DL_STRUCTURE_rangek_ind_button_tiff"),
               uiOutput("DL_STRUCTURE_rangek_pop_button"),
               uiOutput("DL_STRUCTURE_rangek_pop_button_tiff"),
               
               )
           
         ) #end of fluidrow
         
)#end of tabpanel

