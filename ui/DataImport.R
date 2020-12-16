navbarPage("Data Import",
           tabPanel ("Choose File",
                     
                     
                     fluidRow(
                       
                       box(title = "Project Name",
                           solidHeader = TRUE,
                           status = "success",
                           width = 12,
                           
                           textInput("projectID", label = "",  value = "projectID"),
                           
                           h5("The project name will be attached to any file downloaded from this app for identification purposes (file = 'ProjectID_output'') so name it appropriately for this")
                           
                           
                       
                     )
                              ),
                     
                     fluidRow(
                       box(title = "Import",
                           status = "success",
                           solidHeader = TRUE,
                           
                           # Input: Select a file ----
                                   fileInput("file1", h4("Choose File"),
                                             multiple = FALSE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        "csv",
                                                        "str","stru",
                                                        "gtx","gen","dat")
                                            ),
                           
                           h5("Data can be imported from files with the .csv filetype extension for GenAlEx formatted data, the 
                              .str and .stru filetype extensions for STRUCTURE formatted data, and the .gen, .gtx, and .dat filetype extensions
                              for Genepop, GENETIX, and Fstat formatted data respectively. Consult the 'Formatting data for import' 
                              tab to ensure that your data is correctly formatted to the specifications of each import type."),
                                   
                                   
                                   # Horizontal line ----
                                   tags$hr(),
                                   
                                   
                                   # Input: Select number of rows to display ----
                                   radioButtons("format", h4("Data Import Format"),
                                                choices = c("Genalex" = "gen",
                                                            "STRUCTURE" = "structure",
                                                            "GENETIX, Genepop or Fstat"= "other"),
                                                selected = "gen"),
                                   
                                   
                                   #Horizontal line ----
                                   tags$hr(),
                                   
                           conditionalPanel("input.format == 'other'",
                             
                                            h5("Make sure that your file extension aligns with the file type that you are importing. Genepop files
                                               must have the .gen filetype extension, GENETIX files must have the .gtx filetype extension, and 
                                               Fstat files must have the .dat filetype extension. The import function detects the filteype extension and uses
                                               this to inform how to read the data. Incorrect extensions will therefore result in incorrectly imported data or a failed import.
                                               ")
                                                           
                           ),
                           
                                   conditionalPanel("input.format == 'structure'",
                                                    
                                                    #Header for structure options
                                                    h4("Structure import options"),
                                                    
                                                    h5("The import function for STRUCTURE files is interractive and requires you to
                                                       input the variables below to convert the file to a genind object, please consult
                                                       the page 'Formatting data for import' for more information and to see how you
                                                       can find this information from viewing your STRUCTURE file if you do not already
                                                       know"),
                                                    
                                                    
                                                    #Input: What row are locus names in?
                                                    numericInput('locrow', 'Locus names row (0 = no separate row of locus names)', 
                                                                 value = 0, min = NA, max = NA, 
                                                                 step = 1, width = NULL),
                                                    
                                                    #Input: Structure specific input specifications
                                                    numericInput('indno', 'Number of Individuals', 0, min = NA, max = NA, step = NA,
                                                                 width = NULL),
                                                    
                                                    #Input: Structure specific input specifications
                                                    numericInput('locno', 'Number of loci', 0, min = NA, max = NA, step = NA,
                                                                 width = NULL),
                                                    
                                                    #Input: Structure specific input specifications
                                                    radioButtons("rowno", "Rows per Individual",
                                                                 choices = c("One" = "One",
                                                                             "Two" = "Two"
                                                                 ),
                                                                 selected = "One"))
                                   
                                   
                                   
                               ),
                               box(title = "Define data strata",
                                   status = "success",
                                   solidHeader = TRUE,
                                  
                                   selectInput("stratdef", label = h5("Number of Strata to define"), 
                                               choices = list("One (The default 'pop' from the input file will be used)" = "One", "Two" = "Two", "Three" = "Three", "Four" = "Four", "Five" = "Five", "Six" = "Six"), 
                                               selected = "One"),
                                   
                                   conditionalPanel("input.stratdef == 'Two'",
                                                    textInput("strat1", label = h5("Define First Strata"), value = "strat1"),
                                                    textInput("strat2", label = h5("Define Second Strata"), value = "strat2")
                                                    ),
                                   conditionalPanel("input.stratdef == 'Three'",
                                                    textInput("strat1", label = h5("Define First Strata"), value = "strat1"),
                                                    textInput("strat2", label = h5("Define Second Strata"), value = "strat2"),
                                                    textInput("strat3", label = h5("Define Third Strata"), value = "strat3")
                                                    ),
                                   conditionalPanel("input.stratdef == 'Four'",
                                                    fluidRow(
                                                      column(
                                                        width = 6,
                                                        textInput("strat1", label = h5("Define First Strata"), value = "strat1"),
                                                        textInput("strat2", label = h5("Define Second Strata"), value = "strat2"),
                                                        textInput("strat3", label = h5("Define Third Strata"), value = "strat3")
                                                      ), #end of column
                                                      column(
                                                        width = 6,
                                                        textInput("strat4", label = h5("Define Fourth Strata"), value = "strat4"),
                                                        
                                                      ) #end of column
                                                    ) #end of fluidrow
                                   ), #end of conditional panel
                                   conditionalPanel("input.stratdef == 'Five'",
                                                    fluidRow(
                                                      column(
                                                        width = 6,
                                                        textInput("strat1", label = h5("Define First Strata"), value = "strat1"),
                                                        textInput("strat2", label = h5("Define Second Strata"), value = "strat2"),
                                                        textInput("strat3", label = h5("Define Third Strata"), value = "strat3")
                                                      ), #end of column
                                                      column(
                                                        width = 6,
                                                        textInput("strat4", label = h5("Define Fourth Strata"), value = "strat4"),
                                                        textInput("strat5", label = h5("Define Fifth Strata"), value = "strat5"),
                                                       
                                                      ) #end of column
                                                    ) #end of fluidrow
                                   ), #end of conditional panel
                                   conditionalPanel("input.stratdef == 'Six'",
                                                    fluidRow(
                                                      column(
                                                        width = 6,
                                                        textInput("strat1", label = h5("Define First Strata"), value = "strat1"),
                                                        textInput("strat2", label = h5("Define Second Strata"), value = "strat2"),
                                                        textInput("strat3", label = h5("Define Third Strata"), value = "strat3")
                                                      ), #end of column
                                                      column(
                                                        width = 6,
                                                        textInput("strat4", label = h5("Define Fourth Strata"), value = "strat4"),
                                                        textInput("strat5", label = h5("Define Fifth Strata"), value = "strat5"),
                                                        textInput("strat6", label = h5("Define Sixth Strata"), value = "strat6")
                                                         ) #end of column
                                                    ) #end of fluidrow
                                                    ), #end of conditional panel
                                   
                                   verbatimTextOutput("genindstrat3")
                                   
                               )
                               
                             ),
                     
                     
              
                             fluidRow(
                               box(title = "Input Table Overview", 
                                   width = 12,
                                   status = "success",
                                   solidHeader = TRUE,
                                   div(style = 'overflow-x: scroll',
                                   DT::dataTableOutput("input")
                                   )
                               ) 
                             ),
                             fluidRow(
                               box(title = "Genind Summary",
                                   width = 12,
                                   status = "success",
                                   solidHeader = TRUE,
                                   verbatimTextOutput("genindsummary2")
                               )
                             )
                             
                   ),
                   tabPanel ("Formatting data for import",
                             fluidRow(
                             box(success = TRUE, 
                                 width = 12,
                                 solidHeader = TRUE,
                                 
                              fluidRow(
                                column(width=12,
                                  includeMarkdown("www/DataImport/DataImport_Formatting_RMD.Rmd") 
                                )
                              ) 
                               
                             )
                             )
                             )
                   
        )#END OF NAVBAR PAGE FOR 'DATA IMPORT'


