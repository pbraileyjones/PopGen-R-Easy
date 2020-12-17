      navbarPage("Data Assessment and Filtering",
                   tabPanel ("Filtering",
                             
                             fluidRow(
                               
                               box(title= "Data Filtering",
                                   solidHeader = TRUE,
                                   status="success",
                                   width = 12,
                                   
                                   includeMarkdown("Abstracts/Filter_intro.Rmd")
                                   
                               )
                               
                             ), #end of fluidrow
                             
                             fluidRow(
                               
                               
                               box(title = "Filtering Parameters",
                                   solidHeader = TRUE,
                                   status = "success",
                                   width=12,
                                   numericInput("locf", label = h5("Missing Loci filtering value"), value = 0.05, step=0.01),
                                   numericInput("genf", label = h5("Missing Individuals filtering value"), value = 0.05,step=0.01),
                                   numericInput("maff", label = h5("Minor Allele Frequency (MAF) filtering value"), value = 0.01,step=0.01),
                                   
                                   awesomeCheckbox(inputId = "filterbyLD" , 
                                                   label = "LD filter" , 
                                                   value =TRUE, 
                                                   status = "success"),
                                   
                                   awesomeCheckbox(inputId = "filterbyHWE" , 
                                                   label = "HW filter" , 
                                                   value =TRUE, 
                                                   status = "success"),
                                   awesomeCheckbox(inputId = "filterbyFST" , 
                                                   label = "Filter non-neutral markers by FST (SNP Only)" , 
                                                   value =TRUE, 
                                                   status = "success"),
                                   
                                   
                                   materialSwitch(
                                     inputId = "filterdata",
                                     label = "Filter Data", 
                                     value = FALSE,
                                     status = "success"
                                   ),
                                   
                                   uiOutput("filterprogress"),
                                   
                               ) #end of box
                               
                             ), #end of fluidrow
                             
                             
                             fluidRow(
                               
                               box(title = "Filter Summary",
                                   solidHeader = TRUE,
                                   status = "success",
                                   width=12,
                                   
                                   uiOutput("filtersumtab")
                                   
                               )
                               
                             ),
                             
                             fluidRow(
                               box(title = "Filtered Data",
                                   solidHeader = TRUE,
                                   status = "success",
                                   width=12,
                                   uiOutput("filterdf2")
                                 
                             )
                   )
                   ), #end of tabPanel
                 
                 tabPanel(title = "Filtering methods and code",
                   
                   box(width = 12,
                       
                       includeMarkdown("www/DataFilter/DataFilter_Methods_RMD.Rmd")
                       )
                   
                 ) #end of tabPanel
                 #, 
                 #end of tabpanel
                   #tabPanel("Outlier Detection",
                    #        box(title = "Outlier Detection",
                     #           solidHeader = TRUE,
                      #          status = "success",
                       #         actionButton("pcadaptGO", "Find optimal k"),
                        #        sliderInput("pcadaptk","Optimal k",min=1,max=20,value=2, step = 1),
                         #       actionButton("pcadapt2GO","Run outlier detection")), #end of box
                          #  tabBox(
                           #   tabPanel("Optimal k",
                            #           withSpinner(plotlyOutput("pcadaptk"))
                             #          ),
                              #tabPanel("Outlier Summary",
                               #        verbatimTextOutput("pcadaptrun2"),
                                #       verbatimTextOutput("genindoutlier"),
                                 #      verbatimTextOutput("genindneutral"))
                            #)
                   #) 
        ) 