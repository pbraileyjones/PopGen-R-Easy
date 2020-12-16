#Preparing files based on chosen strata

stratachoice<-reactive({
  
  if (input$diversitystrata == input$strat1) {
    1
  }
  
  else if (input$diversitystrata == input$strat2) {
    2
  }
  
  else if (input$diversitystrata == input$strat3) {
    3
  }
  
  else if (input$diversitystrata == input$strat4) {
    4
  }
  
  else if (input$diversitystrata == input$strat5) {
    5
  }
  
  else if (input$diversitystrata == input$strat6) {
    6
  }
  
})


output$choosediversitystrata<-renderUI({
  
  genind<-genind_filt()
  variables<-colnames(genind$strata)
  
  selectInput("diversitystrata", "Choose Strata", variables, selected = variables[1], multiple = FALSE,
              width = NULL, size = NULL)
  
})

output$choosediversitystratanest<-renderUI({
  
  genind<-genind_filt()
  variables<-colnames(genind$strata)
  
  selectizeInput("diversitystratanest", "Choose  two strata to compare", variables, selected = NULL, multiple = TRUE,
                 width = NULL, size = NULL, options = list(maxItems = 2))
  
})

output$chosenstratanest<-renderText({
  
  input$diversitystratanest
  
})

output$choosediversitystrataint<-renderUI({
  
  genind<-genind_filt()
  variables<-colnames(genind$strata)
  
  selectizeInput("diversitystrataint", "Choose  two strata to compare", variables, selected = NULL, multiple = TRUE,
                 width = NULL, size = NULL, options = list(maxItems = 2))
  
})

output$chosenstrataint<-renderText({
  
  input$diversitystrataint
  
})




output$stratabasedanalysis<-renderUI({
  
  if (input$stratdef == "One" || input$stratdef == "Two" || input$stratdef == "Three" || input$stratdef == "Four" || input$stratdef == "Five" || input$stratdef == "Six" ) {
    
    fluidRow(
      
      box(title = "Define strata for analysis",
          solidHeader = TRUE,
          status = "success", 
          width = 12,
          
          h5("If you chose to separate your population strata 
                            in the import section (e.g. pop = Factor1_Factor2 -> 
                            Strat1 = Factor1 and Strat2 = Factor2), then here you
                            can choose whether to summarize and analyze your data 
                            by the original population, or by the newly defined strata
                            variables that you created. If you did not define 
                            specific strata within your population then all summaries
                            will be made by the default 'pop' assignments"),
          
          selectInput("usestratadiv1", "Choose data structure to run analysis",
                      choices = c(
                        "Default: pop" ="nostrat",
                        "Use defined strata" = "strat"
                      ),
                      selected = "nostrat"),
          
          conditionalPanel("input.usestratadiv1 == 'strat'", 
                           uiOutput("choosediversitystrata"))
          
          
      )
      
    )
    
  }
  
})


genindanalysis<-reactive ({
  
  genind<-genind_filt()
  
  if (input$stratdef == "One" || input$usestratadiv1 == "nostrat") {
    
    genind
    
  }
  
  else {
    
    
    if (input$usestratadiv1 == "strat") {
      
      pop(genind)<-as.matrix(genind$strata[as.numeric(stratachoice())])
      genind
      
    }
  }
  
})
