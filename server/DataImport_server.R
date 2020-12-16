#TAB:DATA INPUT

output$input<- DT::renderDataTable({
  
  req(input$file1)
  
  if (input$format == "gen") {
    tryCatch(
      {
        inputfile <- read.csv(input$file1$datapath,
                              header = FALSE,)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
    
  }
  
  else if (input$format == "structure") {
    tryCatch(
      {
        #inputfile <- read.csv(input$file1$datapath,
        #                      header = FALSE,)
        
        inputfile <- read.table(input$file1$datapath, sep = "\t", header=FALSE)
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
    
    DT::datatable(inputfile)
  }
})




test<-reactive({
  
  req(input$file1)
  
  if(input$format == "gen") {
    
    genind <- poppr::read.genalex(input$file1$datapath, ploidy=2, genclone=FALSE)
  }
  
  else if (input$format == "structure") {
    
    if (input$rowno == "One") {
      genind<-adegenet::read.structure(input$file1$datapath,
                             n.ind=input$indno,
                             n.loc=input$locno,
                             col.lab=1,
                             col.pop=2,
                             row.marknames=input$locrow,
                             onerowperind=TRUE,
                             col.others=0)
      
    }
    
    
    
    else if (input$rowno == "Two") {
      genind<-adegenet::read.structure(input$file1$datapath,
                             n.ind=input$indno,
                             n.loc=input$locno,
                             col.lab=1,
                             col.pop=2,
                             row.marknames=input$locno,
                             onerowperind=FALSE,
                             col.others=0)
      
      
    }
    
  }
  
  else if (input$format == "genepop") {
    
    suppressWarnings(
      genind<-import2genind(input$file1$datapath)
    )
    
    
  }
  
  
  })


output$genindsummary2<- renderPrint({
  
  req(input$file1)
  
  genind_filt()
  
})



genindstrat<- reactive({
  
  req(test)
  
  testgenind<-test()
  
  pop<-as.data.frame(testgenind@pop)
  testgenind$strata<-pop
  
  if (input$stratdef == "One") {
    names(testgenind$strata)[1] <- "pop"
    testgenind
    
  }
  
  else if (input$stratdef == "Two") {
    
    testgenind<- splitStrata(testgenind, ~ a/b, sep ="_")
    names(testgenind$strata)[1] <- input$strat1
    names(testgenind$strata)[2] <- input$strat2
    testgenind

    
  }
  
  else if (input$stratdef == "Three") {
    
    testgenind<- splitStrata(testgenind, ~ a/b/c, sep ="_")
    names(testgenind$strata)[1] <- input$strat1
    names(testgenind$strata)[2] <- input$strat2
    names(testgenind$strata)[3] <- input$strat3
    testgenind
    
  }
  
  else if (input$stratdef == "Four") {
    
    testgenind<- splitStrata(testgenind, ~ a/b/c/d, sep ="_")
    names(testgenind$strata)[1] <- input$strat1
    names(testgenind$strata)[2] <- input$strat2
    names(testgenind$strata)[3] <- input$strat3
    names(testgenind$strata)[4] <- input$strat4
    testgenind
    
  }
  
  else if (input$stratdef == "Five") {
    
    testgenind<- splitStrata(testgenind, ~ a/b/c/d/e, sep ="_")
    names(testgenind$strata)[1] <- input$strat1
    names(testgenind$strata)[2] <- input$strat2
    names(testgenind$strata)[3] <- input$strat3
    names(testgenind$strata)[4] <- input$strat4
    names(testgenind$strata)[5] <- input$strat5
    testgenind
    
  }
  
  else if (input$stratdef == "Six") {
    
    testgenind<- splitStrata(testgenind, ~ a/b/c/d/e/f, sep ="_")
    names(testgenind$strata)[1] <- input$strat1
    names(testgenind$strata)[2] <- input$strat2
    names(testgenind$strata)[3] <- input$strat3
    names(testgenind$strata)[4] <- input$strat4
    names(testgenind$strata)[5] <- input$strat5
    names(testgenind$strata)[6] <- input$strat6
    testgenind
    
  }
  

  
  
})



output$genindstrat3<-renderPrint({
  
  req(genindstrat)
  
  genind<-genindstrat()
  
  head(genind$strata)
  
})

