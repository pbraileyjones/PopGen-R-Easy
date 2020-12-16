#TAB: DATA FILTERING

#Reactive UI
#The progress bar when carrying out data filtering depends on the number of step you choose to carry out
filtsteps <- reactive({
  if (input$filterdata == TRUE) {
    #All filtering steps
    if ((input$filterbyLD == TRUE) && (input$filterbyHWE == TRUE) && input$filterbyFST == TRUE) {7}
    #Basic filtering + one other
    else if (((input$filterbyLD == TRUE) && (input$filterbyHWE == TRUE)) && (input$filterbyFST == FALSE ) || 
             ((input$filterbyLD == TRUE) && (input$filterbyHWE == FALSE)) && (input$filterbyFST == TRUE)  || 
             ((input$filterbyLD == FALSE) && (input$filterbyHWE == TRUE)) && (input$filterbyFST == TRUE)  ) {6}
    #Basic filtering + two others
    else if (((input$filterbyLD == TRUE) && (input$filterbyHWE == FALSE)) && (input$filterbyFST == FALSE ) || 
             ((input$filterbyLD == FALSE) && (input$filterbyHWE == TRUE)) && (input$filterbyFST == FALSE)  || 
             ((input$filterbyLD == FALSE) && (input$filterbyHWE == FALSE)) && (input$filterbyFST == TRUE)  ) {5}
    #Only basic filtering steps (monomorphs, missing data, minor allele frequency)
    else {4}
  }
})
#Progress Bar UI
output$filterprogress <- renderUI({
  if (input$filterdata == TRUE) {
    progressBar(id = "filterprog", value = 0, total = as.numeric(filtsteps()), status = "success", display_pct = TRUE, striped = TRUE, title = "Filtering Progress")
  }
})


output$DLfilterGenAlex <- renderUI({
  req(filtersummary)
  if (input$filterdata == TRUE) {
    downloadButton("DLfiltGenAlex", "Download Filtered GenAlex file (csv)")
  }
})

#Filter Switch
#When pressed this switch will initiate any chosen filtering steps
output$filtertext<-renderUI({
  if (input$filterGOGO == FALSE) {
    "Press switch to begin filtering"
  }
  else {
    "Filtering commenced"
  }
})

#********************************************************Filtering steps*******************************************************************

#Step 1
#Filter MAF
filtermon <- reactive({
  
  genind<-genindstrat()
  
  #Filter by polymorphism to remove any markers with NO POLYMORPHISM IN ALL SAMPLES
  temp<-summary(genind)
  Nalleles<-as.data.frame(temp$loc.n.all)
  colnames(Nalleles)<-"Nalleles"
  Nalleles$locus<-rownames(Nalleles)
  monomorphic<-subset(Nalleles, Nalleles == 1,) 
  monomorphic<-monomorphic$locus
  locnames<-as.data.frame(unique(genind$loc.fac)) #all loci present in dataset
  colnames(locnames)<-c("locnames")
  locnames$monomorphic<-locnames$locnames %in% monomorphic 
  locnames<-locnames[locnames$monomorphic == "FALSE",]
  locnames<-as.character(locnames$locnames) 
  monfilt <- genind[loc=c(locnames)]
  #Update Progres Bar
  updateProgressBar(session = session, id = "filterprog", value = 1, total = as.numeric(filtsteps()))
  #Output object
  monfilt
})

#Step 2
#Filter loci missing in above a user-provided percentage threshold of individuals
filterloc <- reactive({
  
  req(filtermon)
  monfilt<-filtermon()
  
  locfilt<-poppr::missingno(monfilt, type = "loci", cutoff= input$locf)  #Filter step
  #Update Progress Bar
  updateProgressBar(session = session, id = "filterprog", value = 2,total = as.numeric(filtsteps()))
  #Output object
  locfilt
})

#Step 3
#Filter individuals missing above a user-provided percentage threshold of loci
filterind <- reactive({
  
  req(filterloc)
  locfilt <- filterloc()
  
  genfilt<-poppr::missingno(locfilt, type="geno", cutoff= input$genf) #Filter step
  #Update Progress Bar
  updateProgressBar(session = session, id = "filterprog", value = 3,total = as.numeric(filtsteps()))
  #Output object
  genfilt
})

#Step 4
#Filter by minor allele frequency
filtermaf <- reactive({ 
  
  req(filterind)
  genfilt <- filterind()
  
  maffilt<-poppr::informloci(genfilt, MAF=input$maff, quiet = FALSE)  #Filter step
  #Update Progress Bar
  updateProgressBar(session = session, id = "filterprog", value = 4,total = as.numeric(filtsteps()))
  #Output object
  maffilt
})

#Optional Step
#Filter loci in linkage disequilibrium
filterLD <- reactive({
  
  req(filtermaf)
  maffilt <- filtermaf()
  
  if (input$filterbyLD == TRUE) {
    link<-as.data.frame(poppr::pair.ia(maffilt)) #Very computationally expensive step is sample = >0
    link_sub<-subset(link, rbarD>0.70) #Identify loci pairs in LD
    link_sub$LD <- rownames(link_sub)
    LD<-stringr::str_split_fixed(link_sub$LD, ":", 2) #Split column in to individual loci so you can pick one to remove
    LD<-LD[,1] #Remove one of two linked loci
    LD #List of loci to be removed                
    locnames<-as.data.frame(unique(maffilt$loc.fac)) #all loci present in dataset
    colnames(locnames)<-c("locnames")
    locnames$LD<-locnames$locnames %in% LD #Attatch TRUE to loci which are in LD
    locnames<-locnames[locnames$LD == "FALSE",]
    locnames<-as.character(locnames$locnames) #List of loci to keep
    LDfilt <- maffilt[loc=c(locnames)] #subset genind object by loci that do not violate LD
    #Update Progress Bar
    updateProgressBar(session = session, id = "filterprog", value = 5, total = as.numeric(filtsteps()))
    #Output object
    LDfilt
  }
  else{
    maffilt #Non-LD filtered object if LD filtering is not chosen
  }
})

#Optional Step
#Filter loci not in H-W equilibrium in > 50 % of samples
filterHW <- reactive({
  
  req(filterLD)
  LDfilt<-filterLD()
  
  if (input$filterbyHWE == TRUE) {
    #Filter Hardy-Weinsten (Out of HWE in > 50% samples as per Wyngaarden et al. 2018 https://doi.org/10.1002/ece3.3846)
    #Hardy Weinberg calculation used later
    temp <- seppop(LDfilt) %>% lapply(pegas::hw.test, B =0)
    temp2<-as.data.frame(temp[1])
    L.hwe <- plyr::ldply(temp, data.frame)
    L.hwe$locus<-rownames(temp2) #Need to figure out how to change this one
    colnames(L.hwe)<-c("population", "chi^2","df", "p", "locus")
    L.hwe<-L.hwe[c("population", "locus", "chi^2", "p")]
    L.hwe$Bon.p<- p.adjust(L.hwe$p, method = "bonferroni", n = length(L.hwe$p))
    L.hwe.sig<-L.hwe[L.hwe$Bon.p < 0.01,]
    
    npops<-length(unique(LDfilt$pop))
    HWD<-plyr::count(L.hwe.sig$locus)
    HWD$Proportion<-HWD$freq / npops *100
    HWD_sub<-subset(HWD, Proportion >= 50)
    HW<-as.vector(HWD_sub$x)
    locnames<-as.data.frame(unique(LDfilt$loc.fac)) #all loci present in dataset
    colnames(locnames)<-c("locnames")
    locnames$HW<-locnames$locnames %in% HW #Attatch TRUE to SNPs which deviate from HWE
    locnames<-locnames[locnames$HW == "FALSE",]
    locnames<-as.character(locnames$locnames) #List of loci to keep
    HWfilt <- LDfilt[loc=c(locnames)] #subset genind object by loci that do not deviate from HWE
    #Update Progresss Bar
    if (input$filterbyLD == FALSE) {
      updateProgressBar(session = session, id = "filterprog", value = 5, total = as.numeric(filtsteps()))
    }
    else {
      updateProgressBar(session = session, id = "filterprog", value = 6, total = as.numeric(filtsteps()))
    }
    #Output object
    HWfilt
  }
  else if (input$filterbyHWE == FALSE) {
    LDfilt
  }
})


#Optional Step
#Filter Fst outliers to remove those under selection pressure
filterFST <- reactive({
  
  req(filterHW)
  HWfilt <- filterHW()
  
  if (input$filterbyFST == TRUE) {
    genlight<-gi2gl(HWfilt) #Create genlight object from genind object
    outflank<-gl.outflank(genlight, plot = FALSE) #Perform FST outlier testing with the outflank algorithm
    outflank<-as.data.frame(outflank$outflank) #Create dataframe of output
    outflank<-cbind(outflank[6], outflank[17:20]) #Subset only important columns
    #Remove base names added
    outflank$results.LocusName<-stringr::str_remove(outflank$results.LocusName, pattern = c(".A"))
    outflank$results.LocusName<-stringr::str_remove(outflank$results.LocusName, pattern = c(".T"))
    outflank$results.LocusName<-stringr::str_remove(outflank$results.LocusName, pattern = c(".C"))
    outflank$results.LocusName<-stringr::str_remove(outflank$results.LocusName, pattern = c(".G"))
    #Subset SNPs which are Fst outliers
    outflank <- subset(outflank, outflank$results.OutlierFlag == TRUE )
    
    if (length(outflank$results.LocusName) == 0) { #no filtering necessary
      #Update Progresss Bar
      if (input$filterbyLD == FALSE & input$filterbyHWE == FALSE) {
        updateProgressBar(session = session, id = "filterprog", value = 5, total = as.numeric(filtsteps()))
      }
      else if (input$filterbyLD == TRUE && input$filterbyHWE == FALSE | input$filterbyLD == FALSE && input$filterbyHWE == TRUE) {
        updateProgressBar(session = session, id = "filterprog", value = 6, total = as.numeric(filtsteps()))
      }
      else if (input$filterbyLD == TRUE & input$filterbyHWE == TRUE) {
        updateProgressBar(session = session, id = "filterprog", value = 7, total = as.numeric(filtsteps()))
      }
      #Output object
      HWfilt
    }
    else if (length(outflank$results.LocusName > 0)) { #Filter FST outliers
      fstoutliers<-as.character(outflank$results.LocusName)
      locnames<-as.data.frame(unique(HWfilt$loc.fac)) #all loci present in dataset
      colnames(locnames)<-c("locnames")
      locnames$outlier<-locnames$locnames %in% fstoutliers #Attatch TRUE to loci which are in LD
      locnames<-locnames[locnames$outlier == "FALSE",]
      locnames<-as.character(locnames$locnames) #List of loci to keep
      fstfilt <- HWfilt[loc=c(locnames)] #subset genind object by loci which are neutral
      #Update Progresss Bar
      if (input$filterbyLD == FALSE & input$filterbyHWE == FALSE) {
        updateProgressBar(session = session, id = "filterprog", value = 5, total = as.numeric(filtsteps()))
      }
      else if (input$filterbyLD == TRUE && input$filterbyHWE == FALSE | input$filterbyLD == FALSE && input$filterbyHWE == TRUE) {
        updateProgressBar(session = session, id = "filterprog", value = 6, total = as.numeric(filtsteps()))
      }
      else if (input$filterbyLD == TRUE & input$filterbyHWE == TRUE) {
        updateProgressBar(session = session, id = "filterprog", value = 7, total = as.numeric(filtsteps()))
      }
      #Output object
      fstfilt
    }
  }
  else if (input$filterbyFST == FALSE){
    HWfilt
  }
})

#Final Filtered (or not) genind object
genind_filt<- reactive ({
  if (input$filterdata == TRUE) {
    filterFST()
  }
  else if (input$filterdata == FALSE) {
    genindstrat()
  }
})

#Summary table of removed loci and individuals
filtersummary <- reactive({
  if (input$filterdata == TRUE) {
    genind<-genindstrat()
    monfilt<- filtermon()
    locfilt<- filterloc()
    genfilt<- filterind()
    maffilt<- filtermaf()
    LDfilt<- filterLD()
    HWfilt<- filterHW()
    FSTfilt<-filterFST()
    
    #Loci and individuals dropped at each filtering step
    y <- data.frame(Individuals=numeric(), Loci=numeric())
    #Initial values
    y[1,1]<-summary(genind)$n
    y[1,2]<-sum(summary(genind)$loc.n.all/summary(genind)$loc.n.all)
    #Remove monomorphs
    y[2,1]<-summary(monfilt)$n
    y[2,2]<-sum(summary(monfilt)$loc.n.all/summary(monfilt)$loc.n.all)
    #Remove missing loci
    y[3,1]<-summary(locfilt)$n
    y[3,2]<-sum(summary(locfilt)$loc.n.all/summary(locfilt)$loc.n.all)
    #Remove missing individuals
    y[4,1]<-summary(genfilt)$n
    y[4,2]<-sum(summary(genfilt)$loc.n.all/summary(genfilt)$loc.n.all)
    #Remove 'non-informative' loci with MAF <0.01
    y[5,1]<-summary(maffilt)$n
    y[5,2]<-sum(summary(maffilt)$loc.n.all/summary(maffilt)$loc.n.all)
    
    if ((input$filterbyLD == TRUE) && (input$filterbyHWE == TRUE) && (input$filterbyFST == TRUE)) {
      
      #Linkage disequilibrium
      y[6,1]<-summary(LDfilt)$n
      y[6,2]<-sum(summary(LDfilt)$loc.n.all/summary(LDfilt)$loc.n.all)
      #HWE
      y[7,1]<-summary(HWfilt)$n
      y[7,2]<-sum(summary(HWfilt)$loc.n.all/summary(HWfilt)$loc.n.all)
      #FST
      y[8,1]<-summary(FSTfilt)$n
      y[8,2]<-sum(summary(FSTfilt)$loc.n.all/summary(FSTfilt)$loc.n.all)
      Filter<-c( "No Filter", "Monomorphs", paste("Loci missing > ",input$locf, "%"), paste("Ind missing > ", input$genf,"%"),paste("MAF < ",input$maff),"Linkage Disequilibrium","Hardy-Weinberg", "FST outlier loci")
    }
    
    if ((input$filterbyLD == TRUE) && (input$filterbyHWE == TRUE) && (input$filterbyFST == FALSE)) {
      
      #Linkage disequilibrium
      y[6,1]<-summary(LDfilt)$n
      y[6,2]<-sum(summary(LDfilt)$loc.n.all/summary(LDfilt)$loc.n.all)
      #HWE
      y[7,1]<-summary(HWfilt)$n
      y[7,2]<-sum(summary(HWfilt)$loc.n.all/summary(HWfilt)$loc.n.all)
      Filter<-c( "No Filter", "Monomorphs", paste("Loci missing > ",input$locf, "%"), paste("Ind missing > ", input$genf,"%"),paste("MAF < ",input$maff),"Linkage Disequilibrium","Hardy-Weinberg")
    }
    
    if ((input$filterbyLD == TRUE) && (input$filterbyHWE == FALSE) && (input$filterbyFST == TRUE)) {
      
      #Linkage disequilibrium
      y[6,1]<-summary(LDfilt)$n
      y[6,2]<-sum(summary(LDfilt)$loc.n.all/summary(LDfilt)$loc.n.all)
      #FST
      y[7,1]<-summary(FSTfilt)$n
      y[7,2]<-sum(summary(FSTfilt)$loc.n.all/summary(FSTfilt)$loc.n.all)
      Filter<-c( "No Filter", "Monomorphs", paste("Loci missing > ",input$locf, "%"), paste("Ind missing > ", input$genf,"%"),paste("MAF < ",input$maff),"Linkage Disequilibrium", "FST outlier loci")
    }
    
    if ((input$filterbyLD == FALSE) && (input$filterbyHWE == TRUE) && (input$filterbyFST == TRUE)) {
      
      #HWE
      y[6,1]<-summary(HWfilt)$n
      y[6,2]<-sum(summary(HWfilt)$loc.n.all/summary(HWfilt)$loc.n.all)
      #FST
      y[7,1]<-summary(FSTfilt)$n
      y[7,2]<-sum(summary(FSTfilt)$loc.n.all/summary(FSTfilt)$loc.n.all)
      Filter<-c( "No Filter", "Monomorphs", paste("Loci missing > ",input$locf, "%"), paste("Ind missing > ", input$genf,"%"),paste("MAF < ",input$maff),"Hardy-Weinberg", "FST outlier loci")
    }
    
    else if ((input$filterbyLD == TRUE) && (input$filterbyHWE == FALSE) && (input$filterbyFST == FALSE)) {
      
      #Linkage disequilibrium
      y[6,1]<-summary(LDfilt)$n
      y[6,2]<-sum(summary(LDfilt)$loc.n.all/summary(LDfilt)$loc.n.all)
      Filter<-c( "No Filter", "Monomorphs", paste("Loci missing > ",input$locf, "%"), paste("Ind missing > ", input$genf,"%"),paste("MAF < ",input$maff),"Linkage Disequilibrium")
    }
    
    else if ((input$filterbyLD == FALSE) && (input$filterbyHWE == TRUE) && (input$filterbyFST == FALSE)) {
      
      #HWE
      y[6,1]<-summary(HWfilt)$n
      y[6,2]<-sum(summary(HWfilt)$loc.n.all/summary(HWfilt)$loc.n.all)
      Filter<-c( "No Filter", "Monomorphs", paste("Loci missing > ",input$locf, "%"), paste("Ind missing > ", input$genf,"%"),paste("MAF < ",input$maff),"Hardy-Weinberg")
    }
    
    else if ((input$filterbyLD == FALSE) && (input$filterbyHWE == FALSE) && (input$filterbyFST == TRUE)) {
      
      #FST
      y[6,1]<-summary(FSTfilt)$n
      y[6,2]<-sum(summary(FSTfilt)$loc.n.all/summary(FSTfilt)$loc.n.all)
      Filter<-c( "No Filter", "Monomorphs", paste("Loci missing > ",input$locf, "%"), paste("Ind missing > ", input$genf,"%"),paste("MAF < ",input$maff),"FST outlier loci")
    }
    
    else if ((input$filterbyLD == FALSE) && (input$filterbyHWE == FALSE) && (input$filterbyFST == FALSE)) {
      
      Filter<-c( "No Filter", "Monomorphs", paste("Loci missing > ",input$locf, "%"), paste("Ind missing > ", input$genf,"%"),paste("MAF < ",input$maff))
    }
    
    y<-cbind(Filter,y)
    y$Individuals<-round(y$Individuals, 0)
    y$Loci<-round(y$Loci,0)
    
  }
  
  y
  
})

#Visualise and download summary table
output$filtersummary <- renderDataTable(filtersummary(), extensions = 'Buttons', 
                                        options = list(dom = 'Bfrtip',
                                                       buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_FilterSummary_Table")), list(extend='excel', filename= paste(input$projectID, "_FilterSummary_Table2")) )),
                                        caption = "Summary of loci and individuals remaining in the dataset after each filtering step",
                                        rownames=FALSE)
#RenderUI for filter summary so it only appears IF you have filtered data
output$filtersumtab <- renderUI({
  if (input$filterdata == TRUE) {
    div(style = 'overflow-x: scroll',
        withSpinner(DT::dataTableOutput("filtersummary")),
        br()
    )
  }
})

###################################
######Download Filtered Data#######
###################################

#########STRUCTURE FORMAT###############
structureconverted_filter<- reactive ({
  
  req(test)
  
  genind<-genind_filt()
  
  tab<-capture.output(genind2structure(genind, pops = TRUE))
  
  tab<-as.data.frame(tab)
  
})

#Download converted structure file
output$downloadstructure_filt <-downloadHandler (
  
  filename = function() {
    paste(input$projectID,"filtereddata_STRUCTURE_formatted.str")
  },
  
  content = function(file) {
    write.table(structureconverted_filter(), file, sep="\t", quote=FALSE, row.names=FALSE, col.names = FALSE)
  }
  
)


################GenAlEx Format####################

genealexconverted_filter<- reactive ({
  
  req(test)
  
  genind<-genind_filt()
  
  tab<-capture.output(genind2genalex(genind))
  
  tab<-as.data.frame(tab)
  
})

#Download converted structure file

output$downloadgenealex_filt <-downloadHandler (
  
  filename = function() {
    paste(input$projectID,"filtereddata_GeneAlex_formatted.str")
  },
  
  content = function(file) {
    write.table(genealexconverted_filter(), file, sep="\t", quote=FALSE, row.names=FALSE, col.names = FALSE)
  }
  
)

################################################
####Create metadata file to match STR output####
################################################


metadata_filt<-reactive ({
  
  req(genind_filt)
  genind<-genind_filt()
  
  metadata<-as.data.frame(genind$strata)
  temp<-as.data.frame(genind$tab)
  metadata$Ind<-rownames(temp)
  metadata$pop<-genind$pop
  metadata
  
})

#Visualize and download metadata file
output$metadata_filt <- renderDataTable(metadata_filt(), extensions = 'Buttons', 
                                   options = list(dom = 'Bfrtip',
                                                  buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_FilteredData_Metadata")), list(extend='excel', filename= paste(input$projectID, "_FilteredData_Metadata")) )),
                                   caption = "Filtered metadata",
                                   server = FALSE,
                                   rownames=FALSE)



#Convert genind to df

filteredfile<- reactive({
  
  req(genind_filt)
  
  gendf<-genind2df(genind_filt())

  gendf
  })

output$filterdf <- renderDataTable(filteredfile(), extensions = 'Buttons', 
                                        options = list(dom = 'Bfrtip',
                                                       buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_FilteredData_df")), list(extend='excel', filename= paste(input$projectID, "_FilteredData_df")) )),
                                        caption = "Filtered Data",
                                   server = FALSE,
                                        rownames=TRUE
)


output$filterdf2 <- renderUI({
  
  if (input$filterdata == TRUE) {
    
    conditionalPanel(
      
      h5(),
    
    div(style = 'overflow-x: scroll',
        
        withSpinner(DT::dataTableOutput("filterdf"))
    ),
    
    br(),
    
    h5("You can download the above table as a csv or Excel file and add the appropriate additions to format
       it in the same manner as a GenAlEx file for future re-import in to the application
       or other programs"),
    
    h5("Alternatively, you can also download the STRUCTURE formatted file of the filtered data,
       bare in STRUCTURE export converts your metadata to a numerical format which you will also
       have to re-import"),
    
    #downloadButton("downloadgenealex_filt", "Download Filtered File (GenAlex)"),
    downloadButton("downloadstructure_filt", "Download Filtered File (STRUCTURE)"),
    
    br(),
    
    div(style = 'overflow-x: scroll',
        
        withSpinner(DT::dataTableOutput("metadata_filt"))
    )
    
    
    )
  }
  
  
})

