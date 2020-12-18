
#AMOVA
source(file.path("server", "PopulationStructure_AMOVA_server.R"),  local = TRUE)$value

#GENETIC DISTANCES

source(file.path("server", "PopulationStructure_Pairwisecomp_server.R"),  local = TRUE)$value

#DAPC Analysis
source(file.path("server", "PopStructure_snapclustdapc_server.R"),  local = TRUE)$value


###################################
####Convert genind to structure####
###################################

#Convert genind object

structureconverted<- reactive ({
  
  req(test)
  
  genind<-genind_filt()
  
  tab<-capture.output(genind2structure(genind, pops = TRUE))
  
  tab<-as.data.frame(tab)
  
})

#Download converted structure file

output$downloadstructure <-downloadHandler (
  
  filename = function() {
    paste(input$projectID,"STRUCTURE_formatted.str")
  },
  
  content = function(file) {
    write.table(structureconverted(), file, sep="\t", quote=FALSE, row.names=FALSE, col.names = FALSE)
  }

)


###################################
#####POPHELPER Import functions####
###################################

#Import files from STRUCTURE analysis
mycsvs<-reactive({
  
  pophelper::readQ(input$csvs$datapath, filetype = "structure")
  
})

#Define what range of K values you carried out STRUCTURE analysis for
output$optclustslider <- renderUI ({
  
  sliderInput("optclust", "Assign optimum population clusters (k) interpreted from the evanno-method graphs",
              min = min(input$structuretestk), max = max(input$structuretestk), value = min(input$structuretestk), step = 1)
  
  
})

#Carry out evanno-structure method to find the optimal number of clusters
evannostr <- reactive ({

  slist <- mycsvs()
  
  #Tabulate Q files
  # basic usage
  tr1 <- pophelper::tabulateQ(qlist=slist)
  
  #Summarize Q
  # basic usage
  sr1 <- pophelper::summariseQ(tr1)
  
  p <- pophelper::evannoMethodStructure(data=sr1,exportplot=F,returnplot=T,returndata=F,basesize=12,linesize=0.7, xaxisbreaks = seq(0,10,1))
  p
  
})

#Merge runs
slist2 <- reactive ({
  
  req(mycsvs)
  req(evannostr)
  
  slist <- mycsvs()
  
  #Align colours assigned to K over multiple runs
  slist1 <- pophelper::alignK(slist)
  #Merge runs of the same k by calculating mean
  slist2<- pophelper::mergeQ(slist1)
  
})

#Create evanno-structure plots
output$evannostruc<- renderPlot({
  
  req(evannostr)
  
  temp<-evannostr()
  
  p<-ggarrange(temp)
  
  p
  
})



###################################
#####VISUALIZE OPTIMAL CLUSTERS####
###################################

#Visualise clusters by individual
structureoptimal<-reactive({
  
  req(slist2)
  
  genind<-genind_filt()
  slist2<-slist2()
  
  temp<-as.data.frame(slist2[input$optclust])
  temp$indNames<-rownames(temp)
  temp$pop<-genind$pop
  
  temp
  
  x<-tidyr::pivot_longer(temp, 1:input$optclust, names_to = "Cluster")
  x<- x %>% arrange(desc(x$pop))
  
  x
  
  
  y<-ggplot(x,mapping = aes(x$indNames, y = value, fill = x$Cluster)) +
    geom_bar(position = "stack", stat="identity",width = 1)+
    scale_fill_viridis(discrete = T, name="Cluster") +
    labs(y="Proportion", x = "Individuals")+
    theme_bw()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank())+
    theme(legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),
          plot.title = element_text(face="bold", size = input$STRUCTURE_optk_title),
          axis.title = element_text(size = input$STRUCTURE_optk_axistitle),
          strip.text = element_text(size = input$STRUCTURE_optk_facet),
          axis.text = element_text(size = input$STRUCTURE_optk_axistext),
          legend.text = element_text(size = input$STRUCTURE_optk_legend),
          legend.title = element_text(size = input$STRUCTURE_optk_legend))+
    theme(legend.position = input$STRUCTURE_optk_legendposition)+
    facet_grid(~pop, scales = "free") +
    ggtitle(label = "STRUCTURE cluster assignment probabilities")
  
  y
  
  
})

observe({

output$structureoptimal <- renderPlot({
  
  req(structureoptimal)
  
  temp<-structureoptimal()
  
  temp
  
}, width = input$STRUCTURE_optk_X, height = input$STRUCTURE_optk_Y)

})


#Visualize clusters by population hypothese
structureoptimalmeans<-reactive({
  
  req(slist2)
  
  genind<-genind_filt()
  slist2<-slist2()
  
  temp<-as.data.frame(slist2[input$optclust])
  temp$indNames<-rownames(temp)
  temp$pop<-genind$pop
  
  temp
  
  x<-tidyr::pivot_longer(temp, 1:input$optclust, names_to = "Cluster")
  x<- x %>% arrange(desc(x$pop))
  

  x$popxcluster<- with(x, interaction(pop, Cluster, sep = ":"))

  
  agg<-aggregate(x$value, by=list(x$popxcluster), FUN=mean)
  agg
  agg<-agg %>% tidyr::separate(Group.1, c("Population", "Cluster"), sep=":")
  agg
  
  structure<-ggplot(agg, aes(fill=Cluster, y=x, x=Population)) + 
    scale_fill_viridis(discrete = TRUE) +
    geom_bar(position="stack", stat="identity") +
    ggtitle(label = "Mean STRUCTURE cluster assignment probabilities") +
    ylab("Proportion")+
    xlab("Population") +
    theme_classic()  + 
    theme(legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),
          plot.title = element_text(face="bold", size = input$STRUCTURE_optk_title),
          axis.title = element_text(size = input$STRUCTURE_optk_axistitle),
          strip.text = element_text(size = input$STRUCTURE_optk_facet),
          axis.text = element_text(size = input$STRUCTURE_optk_axistext),
          legend.text = element_text(size = input$STRUCTURE_optk_legend),
          legend.title = element_text(size = input$STRUCTURE_optk_legend))+
    theme(legend.position = input$STRUCTURE_optk_legendposition)
  structure
  
})

observe({

output$structureoptimalmeans<-renderPlot({
  
  req(structureoptimalmeans)
  
  temp<-structureoptimalmeans()
  
  temp
  
}, width = input$STRUCTURE_optk_X, height = input$STRUCTURE_optk_Y)

})

###################################
#####VISUALIZE A RANGE CLUSTERS####
###################################

#Create UI of a slider with as many k options as in your data
output$strucvisrangeslider <- renderUI ({
  
  req(mycsvs)
  
  sliderInput("structurevisrange", label = h5("Choose range of k to visualise"), min(input$structuretestk), 
              max(input$structuretestk), value = c(min(input$structuretestk),max(input$structuretestk)), step = 1)
  
})

#Visualize number of clusters for range of k by individual
struccompoplot <- eventReactive (input$badscienceGO,{
  
  req(slist2)
  slist2<-slist2()
  genind<-genind_filt()
  
  mi<-min(input$structurevisrange)
  ma<-max(input$structurevisrange)
  
  ##Create pop file for further plotting
  pop <- NULL;
  pop$Population<-genind$pop
  pop<-as.data.frame(pop)
  
  
  
  #Transform data for plotting
  sublist<-as.data.frame(data.table::rbindlist(slist2[mi:ma], fill= TRUE))
  sublist
  k<-apply(sublist, 1, length)
  temp<-apply(sublist, 1, function(x) sum(is.na(x)))
  sublist$k<-(k-temp)
  temp<-xfun::numbers_to_words(sublist$k)
  sublist$k<-temp
  sublist$Population = pop$Population
  sublist$Individual <- unique(rownames(genind$tab))
  
  
  q<-NULL;
  for(i in 1:ma) { 
    temp<-numbers2words(i)
    nam <- paste("Cluster",temp, sep = " ")
    q<-as.matrix(cbind(q,nam))
  }
  q
  
  sublist
  
  colnames(sublist)<-c(q, "k", "Population", "IndID")
  sublist<-select(sublist,starts_with("unk"))
  
  
  tidy<-tidyr::pivot_longer(sublist,cols= dplyr::starts_with("Cluster"), names_to = c("Cluster"))
  
  tidy$k2<-"k = "
  tidy<-tidy %>% 
    dplyr::mutate(
      k = interaction(k2, k, sep = "")
    )
  neworder <- c(q)
  tidy <- plyr::arrange(transform(tidy,
                                  Cluster=factor(Cluster,levels=neworder)),Cluster)
  
  q<-NULL;
  for(i in mi:ma) { 
    temp<-numbers2words(i)
    nam <- paste("k =", temp, sep = " ")
    q<-as.matrix(cbind(q,nam))
  }
  
  neworder <-c(q)
  tidy <- plyr::arrange(transform(tidy,
                                  k=factor(k,levels=neworder)),k)
  
  tidy<-na.omit(tidy)
  
  tidy
  
  
  structure<-ggplot(tidy, aes(fill=Cluster, y=value, x=IndID)) + 
    scale_fill_viridis(discrete = TRUE) +
    geom_bar(position="stack", stat="identity", width = 1) +
    ggtitle("STRUCTURE cluster assignment probabilities") +
    ylab("Proportion")+
    xlab("Individuals") +
    theme_classic() +
    facet_grid(facets=tidy$k ~ tidy$Population,scales = "free")+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank())+
    theme(legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),
          plot.title = element_text(face="bold", size = input$STRUCTURE_range_title),
          axis.title = element_text(size = input$STRUCTURE_range_axistitle),
          strip.text = element_text(size = input$STRUCTURE_range_facet),
          axis.text.y = element_text(size = input$STRUCTURE_range_axistext),
          legend.text = element_text(size = input$STRUCTURE_range_legend),
          legend.title = element_text(size = input$STRUCTURE_range_legend))+
    theme(legend.position = input$STRUCTURE_range_legendposition)
  structure
  
  
})

observe({

output$struccompoplot <- renderPlot({
  
  req(struccompoplot)
  
  temp<-struccompoplot()
  
  temp
  
}, width = input$STRUCTURE_range_X, height = input$STRUCTURE_range_Y)

})

#Visualize number of clusters for range of k by population hypothesis
struclistmeanpop <- eventReactive (input$badscienceGO,{
  
  req(slist2)
  slist2<-slist2()
  genind<-genind_filt()
  
  
  mi<-min(input$structurevisrange)
  ma<-max(input$structurevisrange)

  ##########Plot mean proportion of each population belonging to a given cluster at k= i ####################
  
  #Create pop file for further plotting
  pop <- NULL;
  pop$Population<-genind$pop
  pop<-as.data.frame(pop)
  
  
  
  #Transform data for plotting
  sublist<-as.data.frame(data.table::rbindlist(slist2[mi:ma], fill= TRUE))
  k<-apply(sublist, 1, length)
  temp<-apply(sublist, 1, function(x) sum(is.na(x)))
  sublist$k<-(k-temp)
  temp<-xfun::numbers_to_words(sublist$k)
  sublist$k<-temp
  sublist$Population = pop$Population
  
  q<-NULL;
  for(i in 1:ma) { 
    temp<-numbers2words(i)
    nam <- paste("Cluster",temp, sep = " ")
    q<-as.matrix(cbind(q,nam))
  }
  
  
  colnames(sublist)<-c(q, "k", "Population")
  #sublist<-select(sublist,starts_with("unk"))
  
  
  tidy<-tidyr::pivot_longer(sublist,cols= dplyr::starts_with("Cluster"), names_to = c("Cluster"))
  tidy<-tidy %>% 
    dplyr::mutate(
      PopClust = interaction(Cluster, Population,k, sep = ":")
    )
  agg<-aggregate(tidy$value, by=list(tidy$PopClust), FUN=mean)
  tidy<-agg %>% tidyr::separate(Group.1, c("Cluster", "Population","k"), sep=":")
  tidy$k2<-"k = "
  tidy<-tidy %>% 
    dplyr::mutate(
      k = interaction(k2, k, sep = "")
    )
  neworder <- c(q)
  tidy <- plyr::arrange(transform(tidy,
                                  Cluster=factor(Cluster,levels=neworder)),Cluster)
  
  q<-NULL;
  for(i in mi:ma) { 
    temp<-numbers2words(i)
    nam <- paste("k =", temp, sep = " ")
    q<-as.matrix(cbind(q,nam))
  }
  
  neworder <-c(q)
  tidy <- plyr::arrange(transform(tidy,
                                  k=factor(k,levels=neworder)),k)
  
  tidy<-na.omit(tidy)
  
  
  structure<-ggplot(tidy, aes(fill=Cluster, y=x, x=Population)) + 
    scale_fill_viridis(discrete = TRUE) +
    geom_bar(position="stack", stat="identity") +
    ggtitle("Structure cluster assignments") +
    ylab("Proportion (%)")+
    xlab("Population") +
    theme_classic() +
    facet_wrap(facets=tidy$k, strip.position = "right", nrow=as.integer(length(unique(tidy$k))/2))+
    theme(
      axis.text.x=element_text(angle = 90),
      axis.ticks.x=element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom")+
  theme(legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),
        plot.title = element_text(face="bold", size = input$STRUCTURE_range_title),
        axis.title = element_text(size = input$STRUCTURE_range_axistitle),
        strip.text = element_text(size = input$STRUCTURE_range_facet),
        axis.text.y = element_text(size = input$STRUCTURE_range_axistext),
        legend.text = element_text(size = input$STRUCTURE_range_legend),
        legend.title = element_text(size = input$STRUCTURE_range_legend))+
    theme(legend.position = input$STRUCTURE_range_legendposition)
  
  structure
  
})

observe({

output$struclistmeanpop <- renderPlot({
  
  req(struclistmeanpop)
  
  temp<-struclistmeanpop()
  
  temp
  
}, width = input$STRUCTURE_range_X, height = input$STRUCTURE_range_Y)

})


###########################################
#####STRUCTURE MEMBERSHIP PROBABILITIES####
###########################################


STRUCTUREassignments<-reactive({
  
  req(slist2)
  
  genind<-genind_filt()
  slist2<-slist2()
  
  temp<-as.data.frame(slist2[input$optclust])
  temp$indNames<-as.matrix(unique(rownames(genind$tab)))
  temp$pop<-genind$pop
  temp
  
})


output$STRUCTUREassignments <- renderDataTable(STRUCTUREassignments(), extensions = 'Buttons', 
                                         options = list(dom = 'Bfrtip',
                                                        buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_STRUCTURE_assignments")), list(extend='excel', filename= paste(input$projectID, "_snapclust_STRUCTURE_assignments")) )),
                                         caption = "Probabilities of membership to each population cluster assigned by STRUCTURE analysis",
                                         rownames=TRUE)

##################################
#####Download Structure plots#####
##################################

###########EVANNO-STRUCTURE#################

#PNG
output$download_evannostructure <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'STRUCTURE_evannostructure.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(600)
    Y<-as.numeric(600)
    
    png(file, height = Y, width = X)
    
    print(evannostr())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_evannostructure_button<- renderUI({
  
  downloadButton("download_evannostructure", "Download Evanno-Structure Plot (.png)")
  
})

#TIFF
output$download_evannostructure_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'STRUCTURE_evannostructure.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(600)
    Y<-as.numeric(600)
    
    tiff(file, height = Y, width = X)
    
    print(evannostr())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_evannostructure_button_tiff<- renderUI({
  
  downloadButton("download_evannostructure", "Download Evanno-Structure Plot (.tiff)")
  
})



###########Opt-K Compoplot INDIVIDUAL#################

#PNG
output$download_STRUCTURE_optk_ind <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'STRUCTURE_optk_compoplot_ind.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$STRUCTURE_optk_X)
    Y<-as.numeric(input$STRUCTURE_optk_Y)
    
    png(file, height = Y, width = X)
    
    print(structureoptimal())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_STRUCTURE_optk_ind_button<- renderUI({
  
  downloadButton("download_STRUCTURE_optk_ind", "Download Opt-K Compoplots by Ind (.png)")
  
})
#TIFF

output$download_STRUCTURE_optk_ind_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'STRUCTURE_STRUCTURE_optk_compoplot_ind.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$STRUCTURE_optk_X)
    Y<-as.numeric(input$STRUCTURE_optk_Y)
    
    tiff(file, height = Y, width = X)
    
    print(structureoptimal())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_STRUCTURE_optk_ind_button_tiff<- renderUI({
  
  downloadButton("download_STRUCTURE_optk_ind_tiff", "Download Opt-K Compoplots by Ind (.tiff)")
  
})





###########Opt-K Compoplot POPULATION#################

#PNG
output$download_STRUCTURE_optk_pop <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'STRUCTURE_optk_compoplot_pop.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$STRUCTURE_STRUCTURE_optk_X)
    Y<-as.numeric(input$STRUCTURE_optk_Y)
    
    png(file, height = Y, width = X)
    
    print(structureoptimalmeans())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_STRUCTURE_optk_pop_button<- renderUI({
  
  downloadButton("download_STRUCTURE_optk_pop", "Download Opt-K Compoplots by pop (.png)")
  
})
#TIFF

output$download_STRUCTURE_optk_pop_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'STRUCTURE_STRUCTURE_optk_compoplot_pop.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$STRUCTURE_optk_X)
    Y<-as.numeric(input$STRUCTURE_optk_Y)
    
    tiff(file, height = Y, width = X)
    
    print(structureoptimalmeans())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_STRUCTURE_optk_pop_button_tiff<- renderUI({
  
  downloadButton("download_STRUCTURE_optk_pop_tiff", "Download Opt-K Compoplots by pop (.tiff)")
  
})




