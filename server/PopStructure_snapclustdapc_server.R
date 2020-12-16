#######################################
#######################################
######Admixture: Snapclust + DAPC######
#######################################
#######################################

######################
###Intro section######
######################

#Panel appears if no optimum k (no. of pop clusters) has been yet selected or after determination k = 1

output$beforeyoucontinue<- renderUI({
  
  if (input$snapclustk == 1) {
  
  conditionalPanel("input.snapclustk  == 1",
                   
                   fluidRow(
                     
                     box(title = "Before you continue",
                         solidHeader=TRUE,
                         status = "success",
                         width = 6,
                         
                         "If you have not plotted the AIC / BIC curves please do so now. It is very important that you have an idea of the optimal number of population clusters is before continuing with snapclust or DAPC.",
                         
                         br(),hr(),
                         
                         "If you have done so and the optimum k is 1, then please do not continue on this page. There is no strong underlying population structure to be described, and to report such would be misleading."
                         
                     ) #end of box
                   ) #end of fluidrow
 
  ) #end of conditional panel 
  }
})


#######################################
###A priori AIC / BIC Calculation######
#######################################

#Calculate information criteria for a range of values of K clusters

snapclustoptk <- eventReactive(input$snapclustoptkGO,{
  
  req(snapclustoutput)
  
  genind <- genind_filt()
  
  #Create BIC plot
  bic<-snapclust.choose.k(20, genind, IC = BIC)
  bic<-as.data.frame(bic)
  bic$k<-as.numeric(row.names(bic))
  
  bicplot<-ggplot(bic, aes(x=k, y=bic)) +
    geom_line() +
    geom_point(size=5) +
    scale_x_continuous(name="k", breaks = seq(0,20,1) ) +
    theme_bw()+
    theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 17),
          axis.text.x = element_text(size = 10)
    )
  bicplot
  
  
  aic<-snapclust.choose.k(20, genind, IC = AIC)
  aic<-as.data.frame(aic)
  aic$k<-as.numeric(row.names(aic))
  
  aicplot<-ggplot(aic, aes(x=k, y=aic)) +
    geom_line() +
    geom_point(size=5) +
    scale_x_continuous(name="k", breaks = seq(0,20,1) ) +
    theme_bw()+
    theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 17),
          axis.text.x = element_text(size = 10)
          )
  aicplot
  
  
  aicbic<-ggarrange(bicplot, aicplot)
  
  
  aicbic
  
})

#Plot AIC / BIC Values

observe({

output$snapclustoptk <- renderPlot({
  
  req(snapclustoptk)
  
  temp <- snapclustoptk()
  
  temp
}, width = 775, height = 400)

}) #end of observe



#Download AIC / BIC plots
#PNG Download
#output$download_aicbic <- downloadHandler(
#  
#  filename <- function() {
#    paste(input$projectID, 'snapclust_optk_InformationCriteria.png', sep = "_")
#  },
#  content <- function(file) {
#    
#    png(file, height = 400, width = 775)
#    
#    print(snapclustoptk())
#    
#    dev.off()
#  },
#  contentType = "image/png"
#)

#output$DL_aicbic_button<- renderUI({
#  
#  downloadButton("download_aicbic", "Download AIC / BIC Plots (.png)")
#  
#})

#TIFF Download
#output$download_aicbic_tiff <- downloadHandler(
#  
#  filename <- function() {
#    paste(input$projectID, 'snapclust_optk_InformationCriteria.tiff', sep = "_")
#  },
#  content <- function(file) {
    
#    tiff(file, height = 400, width = 775)
#    
#    print(snapclustoptk())
#    
#    dev.off()
#  },
#  contentType = "image/tiff"
#)

#output$DL_aicbic_button_tiff<- renderUI({
#  
#  downloadButton("download_aicbic", "Download AIC / BIC Plots (.tiff)")
#  
#})


##############################
######SNAPCLUST ANALYSIS######
##############################

#Carry out snapclust analysis using the user defined optimum number of clusters (k)



snapclustoutput<- reactive ({
  
  req(snapclustoptk)
  
  genind <- genind_filt()
  
  clusters <- snapclust (genind, k = input$snapclustk, 
                         pop.ini = "kmeans", max.iter = 100, )
  
  clusters
  
})

snapclustassignments <- reactive({
  
  req(snapclustoptk)
  req(snapclustoutput)
  
  clusters <- snapclustoutput()
  genind<-genind_filt()
  
  snapassign<-as.data.frame(clusters$proba)
  temp<-as.data.frame(rownames(clusters$proba))
  colnames(temp)<-"Individual"
  snapassign<-cbind(temp,snapassign)
  snapassign$Pop_Hypothesis<-genind$pop
  snapassign$Assigned_Cluster<-clusters$group
  snapassign
  
  
  
})


snapclustadmix<-reactive({
  
  snapassign<-snapclustassignments()
  
  
  maxassign<-as.data.frame(apply((snapassign[2:(input$snapclustk+1)]), 1, max))
  colnames(maxassign)<-"Maximum_Assignment"
  snapassign<-cbind(snapassign, maxassign)
  snapassign
  
  X<-input$snapclust_admixcutoff / 100
  
  snapadmix<-subset(snapassign, Maximum_Assignment < X )
  
  
  if (length(snapadmix[,1]) == 0) {
    
    admixed<-as.data.frame("No putatively admixed individuals")
    colnames(admixed)<-""
    admixed
  }
  
  else {
    
    snapadmix
    
  }
  
  
})



output$snapclustadmix <- renderDataTable(snapclustadmix(), extensions = 'Buttons', 
                                options = list(dom = 'Bfrtip',
                                               buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_snapclust_putativeadmixture")), list(extend='excel', filename= paste(input$projectID, "_snapclust_putativeadmixture")) )),
                                caption = "Putatively admixed individuals",
                                rownames=FALSE
)


output$snapclustassignments <- renderDataTable(snapclustassignments(), extensions = 'Buttons', 
                                options = list(dom = 'Bfrtip',
                                               buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_snapclust_membershipprobability_assignments")), list(extend='excel', filename= paste(input$projectID, "_snapclust_membershipprobability_assignments")) )),
                                caption = "The maximum likelihood assignment of each individual to a population cluster. Based on proportions, individuals are labelled to belong to a given cluster if the proportion of probable membership is >0.5. If an individual does not have a majority proportion of membership to any cluster then it is defined as being admixed."
                                ,
                                rownames=FALSE
)





snapclustcompoplot<- reactive({
  
  req(snapclustoutput)
  
  clusters<-snapclustoutput()
  genind<-genind_filt()
  
  temp<-as.data.frame(clusters$proba)
  temp$indNames<-rownames(temp)
  temp$pop<-genind$pop
  
  
  x<-tidyr::pivot_longer(temp, 1:input$snapclustk, names_to = "Cluster")
  x<- x %>% arrange(desc(x$pop))
  
  
  
ggplot(x,mapping = aes(x$indNames, y = value, fill = x$Cluster)) +
    geom_bar(position = "stack", stat="identity",width = 1)+
    scale_fill_viridis(discrete = T, name="Cluster", option = input$snapclust_compoplot_colpal) +
    labs(y="Proportion", x = "Individuals")+
    theme_bw()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = input$snapclust_compoplot_legendposition)+
    theme(legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),
          plot.title = element_text(face="bold", size = input$snapclust_compoplot_title),
          strip.text = element_text(size = input$snapclust_compoplot_facet),
          axis.title = element_text(size = input$snapclust_compoplot_axistitle),
          axis.text = element_text(size = input$snapclust_compoplot_axistext),
          legend.title = element_text(size = input$snapclust_compoplot_legend),
          legend.text = element_text(size = input$snapclust_compoplot_legend)
          
          
          )+
    facet_grid(~pop, scales = "free") +
    ggtitle(label = "Snapclust cluster assignment probabilities")
  
  
})

#output$snapclustcompoplot <- renderPlot({
#  
#  req(snapclustcompoplot)
#  
#  snapclustcompoplot()
#  
#})

observe({
  
  output$snapclustcompoplot <- renderPlot({
    
    req(snapclustcompoplot)
    
    snapclustcompoplot()
    
  },   width = input$snapclust_compoplot_X, height = input$snapclust_compoplot_Y)
  
})

##################################
###Download snapclust compoplot###
##################################

#PNG Download
output$download_snapclust_compoplot <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'snapclust_compoplot.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$snapclust_compoplot_X)
    Y<-as.numeric(input$snapclust_compoplot_Y)
    
    png(file, height = Y, width = X)
    
    print(snapclustcompoplot())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_snapclust_compoplot_button<- renderUI({
  
  downloadButton("download_snapclust_compoplot", "Download Plot (.png)")
  
})

#TIFF DOwnload
output$download_snapclust_compoplot_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'snapclust_compoplot.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$snapclust_compoplot_X)
    Y<-as.numeric(input$snapclust_compoplot_Y)
    
    tiff(file, height = Y, width = X)
    
    print(snapclustcompoplot())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_snapclust_compoplot_button_tiff<- renderUI({
  
  downloadButton("download_snapclust_compoplot_tiff", "Download Plot (.tiff)")
  
})


##################################################################
###Snapclust comparisons against original population hypotheses###
##################################################################

snapclustxoriginaltab <- reactive ({
  
  req(snapclustoutput)
  
  clusters<-snapclustoutput()
  genind<-genind_filt()
  
  
  my_table <- addmargins(table(factor(pop(genind)), factor(clusters$group))) %>% 
    as.data.frame() %>% 
    tidyr::spread(Var2, Freq)

  my_table <- my_table %>% rename(Comparison = Var1)
  
  my_table
  
 
})

output$snapclustxoriginaltab <- renderDataTable(snapclustxoriginaltab(), extensions = 'Buttons', 
                                options = list(dom = 'Bfrtip',
                                               buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_snapclust_clustersvspophypothesis_comparison")), list(extend='excel', filename= paste(input$projectID, "_snapclust_clustersvspophypothesis_comparison")) )),
                                caption = "Snapclust maximum likelihood assignments compared to the original population hypothesis",
                                rownames=FALSE
)


output$snapclustxoriginalplot <- renderPlot({
  
  req(snapclustoutput)
  
  clusters<-snapclustoutput()
  genind<-genind_filt()
  
  table.value(table(pop(genind), clusters$group), col.lab = paste("Cluster", 1:input$snapclustk), csize =1.5)

  
})




















##################################
#########DAPC Analysis############
##################################


#Determine the number of discriminate components to use
#       This is basically the number of population hypotheses
nda <- reactive ({
  
  req(snapclustoutput)
  
  clusters <- snapclustoutput()
  
  nda<-length(as.matrix((unique(clusters$group))))
  
  nda 
  
})


DAPCAxes <- reactive({
  
  req(nda)
  
  genind <- genind_filt()
  nda<-nda()
  
  dapctest<- dapc(genind, n.da = nda, n.pca = 100)
  temp<-optim.a.score(dapctest)
  temp
  
})

output$DAPCalpha<-renderPlot({
  
  req(DAPCAxes)
  
  DAPCAxes()
  
  
})



#Carry out DAPC Analysis
DAPCcalc<-reactive({
  
  req(nda)
  req(snapclustoutput)
  req(genind_filt)
  
  genind<-genind_filt()
  clusters<-snapclustoutput()
  nda<-as.numeric(nda())
  
  dapc.df = adegenet::dapc(genind, var.contrib = TRUE, pop=clusters$group, scale = FALSE, n.pca = 12, n.da = nda)
  
})

DAPCassignments <- reactive ({
  
  req(DAPCcalc)
  
  dapc.df <- DAPCcalc()
  genind<-genind_filt()
  clusters<-snapclustoutput()
  
  dapc.results <- as.data.frame(dapc.df$posterior)
  dapc.results$pop = pop(genind)
  dapc.results$indNames = rownames(dapc.results)
  suppressWarnings(dapc.results$DAPCassignment<-(names(dapc.results)[apply(dapc.results, 1, which.max)]))
  dapc.results$Snapclustassignment<-clusters$group
  
  dapc.results
  
  
})

DAPCass<-reactive({
  
  req(DAPCassignments)
  
  dapc.results<-DAPCassignments()
  
  DT::datatable(dapc.results,
                caption= "Posterior probabilites that each individual belongs to the population clusters as defined by the Snapclust algorithm.",
                rownames=FALSE
  )
  
})

output$DAPCass <- DT::renderDataTable({
  

  DAPCass()
  
})

output$DAPCdensityrender<-renderUI({
  
  
  withSpinner(plotOutput("DAPCdensity"))
  
})

output$DAPC_nda2message<-renderPrint({
  
  print("Because the number of discriminate axes used (# optimum clusters - 1) is two, a scatter plot cannot be generated. We recommend visualizing the ordination of your data through one of the other methods offered.")

})


output$DAPCscatterrender<-renderUI({

  nda<-nda()
  
  if (nda == 2) {
  
    "Because the number of discriminate axes used (# optimum clusters - 1) is two, a scatter plot cannot be generated. We recommend visualizing the ordination of your data through one of the other methods offered."
    
  }
  
  if(nda > 2) {
    
    withSpinner(plotOutput("DAPCscatter"))
    
  }
  
})




DAPCdensity <- reactive ({
  
  req(DAPCassignments)
  
  dapc.results <- DAPCassignments()
  dapc.df <- DAPCcalc()
  genind <- genind_filt()
  nda<-nda()
  
    
    viridispalette<-(viridis_pal()(nda))
    
    
    temp<-as.data.frame(dapc.df$ind.coord)
    temp$Cluster<-dapc.df$grp
    
    
    mu<-plyr::ddply(temp, "Cluster", summarize, grp.mean=mean(LD1))
    
    y<-ggplot(data = temp, aes(x=LD1, color= Cluster, fill=Cluster ))+
      geom_density(alpha=0.4) +
      scale_x_continuous(name= "Discriminant Axis 1")+
      scale_y_continuous(name = "Density")+
      
      #geom_vline(data=mu, aes(xintercept=grp.mean, color=Cluster),linetype="dashed",size = 1.5)+
      
      theme_bw() +
      scale_fill_viridis(discrete=TRUE) +
      scale_color_viridis(discrete=TRUE)+
      theme(
        axis.title = element_text(color="black", size=input$DAPC_axistitle),
        axis.text = element_text(color= "black", size = input$DAPC_axistext),
        legend.text = element_text(color= "black", size = input$DAPC_legend),
        legend.title = element_text(color= "black", size = input$DAPC_legend),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.position = input$DAPC_legendposition,
        panel.grid = element_blank()
      )
    y
  
  
  
})

observe({

output$DAPCdensity<-renderPlot({
  
  req(DAPCdensity)
  
  (DAPCdensity())
  
}, width = input$DAPC_X, height = input$DAPC_Y)

})

DAPCscatter <- reactive ({
  
  dapc.results <- DAPCassignments()
  dapc.df <- DAPCcalc()
  genind <- genind_filt()
  nda<-nda()
  
  
  if (nda == 2) {
    
    NULL
    
  }
  
  
  if (nda > 2) {
    
    dapccoords<-as.data.frame(dapc.df$ind.coord)
    
    #Calculate centroids and spiders
    
    dapccoords$assignment<-dapc.results$DAPCassignment
    dapccoords
    
    dapccoords<- dapccoords %>% arrange(desc(assignment))
    
    cent<-aggregate(cbind(dapccoords$LD1,dapccoords$LD2) ~ assignment, data = dapccoords, FUN = mean)
    segs<-merge(dapccoords, setNames(cent, c('assignment', 'oAxis1','oAxis2')), 
                by = 'assignment', sort = FALSE)
    dapccoords$oAxis1 <-segs$oAxis1
    dapccoords$oAxis2 <-segs$oAxis2
    dapccoords
    
    segs
    
    
    x<-ggplot(dapccoords, aes(dapccoords$LD1 , dapccoords$LD2,colour=dapccoords$assignment))+
      geom_point(size=input$DAPC_point, alpha=input$DAPC_transparent) +
      scale_color_viridis(discrete=TRUE)+
      theme_bw()+
      labs(x= "Discriminant Axis 1", y= "Discriminant Axis 2", color="Cluster")+
      theme(
            axis.title = element_text(color="black", size=input$DAPC_axistitle),
            axis.text = element_text(color= "black", size = input$DAPC_axistext),
            legend.text = element_text(color= "black", size = input$DAPC_legend),
            legend.title = element_text(color= "black", size = input$DAPC_legend),
            legend.box.background = element_rect(),
            legend.box.margin = margin(6, 6, 6, 6),
            legend.position = input$DAPC_legendposition,
            panel.grid = element_blank()
            )
      
    if (input$statellipseDAPC == "TRUE") {
      
      x<- x+stat_ellipse(size =1)
    }
    
    if (input$spiderDAPC == "TRUE") {
      x<-x + geom_segment(data=dapccoords,aes(xend = oAxis1, yend = oAxis2))
    }
    
    if (input$showcentroidDAPC == "TRUE") {
      x<- x + geom_point(data = cent, size = as.numeric(input$DAPC_point + 6), aes(x = cent$V1, y= cent$V2, colour = cent$assignment)) 
    }
    
    if (input$showhvlineDAPC == "TRUE") {
      x<- x + 
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0)
      }
    x
    
  }
  
  
})

observe({

output$DAPCscatter<-renderPlot({
  
  DAPCscatter()
  
}, width = input$DAPC_X, height = input$DAPC_Y)

})


##################################
#######Download DAPC Figures######
##################################

###################
###Density Plots###
###################

#PNG Download
output$download_DAPC_density <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'DAPC_densityplot.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$DAPC_X)
    Y<-as.numeric(input$DAPC_Y)
    
    png(file, height = Y, width = X)
    
    print(DAPCdensity())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_DAPC_density_button<- renderUI({
  
  downloadButton("download_DAPC_density", "Download Density Plot (.png)")
  
})

#TIFF Download
output$download_DAPC_density_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'DAPC_densityplot.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$DAPC_X)
    Y<-as.numeric(input$DAPC_Y)
    
    tiff(file, height = Y, width = X)
    
    print(DAPCdensity())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_DAPC_density_button_tiff<- renderUI({
  
  downloadButton("download_DAPC_density_tiff", "Download Density Plot (.tiff)")
  
})



###################
###Scatter Plots###
###################

#PNG Download
output$download_DAPC_scatter <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'DAPC_scatterplot.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$DAPC_X)
    Y<-as.numeric(input$DAPC_Y)
    
    png(file, height = Y, width = X)
    
    print(DAPCscatter())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_DAPC_scatter_button<- renderUI({
  
  downloadButton("download_DAPC_scatter", "Download Scatter Plot (.png)")
  
})

#TIFF Download
output$download_DAPC_scatter_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'DAPC_scatterplot.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$DAPC_X)
    Y<-as.numeric(input$DAPC_Y)
    
    tiff(file, height = Y, width = X)
    
    print(DAPCscatter())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_DAPC_scatter_button_tiff<- renderUI({
  
  downloadButton("download_DAPC_scatter_tiff", "Download Scatter Plot (.tiff)")
  
})

























DAPCSnapcomptab <- reactive ({
  
  req(DAPCassignments)
  dapc.results<-DAPCassignments()
  
  
  my_table <- addmargins(table(factor(dapc.results$DAPCassignment), factor(dapc.results$Snapclustassignment))) %>% 
    as.data.frame() %>% 
    tidyr::spread(Var2, Freq)
  
  my_table <- my_table %>% rename(Comparison = Var1)
  
  DT::datatable(my_table,
                rownames = FALSE,
                caption = "Snapclust maximum likelihood assignments compared to the original population hypotheses",
                options = list(
                  dom = 't'))
  
  
})


DAPCSnapcompplot <- reactive({
  
  req(DAPCassignments)
  
  dapc.results<-DAPCassignments()
  
  table.value(table(dapc.results$Snapclustassignment, dapc.results$DAPCassignment), col.lab = paste("DAPC Cluster", 1:input$snapclustk),row.labels = paste(" Snapclust Cluster", 1:input$snapclustk), csize =1)
  
  
})

output$DAPCSnapcomptab<-DT::renderDataTable({
  
  req(DAPCSnapcomptab)
  
  DAPCSnapcomptab()
  
})

output$DAPCSnapcompplot<-renderPlot({
  
  req(DAPCSnapcompplot)
  
  DAPCSnapcompplot()
  
})



DAPCSnapcompoplot<-reactive({
  
  req(DAPCassignments)
  
  dapc.results<-DAPCassignments()
  nda<-nda()
  
  temp<-as.data.frame(dapc.results)
  
  x<-tidyr::pivot_longer(temp, 1:nda, names_to = "Cluster")
  x<- x %>% arrange(desc(x$pop))
  
  
  
  y<-ggplot(x,mapping = aes(x$indNames, y = value, fill = x$Cluster)) +
    geom_bar(position = "stack", stat="identity",width = 1)+
    scale_fill_viridis(discrete = T, name="Cluster") +
    labs(y="Proportion", x = "Individuals")+
    theme_bw()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom")+
    theme(legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),
          plot.title = element_text(face="bold"))+
    facet_grid(~pop, scales = "free") +
    ggtitle(label = "DAPC posterior probabilities")
  
  y
  
})

output$DAPCSnapcompoplot<-renderPlot({
  
  req(DAPCSnapcompoplot)
  DAPCSnapcompoplot()
  
})