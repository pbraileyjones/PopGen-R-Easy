########################
####Scree Plot##########
########################

#Use Scree plots to determine which principal components are important to consider

#Calculate variance explained by each axis
PCAscree<-eventReactive(input$PCAScreeGO,{
  
  genind<-genind_filt()
  
  #SSR data screeplot
  if (max(genind$loc.n.all) >2) {
    genind.X <- tab(genind, freq = TRUE, NA.method = "mean")
    genind.pca <- dudi.pca(genind.X, scale = TRUE, scannf = FALSE, nf = 50)
    
    temp<-as.data.frame(genind.pca$eig)
    colnames(temp)<-c("var")
    temp$axes<-rownames(temp)
    temp$axes<-as.numeric(temp$axes)
    
  }
  
  #SNP data screeplot
  if (max(genind$loc.n.all)  == 2) {
    genlight<-gi2gl(genind)
    genlight.pca = glPca(genlight, nf = 50)
    temp<-as.data.frame(genlight.pca$eig*100/sum(genlight.pca$eig))
    colnames(temp)<-c("var")
    temp$axes<-rownames(temp)
    temp$axes<-as.numeric(temp$axes)

  }
  
  temp
  
}) 


#Plot Scree Plot
PCAscree2<-reactive({
  
  temp<-PCAscree()
  
  ggplot(temp[1:50,], aes(x = axes, y = var ))+
    geom_bar(stat = "identity")+
    theme_bw()+
    xlab("PCA Axis")+
    ylab("Variance Explained (%)")+
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 15))
  
})




observe(
output$PCAscree2<-renderPlot({
  req(PCAscree2)
  
  PCAscree2()

  
  
}, width = 1000, height = 400)
)


PCAscree3<-reactive({
  
  temp<-PCAscree()
  
  colnames(temp)<-c("Variance explained", "Principal Component")
  
  temp<-cbind(temp[2], temp[1])
  
  temp
  
})

output$PCAScree_tab <- renderDataTable(PCAscree3(), extensions = 'Buttons', 
                              options = list(dom = 'Bfrtip',
                                             buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_Pairwise_",input$gendist_other,"_Comparisons")), list(extend='excel', filename= paste(input$projectID,"_Pairwise",input$gendist_other,"Comparisons")) )),
                              caption = paste("Variance accounted for by each principal component"),
                              rownames=FALSE
)

###########################
###Download Scree Plot#####
###########################

#PNG Download
output$download_PCA_scree <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCA_scree.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(1000)
    Y<-as.numeric(400)
    
    png(file, height = Y, width = X)
    
    print(PCAscree2())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_PCA_scree_button<- renderUI({
  
  downloadButton("download_PCA_scree", "Download Plot (.png)")
  
})

#tiff Download
output$download_PCA_scree_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCA_scree.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(1000)
    Y<-as.numeric(400)
    
    tiff(file, height = Y, width = X)
    
    print(PCAscree2())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_PCA_scree_button_tiff<- renderUI({
  
  downloadButton("download_PCA_scree_tiff", "Download Plot (.tiff)")
  
})


##########################################
######Principal Component Analysis########
##########################################

#Carry out PCA

PCAcalc<-eventReactive(input$PCAGO,{
  
  req(genind_filt)
  genind<-genind_filt()
  
  ##############
  ###SSR Data###
  ##############
  
  #This variant is not run for SNP data, as the dudi.pca script cannot handle NAs, which are more plentiful in SNP data
  #Filling in NAs with the mean will affect analysis
  if (max(genind$loc.n.all) > 2) {
    
    genind.X <- tab(genind, freq = TRUE, NA.method = "mean")
    genind.pca <- dudi.pca(genind.X, scale = TRUE, scannf = FALSE, nf = 20)
    genind.pca$scores<- genind.pca$li
    genind.pca
    
  }
  ##############
  ###SNP Data###
  ##############
  
  else if (max(genind$loc.n.all) == 2) {
    
    genlight<-gi2gl(genind)
    genlight.pca = glPca(genlight, nf = 20)
    genlight.pca
    
  }
  
})

#Determine variance explained by Axis 1
PCAAxis1<- reactive ({
  
  req(PCAcalc)
  PCAcalc<-PCAcalc()
  
  #calculate explained variance
  variance = PCAcalc$eig*100/sum(PCAcalc$eig)
  cumvar = cumsum(variance)
  varexp= data.frame(eigenvalue = PCAcalc$eig, variance = variance, cumvariance = cumvar)
  Axis1var=round(varexp[input$PCAx,2])
  
})

#Determine variance explained by Axis 2
PCAAxis2<- reactive ({
  
  req(PCAcalc)
  PCAcalc<-PCAcalc()
  
  #calculate explained variance
  variance = PCAcalc$eig*100/sum(PCAcalc$eig)
  cumvar = cumsum(variance)
  varexp= data.frame(eigenvalue = PCAcalc$eig, variance = variance, cumvariance = cumvar)
  Axis2var=round(varexp[input$PCAy,2])
  
})

#Extract PCA scores dataframe
PCAscores<- reactive ({
  
  req(PCAcalc)
  PCAcalc<-PCAcalc()
  
  #modify for ploting
  scores<-as.data.frame(PCAcalc$scores) #Create dataframe of individual points
  scores
  
})

#Plot PCA
PCAplot<- reactive ({
  
  req(PCAcalc)
  
  genind<-genind_filt()
  scores<-PCAscores()
  Axis1var<-PCAAxis1()
  Axis2var<-PCAAxis2()
  
  #Create dataframes to plot
  Axis1<-as.data.frame(scores[input$PCAx])
  colnames(Axis1)<-"Axis1"
  Axis2<-as.data.frame(scores[input$PCAy])
  colnames(Axis2)<-"Axis2"
  scores$Axis1<-Axis1$Axis1
  scores$Axis2<-Axis2$Axis2
  
  ####################################################
  ######Define Colors and Shapes based on strata######
  ####################################################
  
  #Define point colours by strata
  
  if (input$stratdef == "One") {  #If there is no strata defined colours will be exclusively by points
    
    scores$pop<-genind$pop
    colorname<-"Population"
    
  }
  
  else {  #If strata were previously defined you can now choose how to color the points
    
    if (input$colorPCAstrata == "nostrat") {  #The original option of coloring without any strata definition is still the default
      
      scores$pop <- genind$pop
      colorname<-"Population"
      
    }
    
    else {  #Additionally you can color by the strata of your choosing
      
      pop<-as.matrix(genind$strata[as.numeric(input$colorPCAstrata)])
      scores$pop<-pop
      
      
      colorname<-"Population"
      
    }
    
  }
  
  
  #Define point shape by strata
  
  if (input$stratdef == "One") {  #If there is no strata defined colours will be exclusively by points
    
    scores$shape<-NULL
    
  }
  
  else {  #If strata were previously defined you can now choose how to color the points
    
    if (input$shapePCAstrata == "noshape") {
      
      scores$shape <- NULL
      
    }
    
    else if (input$shapePCAstrata == "nostrat") {  #You can shape points by each original pop
      
      scores$shape <- genind$pop
      
    }
    
    else {  #Additionally you can color by the strata of your choosing
      
      shape<-as.matrix(genind$strata[as.numeric(input$shapePCAstrata)])
      scores$shape<-shape
      
    }
    
  }
  
  ##################
  ####PLOT GRAPH####
  ##################
  
  
  if (input$spiderPCA == "FALSE") {
  
  x<-ggplot(scores, aes(x = Axis1,y =Axis2,colour=scores$pop))+
    geom_point(size=input$PCA_pointsize, alpha=input$PCA_alpha, aes(shape = scores$shape ))+
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    theme_bw()+
    labs(title="PCA Individuals", x=paste("PCA Axis ",input$PCAx," (",Axis1var, " %)", sep= ""), y=paste("PCA Axis ",input$PCAy," (",Axis2var, " %)", sep= ""), color=paste(colorname) ) +
    
    scale_color_viridis(discrete = TRUE, option = input$PCA_colpal)+
    
    theme(plot.title = element_text(color="black", size=input$PCA_title, face="bold"),
          axis.title.x = element_text(color="black", size=input$PCA_axistitle),
          axis.title.y = element_text(color="black", size=input$PCA_axistitle),
          axis.text = element_text(size = input$PCA_axistext),
          legend.title = element_text(size = input$PCA_legend),
          legend.text = element_text(size = input$PCA_legend),
          legend.position = input$PCA_legendposition,
          panel.grid = element_blank())+
    scale_color_viridis(discrete = TRUE, option = input$PCA_colpal)
  
  }
    
    else if (input$spiderPCA == "TRUE") {
      
      scores<-scores[order(scores$pop),]
      cent<-aggregate(cbind(scores$Axis1,scores$Axis2) ~ pop, data = scores, FUN = mean)
      segs<-merge(scores, setNames(cent, c('pop', 'oAxis1','oAxis2')), 
                  by = 'pop', sort = FALSE)
      
      scores$oAxis1 <-segs$oAxis1
      scores$oAxis2 <-segs$oAxis2
      
      
      x<-ggplot(scores, aes(x = Axis1,y =Axis2,colour=scores$pop))+
        geom_point(size=input$PCA_pointsize, alpha=input$PCA_alpha, aes(shape = scores$shape ))+
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        theme_bw()+
        labs(title="PCA Individuals", x=paste("PCA Axis ",input$PCAx," (",Axis1var, " %)", sep= ""), y=paste("PCA Axis ",input$PCAy," (",Axis2var, " %)", sep= ""), color=paste(colorname) ) +
        
        theme(plot.title = element_text(color="black", size=input$PCA_title, face="bold"),
              axis.title.x = element_text(color="black", size=input$PCA_axistitle),
              axis.title.y = element_text(color="black", size=input$PCA_axistitle),
              axis.text = element_text(size = input$PCA_axistext),
              legend.title = element_text(size = input$PCA_legend),
              legend.text = element_text(size = input$PCA_legend),
              legend.position = input$PCA_legendposition,
              panel.grid = element_blank())+
        scale_color_viridis(discrete = TRUE, option = input$PCA_colpal)
      
      x<-x + geom_segment(data=scores,aes(xend = oAxis1, yend = oAxis2))
    }
    
    
    if (input$statellipsePCA == "TRUE") {
      x<- x + stat_ellipse(size = 1)
    }
  
  if (input$showcentroidPCA == "TRUE") {
    cent<-aggregate(cbind(scores$Axis1,scores$Axis2) ~ pop, data = scores, FUN = mean)
    
    x<- x + geom_point(data = cent, size = input$PCA_pointsize + 5, aes(x = cent$V1, y= cent$V2, colour = cent$pop)) 
  } 
  
  x
  
})

#Render Plot
observe({

output$PCAplot<-renderPlot({
  
  req(PCAplot) 
  
  PCAplot()
  
}, width = input$PCA_X, height = input$PCA_Y)

})

#Plot PCA Centroids only
pcacentroidplot<-reactive ({
  
  req(PCAcalc)
  
  genind<-genind_filt()
  scores<-PCAscores()
  Axis1var<-PCAAxis1()
  Axis2var<-PCAAxis2()
  
  #Create dataframes to plot
  Axis1<-as.data.frame(scores[input$PCAx])
  colnames(Axis1)<-"Axis1"
  Axis2<-as.data.frame(scores[input$PCAy])
  colnames(Axis2)<-"Axis2"
  scores$Axis1<-Axis1$Axis1
  scores$Axis2<-Axis2$Axis2
  
  #Define point colours by strata
  
  if (input$stratdef == "One") {  #If there is no strata defined colours will be exclusively by points
    
    scores$pop<-genind$pop
    colorname<-"Population"
    
  }
  
  else {  #If strata were previously defined you can now choose how to color the points
    
    if (input$colorPCAstrata == "nostrat") {  #The original option of coloring without any strata definition is still the default
      
      scores$pop <- genind$pop
      colorname<-"Population"
      
    }
    
    else {  #Additionally you can color by the strata of your choosing
      
      pop<-as.matrix(genind$strata[as.numeric(input$colorPCAstrata)])
      scores$pop<-pop
      
      
      colorname<-"Population"
      
    }
    
  }
  
  scores<- scores %>% arrange(desc(pop))
  
  
  #Calculate centroids and spiders
  
  cent<-aggregate(cbind(scores$Axis1,scores$Axis2) ~ pop, data = scores, FUN = mean)
  
  #Plot graph
  
  x<-ggplot(cent, aes(x = V1,y =V2,colour=cent$pop))+
    geom_point(data = cent, size = input$PCA_pointsize + 5) +
    xlim((min(scores$Axis1)),(max(scores$Axis1)))+
    ylim((min(scores$Axis2)),(max(scores$Axis2)))+
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    theme_bw()+
    labs(title="PCA Centroids", x=paste("PCA Axis",input$PCAx, " (",Axis1var, " %)", sep= ""), y=paste("PCA Axis",input$PCAy," (",Axis2var, " %)", sep= ""), color=paste(colorname) ) +
    theme(plot.title = element_text(color="black", size=input$PCA_title, face="bold"),
          axis.title.x = element_text(color="black", size=input$PCA_axistitle),
          axis.title.y = element_text(color="black", size=input$PCA_axistitle),
          axis.text = element_text(size = input$PCA_axistext),
          legend.title = element_text(size = input$PCA_legend),
          legend.text = element_text(size = input$PCA_legend),
          legend.position = input$PCA_legendposition,
          panel.grid = element_blank())+
    scale_color_viridis(discrete = TRUE, option = input$PCA_colpal)

    x
  
})

#Render centroid only plot
observe({

output$PCAcentroidplot<-renderPlot({
  
  pcacentroidplot()
  
}, width = input$PCA_X, height = input$PCA_Y)

})

#Show centroid only plot only if the user requests it
output$PCAcentroid<-renderUI({
  
  if (input$plotcentroidonlyPCA == TRUE) {
    
    div(style = 'overflow-x: scroll',
    plotOutput("PCAcentroidplot")
    )
    
  }
  
  
})

##################################
######DOWNLOAD PCA FIGURES########
##################################

#########PCA#############

#PNG
output$download_PCA <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCA.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$PCA_X)
    Y<-as.numeric(input$PCA_Y)
    
    png(file, height = Y, width = X)
    
    print(PCAplot())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_PCA_button<- renderUI({
  
  downloadButton("download_PCA", "Download PCA Plot (.png)")
  
})

#TIFF
output$download_PCA_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCA.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$PCA_X)
    Y<-as.numeric(input$PCA_Y)
    
    tiff(file, height = Y, width = X)
    
    print(PCAplot())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_PCA_button_tiff<- renderUI({
  
  downloadButton("download_PCA_tiff", "Download PCA Plot (.tiff)")
  
})

#########PCA Centroids Only#############

#PNG
output$download_PCA_centroids <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCA_centroids.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$PCA_X)
    Y<-as.numeric(input$PCA_Y)
    
    png(file, height = Y, width = X)
    
    print(pcacentroidplot())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_PCA_centroids_button<- renderUI({
  
  downloadButton("download_PCA_centroids", "Download PCA Centroids Plot (.png)")
  
})

#TIFF
output$download_PCA_centroids_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCA_centroids.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$PCA_X)
    Y<-as.numeric(input$PCA_Y)
    
    tiff(file, height = Y, width = X)
    
    print(pcacentroidplot())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_PCA_centroids_button_tiff<- renderUI({
  
  downloadButton("download_PCA_centroids_tiff", "Download PCA Centroids Plot (.tiff)")
  
})


