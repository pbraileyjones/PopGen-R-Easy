
#PCoA customization options

output$sliderPCoA<-renderUI ({
  
  req(test)
  
  sliderInput("PCoAaxes", "Number of PCoA Axes", min = 2, max= 20, value = 2, step = 1)
  
})

output$sliderxPCoA<-renderUI ({
  
  req(test)
  
  sliderInput("PCoAx", "x-axis", min = 1, max= input$PCoAaxes, value = 1, step = 1)
  
})

output$slideryPCoA<-renderUI ({
  
  req(test)
  
  sliderInput("PCoAy", "y-axis", min = 2, max= input$PCoAaxes, value = 2, step = 1)
  
})



########################
####Scree Plot##########
########################

#Use Scree plots to determine which principal components are important to consider

#Calculate variance explained by each axis
PCoAscree<-eventReactive(input$PCoAScreeGO,{
  
  req(test)
  
  genind<-genind_filt()
  
  ################
  #SSR Scree Plot#
  ################
  
  if (max(genind$loc.n.all) > 2) {
    X<-tab(genind, freq = TRUE, NA.method="mean")
    pco<-ade4::dudi.pco(dist(X), scannf=FALSE, nf=20)
    temp <- as.data.frame(pco$eig*100/sum(pco$eig))
    colnames(temp)<-"var"
    temp$axes<-rownames(temp)
    temp$axes<-as.numeric(temp$axes)
    
  }
  ################
  #SNP Scree Plot#
  ################
  
  else if(max(genind$loc.n.all) == 2) {
  
  genlight<-gi2gl(genind)
  pcoagl =gl.pcoa(genlight)
  eig=as.data.frame(pcoagl$scores)
  
  #calculate explained variance
  temp = as.data.frame(pcoagl$eig*100/sum(pcoagl$eig))
  colnames(temp)<-"var"
  temp$axes<-rownames(temp)
  temp$axes <- as.numeric(temp$axes)
  
  }
  
  temp
  
  
  
})


#Plot Scree Plot
PCoAscree2<-reactive({
  
  temp<-PCoAscree()
  
  ggplot(temp[1:20,], aes(x = axes, y = var ))+
    geom_bar(stat = "identity")+
    theme_bw()+
    xlab("PCoA Axis")+
    ylab("Variance Explained (%)")
  
  
})

#Render scree plot
observe(
  output$PCoAscree2<-renderPlot({
    req(PCoAscree2)
    
    PCoAscree2()
    
    
    
  }, width = 1000, height = 400)
)

#Create table of variances
PCoAscree3<-reactive({
  
  temp<-PCoAscree()
  
  colnames(temp)<-c("Variance explained", "Principal Co-ordinate")
  
  temp<-cbind(temp[2], temp[1])
  
  temp
  
})

#Render table of variances
output$PCoAscree_tab <- renderDataTable(PCoAscree3(), extensions = 'Buttons', 
                                       options = list(dom = 'Bfrtip',
                                                      buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_Pairwise_",input$gendist_other,"_Comparisons")), list(extend='excel', filename= paste(input$projectID,"_Pairwise",input$gendist_other,"Comparisons")) )),
                                       caption = paste("Variance accounted for by each principal co-ordinate"),
                                       rownames=FALSE)

###########################
###Download Scree Plot#####
###########################

#PNG Download
output$download_PCoA_scree <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCoA_scree.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(1000)
    Y<-as.numeric(400)
    
    png(file, height = Y, width = X)
    
    print(PCoAscree2())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_PCoA_scree_button<- renderUI({
  
  downloadButton("download_PCoA_scree", "Download Plot (.png)")
  
})

#tiff Download
output$download_PCoA_scree_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCoA_scree.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(1000)
    Y<-as.numeric(400)
    
    tiff(file, height = Y, width = X)
    
    print(PCoAscree2())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_PCoA_scree_button_tiff<- renderUI({
  
  downloadButton("download_PCoA_scree_tiff", "Download Plot (.tiff)")
  
})


##########################################
######Principal Co-ordinate Analysis########
##########################################

PCoAcalc<-reactive({
  
  genind<-genind_filt()
  
  #For SSR data
  
  if (max(genind$loc.n.all) > 2) {
    X<-tab(genind, freq = TRUE, NA.method="mean")
    pco<-ade4::dudi.pco(dist(X), scannf=FALSE, nf=20)#input$PCoAaxes)
    pco
    
  }
  
  #For SNP Data
  
  else if (max(genind$loc.n.all) == 2) {
    genlight<-gi2gl(genind)
    pcoagl =gl.pcoa(genlight)
    pcoagl
  }
  
})


PCoAscores<-reactive({
  
  req(PCoAcalc)
  
  genind<-genind_filt()
  
  temp<-PCoAcalc()
  
  #For SSR data
  
  if (max(genind$loc.n.all) > 2) {
   pco<-temp
    scores<-as.data.frame(pco$li)
    scores$pop<-genind$pop
    
    scores
    #names for scatter plots
    rownames(scores)<- rownames(pco$li)
    
    scores
    
  }
  
  #For SNP Data
  
  else if (max(genind$loc.n.all) == 2) {
    pcoagl <-temp
    scores=as.data.frame(pcoagl$scores)
    scores$pop=genind$pop
    
    #names for scatter points
    rownames(scores)=rownames(pcoagl$scores)
    
    scores
    
  }
  
})

PCoAvar1<-reactive({
  
  req(PCoAcalc)
  
  temp<-PCoAcalc()
  genind<-genind_filt()
  
  #For SSR data
  
  if (max(genind$loc.n.all) > 2) {
    
    pco<-temp
    
    #Calculate explained variance
    variance <- pco$eig*100/sum(pco$eig)
    cumvar <- cumsum(variance)
    varexp <- data.frame(eigenvalue<-pco$eig, variance <- variance, cumvariance<-cumvar)
    Axis1var=round(varexp[input$PCoAx,2])
    
  }
  
  #For SNP Data
  
  else if (max(genind$loc.n.all) == 2) {

    pcoagl<-temp
    
    #calculate explained variance
    variance = pcoagl$eig*100/sum(pcoagl$eig)
    cumvar = cumsum(variance)
    varexp= data.frame(eigenvalue = pcoagl$eig, variance = variance, cumvariance = cumvar)
    Axis1var=round(varexp[input$PCoAx,2])
  }
  
})

PCoAvar2<-reactive({
  
  req(PCoAcalc)
  
  temp<-PCoAcalc()
  genind<-genind_filt()
  
  #For SSR data
  
  if (max(genind$loc.n.all) > 2) {
    
    pco<-temp
    
    #Calculate explained variance
    variance <- pco$eig*100/sum(pco$eig)
    cumvar <- cumsum(variance)
    varexp <- data.frame(eigenvalue<-pco$eig, variance <- variance, cumvariance<-cumvar)
    Axis1var=round(varexp[input$PCoAx,2])
    
  }
  
  #For SNP Data
  
  else if (max(genind$loc.n.all) == 2) {
    
    pcoagl<-temp
    
    #calculate explained variance
    variance = pcoagl$eig*100/sum(pcoagl$eig)
    cumvar = cumsum(variance)
    varexp= data.frame(eigenvalue = pcoagl$eig, variance = variance, cumvariance = cumvar)
    Axis1var=round(varexp[input$PCoAx,2])
  }
  
})

#Plot PCoA
PCoAplot<- reactive ({
  
  req(PCoAcalc)
  
  genind<-genind_filt()
  scores<-PCoAscores()
  Axis1var<-PCoAvar1()
  Axis2var<-PCoAvar2()
  
  #Create dataframes to plot
  Axis1<-as.data.frame(scores[input$PCoAx])
  colnames(Axis1)<-"Axis1"
  Axis2<-as.data.frame(scores[input$PCoAy])
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
    
    if (input$colorPCoAstrata == "nostrat") {  #The original option of coloring without any strata definition is still the default
      
      scores$pop <- genind$pop
      colorname<-"Population"
      
    }
    
    else {  #Additionally you can color by the strata of your choosing
      
      pop<-as.matrix(genind$strata[as.numeric(input$colorPCoAstrata)])
      scores$pop<-pop
      
      
      colorname<-"Population"
      
    }
    
  }
  
  
  #Define point shape by strata
  
  if (input$stratdef == "One") {  #If there is no strata defined colours will be exclusively by points
    
    scores$shape<-NULL
    
  }
  
  else {  #If strata were previously defined you can now choose how to color the points
    
    if (input$shapePCoAstrata == "noshape") {
      
      scores$shape <- NULL
      
    }
    
    else if (input$shapePCoAstrata == "nostrat") {  #You can shape points by each original pop
      
      scores$shape <- genind$pop
      
    }
    
    else {  #Additionally you can color by the strata of your choosing
      
      shape<-as.matrix(genind$strata[as.numeric(input$shapePCoAstrata)])
      scores$shape<-shape
      
    }
    
  }
  
  ##################
  ####PLOT GRAPH####
  ##################
  
  
  if (input$spiderPCoA == "FALSE") {
    
    x<-ggplot(scores, aes(x = Axis1,y =Axis2,colour=scores$pop))+
      geom_point(size=input$PCoA_pointsize, alpha=input$PCoA_alpha, aes(shape = scores$shape ))+
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()+
      labs(title="PCoA Individuals", x=paste("PCoA Axis ",input$PCoAx," (",Axis1var, " %)", sep= ""), y=paste("PCoA Axis ",input$PCoAy," (",Axis2var, " %)", sep= ""), color=paste(colorname) ) +
      
      scale_color_viridis(discrete = TRUE, option = input$PCoA_colpal)+
      
      theme(plot.title = element_text(color="black", size=input$PCoA_title, face="bold"),
            axis.title.x = element_text(color="black", size=input$PCoA_axistitle),
            axis.title.y = element_text(color="black", size=input$PCoA_axistitle),
            axis.text = element_text(size = input$PCoA_axistext),
            legend.title = element_text(size = input$PCoA_legend),
            legend.text = element_text(size = input$PCoA_legend),
            legend.position = input$PCoA_legendposition,
            panel.grid = element_blank())+
      scale_color_viridis(discrete = TRUE, option = input$PCoA_colpal)
    
  }
  
  else if (input$spiderPCoA == "TRUE") {
    
    scores<-scores[order(scores$pop),]
    cent<-aggregate(cbind(scores$Axis1,scores$Axis2) ~ pop, data = scores, FUN = mean)
    segs<-merge(scores, setNames(cent, c('pop', 'oAxis1','oAxis2')), 
                by = 'pop', sort = FALSE)
    
    scores$oAxis1 <-segs$oAxis1
    scores$oAxis2 <-segs$oAxis2
    
    
    x<-ggplot(scores, aes(x = Axis1,y =Axis2,colour=scores$pop))+
      geom_point(size=input$PCoA_pointsize, alpha=input$PCoA_alpha, aes(shape = scores$shape ))+
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()+
      labs(title="PCoA Individuals", x=paste("PCoA Axis ",input$PCoAx," (",Axis1var, " %)", sep= ""), y=paste("PCoA Axis ",input$PCoAy," (",Axis2var, " %)", sep= ""), color=paste(colorname) ) +
      
      theme(plot.title = element_text(color="black", size=input$PCoA_title, face="bold"),
            axis.title.x = element_text(color="black", size=input$PCoA_axistitle),
            axis.title.y = element_text(color="black", size=input$PCoA_axistitle),
            axis.text = element_text(size = input$PCoA_axistext),
            legend.title = element_text(size = input$PCoA_legend),
            legend.text = element_text(size = input$PCoA_legend),
            legend.position = input$PCoA_legendposition,
            panel.grid = element_blank())+
      scale_color_viridis(discrete = TRUE, option = input$PCoA_colpal)
    
    x<-x + geom_segment(data=scores,aes(xend = oAxis1, yend = oAxis2))
  }
  
  
  if (input$statellipsePCoA == "TRUE") {
    x<- x + stat_ellipse(size = 1)
  }
  
  if (input$showcentroidPCoA == "TRUE") {
    cent<-aggregate(cbind(scores$Axis1,scores$Axis2) ~ pop, data = scores, FUN = mean)
    
    x<- x + geom_point(data = cent, size = input$PCoA_pointsize + 5, aes(x = cent$V1, y= cent$V2, colour = cent$pop)) 
  } 
  
  x
  
})


observe({
  
  output$PCoAplot<-renderPlot({
    
    req(PCoAplot) 
    
    PCoAplot()
    
  }, width = input$PCoA_X, height = input$PCoA_Y)
  
})



#PCoA centroid plot only


#Plot PCoA Centroids only
PCoAcentroidplot<-reactive ({
  
  req(PCoAcalc)
  
  genind<-genind_filt()
  scores<-PCoAscores()
  Axis1var<-PCoAvar1()
  Axis2var<-PCoAvar2()
  
  #Create dataframes to plot
  Axis1<-as.data.frame(scores[input$PCoAx])
  colnames(Axis1)<-"Axis1"
  Axis2<-as.data.frame(scores[input$PCoAy])
  colnames(Axis2)<-"Axis2"
  scores$Axis1<-Axis1$Axis1
  scores$Axis2<-Axis2$Axis2
  
  #Define point colours by strata
  
  if (input$stratdef == "One") {  #If there is no strata defined colours will be exclusively by points
    
    scores$pop<-genind$pop
    colorname<-"Population"
    
  }
  
  else {  #If strata were previously defined you can now choose how to color the points
    
    if (input$colorPCoAstrata == "nostrat") {  #The original option of coloring without any strata definition is still the default
      
      scores$pop <- genind$pop
      colorname<-"Population"
      
    }
    
    else {  #Additionally you can color by the strata of your choosing
      
      pop<-as.matrix(genind$strata[as.numeric(input$colorPCoAstrata)])
      scores$pop<-pop
      
      
      colorname<-"Population"
      
    }
    
  }
  
  scores<- scores %>% arrange(desc(pop))
  
  
  #Calculate centroids and spiders
  
  cent<-aggregate(cbind(scores$Axis1,scores$Axis2) ~ pop, data = scores, FUN = mean)
  
  #Plot graph
  
  x<-ggplot(cent, aes(x = V1,y =V2,colour=cent$pop))+
    geom_point(data = cent, size = input$PCoA_pointsize + 5) +
    xlim((min(scores$Axis1)),(max(scores$Axis1)))+
    ylim((min(scores$Axis2)),(max(scores$Axis2)))+
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    theme_bw()+
    labs(title="PCoA Centroids", x=paste("PCoA Axis",input$PCoAx, " (",Axis1var, " %)", sep= ""), y=paste("PCoA Axis",input$PCoAy," (",Axis2var, " %)", sep= ""), color=paste(colorname) ) +
    theme(plot.title = element_text(color="black", size=input$PCoA_title, face="bold"),
          axis.title.x = element_text(color="black", size=input$PCoA_axistitle),
          axis.title.y = element_text(color="black", size=input$PCoA_axistitle),
          axis.text = element_text(size = input$PCoA_axistext),
          legend.title = element_text(size = input$PCoA_legend),
          legend.text = element_text(size = input$PCoA_legend),
          legend.position = input$PCoA_legendposition,
          panel.grid = element_blank())+
    scale_color_viridis(discrete = TRUE, option = input$PCoA_colpal)
  
  x
  
})

#Render centroid only plot
observe({
  
  output$PCoAcentroidplot<-renderPlot({
    
    PCoAcentroidplot()
    
  }, width = input$PCoA_X, height = input$PCoA_Y)
  
})

#Show centroid only plot only if the user requests it
output$PCoAcentroid<-renderUI({
  
  if (input$plotcentroidonlyPCoA == TRUE) {
    
    div(style = 'overflow-x: scroll',
        plotOutput("PCoAcentroidplot")
    )
    
  }
  
  
})

##################################
######DOWNLOAD PCoA FIGURES########
##################################

#########PCoA#############

#PNG
output$download_PCoA <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCoA.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$PCoA_X)
    Y<-as.numeric(input$PCoA_Y)
    
    png(file, height = Y, width = X)
    
    print(PCoAplot())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_PCoA_button<- renderUI({
  
  downloadButton("download_PCoA", "Download PCoA Plot (.png)")
  
})

#TIFF
output$download_PCoA_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCoA.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$PCoA_X)
    Y<-as.numeric(input$PCoA_Y)
    
    tiff(file, height = Y, width = X)
    
    print(PCoAplot())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_PCoA_button_tiff<- renderUI({
  
  downloadButton("download_PCoA_tiff", "Download PCoA Plot (.tiff)")
  
})

#########PCoA Centroids Only#############

#PNG
output$download_PCoA_centroids <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCoA_centroids.png', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$PCoA_X)
    Y<-as.numeric(input$PCoA_Y)
    
    png(file, height = Y, width = X)
    
    print(PCoAcentroidplot())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_PCoA_centroids_button<- renderUI({
  
  downloadButton("download_PCoA_centroids", "Download PCoA Centroids Plot (.png)")
  
})

#TIFF
output$download_PCoA_centroids_tiff <- downloadHandler(
  
  filename <- function() {
    paste(input$projectID, 'PCoA_centroids.tiff', sep = "_")
  },
  content <- function(file) {
    X<-as.numeric(input$PCoA_X)
    Y<-as.numeric(input$PCoA_Y)
    
    tiff(file, height = Y, width = X)
    
    print(PCoAcentroidplot())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_PCoA_centroids_button_tiff<- renderUI({
  
  downloadButton("download_PCoA_centroids_tiff", "Download PCoA Centroids Plot (.tiff)")
  
})







