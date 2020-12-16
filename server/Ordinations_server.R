
#Tab: Ordinations

#####PCA



output$colorPCA<-renderUI ({
  
  req(genind_filt)

  if (input$stratdef == "One") {
    
  }
  
  else if (input$stratdef == "Two") {
  
    selectInput("colorPCAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2), 
                selected = "nostrat")
      
  }
  
  else if (input$stratdef == "Three") {
    
    selectInput("colorPCAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Four") {
    
    selectInput("colorPCAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Five") {
    
    selectInput("colorPCAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Six") {
    
    selectInput("colorPCAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5, "Choice 6" = 6), 
                selected = "nostrat")
    
  }
  
  
  
  
  
})

output$shapePCA<-renderUI ({
  
  req(genind_filt)
  
  if (input$stratdef == "One") {
    
  }
  
  else if (input$stratdef == "Two") {
    
    selectInput("shapePCAstrata", label = h5("Point shape by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Three") {
    
    selectInput("shapePCAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Four") {
    
    selectInput("shapePCAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Five") {
    
    selectInput("shapePCAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Six") {
    
    selectInput("shapePCAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5, "Choice 6" = 6), 
                selected = "noshape")
    
  }
  
  
  
})




output$colorPCoA<-renderUI ({
  
  req(genind_filt)
  
  if (input$stratdef == "One") {
    
  }
  
  else if (input$stratdef == "Two") {
    
    selectInput("colorPCoAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Three") {
    
    selectInput("colorPCoAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Four") {
    
    selectInput("colorPCoAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Five") {
    
    selectInput("colorPCoAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Six") {
    
    selectInput("colorPCoAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5, "Choice 6" = 6), 
                selected = "nostrat")
    
  }
  
  
  
  
  
})

output$shapePCoA<-renderUI ({
  
  req(genind_filt)
  
  if (input$stratdef == "One") {
    
  }
  
  else if (input$stratdef == "Two") {
    
    selectInput("shapePCoAstrata", label = h5("Point shape by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Three") {
    
    selectInput("shapePCoAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Four") {
    
    selectInput("shapePCoAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Five") {
    
    selectInput("shapePCoAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Six") {
    
    selectInput("shapePCoAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5, "Choice 6" = 6), 
                selected = "noshape")
    
  }
  
  
  
})








#FCA






#Ordinations

source(file.path("server/Ordinations_PCoA_server.R"),  local = TRUE)$value

source(file.path("server/Ordinations_PCA_server.R"),  local = TRUE)$value





##########################
#########FCA##############
##########################
output$colorFCA<-renderUI ({
  
  req(genind_filt)
  
  if (input$stratdef == "One") {
    
  }
  
  else if (input$stratdef == "Two") {
    
    selectInput("colorFCAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Three") {
    
    selectInput("colorFCAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Four") {
    
    selectInput("colorFCAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Five") {
    
    selectInput("colorFCAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5), 
                selected = "nostrat")
    
  }
  
  else if (input$stratdef == "Six") {
    
    selectInput("colorFCAstrata", label = h5("Color points by strata"), 
                choices = list("pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5, "Choice 6" = 6), 
                selected = "nostrat")
    
  }
  
})

output$shapeFCA<-renderUI ({
  
  req(genind_filt)
  
  if (input$stratdef == "One") {
    
  }
  
  else if (input$stratdef == "Two") {
    
    selectInput("shapeFCAstrata", label = h5("Point shape by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Three") {
    
    selectInput("shapeFCAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Four") {
    
    selectInput("shapeFCAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Five") {
    
    selectInput("shapeFCAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5), 
                selected = "noshape")
    
  }
  
  else if (input$stratdef == "Six") {
    
    selectInput("shapeFCAstrata", label = h5("Color points by strata"), 
                choices = list("All circles" = "noshape","pop" = "nostrat", "Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5, "Choice 6" = 6), 
                selected = "noshape")
    
  }
  
  
  
})


















#####FCA

fcaplot1<- reactive({
  
  req(test)
  
  genind2<-genind_filt()
  
  #Extract contingency tables (sample x allele freqs) for population and individual levels from genind / genpop objects
  contab<-as.data.frame(genind2$tab) #create contingency table of allele frequencies per individual
  pop<- (genind2$pop) #Create dataframe of population assignments for each individual 
  
  #Individual level FCA
  #Uses soc.ca because this package can tolerate missing values (NAs) unlike 'FactoMineR' and 'afc' packages
  
  result<-soc.ca::soc.mca(contab) #Perform correspondence analysis
  
  ca=as.data.frame(result$coord.ind) #Extract coordinates from correspondence analysis
  
  eig<-as.data.frame(result$eigen) 
  Axis1 = round(eig[1,]*100, digits = 2) #Proportion of variance of the data explained by axis 1
  Axis2 = round(eig[2,]*100,digits = 2) #Proportion of variance of the data explained by axis 2
  
  #Individual plots
  ca$Axis1<-ca$V1 #rename so it appears clearer on the plot
  ca$Axis2<-ca$V2
  
  x<-ggplot(ca, aes(ca$Axis1,ca$Axis2,colour=pop))+
    geom_point(size=2, alpha=80/100)+
    scale_color_viridis(discrete=TRUE)+
    theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
          axis.title.x = element_text(color="black", size=12),
          axis.title.y = element_text(color="black", size=12))+
    geom_vline(xintercept = 0) +
    theme_classic()+
    labs(title="FCA Individuals", x=paste("FCA Axis 1 ","(",Axis1, " %)", sep= ""), y=paste("FCA Axis2 ","(",Axis2, " %)", sep= ""), color="Population")
  
})

output$fcaplot<- renderPlotly({
  req(fcaplot1)
  
  ggplotly(fcaplot1())
  
})
