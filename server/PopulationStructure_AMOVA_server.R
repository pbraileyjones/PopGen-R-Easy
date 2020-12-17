#

amova.dat <- eventReactive(input$AMOVAGOGO,{
  
  
  genind<-genind_filt()
  
  pop<-as.data.frame(genind@pop)
  genind@strata<-pop
  colnames(genind@strata)<-"pop"
  
  Amova <- poppr::poppr.amova(genind, hier=~pop, nperm=0, within=TRUE)
  Amova
  
} )


amova.tab <- eventReactive(input$AMOVAGOGO,{
  
  amova.dat<-amova.dat()
  perc <- amova.dat$componentsofcovariance[-4,]
  temp<-rownames (perc)
  perc<-cbind(temp, perc)
  names(perc)=c("Comparison","Sigma", "Proportion")
  perc
  
})

output$amovatab <- renderDataTable(amova.tab(), extensions = 'Buttons', 
                                   options = list(dom = 'Bfrtip',
                                                  buttons = list('copy', list(extend ='csv', filename=paste(input$projectID, "_AMOVA")),list(extend='excel',filename=paste(input$projectID,"_AMOVA")))),
                                   caption = "AMOVA results",
                                   rownames=FALSE
)

  
amovapie <- reactive({
  
  perc<-amova.tab()
  
  labelsize<-input$amova_perc
 
  perc<- perc %>% 
    dplyr::arrange(dplyr::desc(Comparison)) %>%
    mutate (prop= Proportion / sum (perc$Proportion) * 100 ) %>%
    mutate (ypos = cumsum(prop) - 0.5 * prop)
  
  plot<-ggplot(data = perc, aes(x = "", y = Proportion, fill = Comparison)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(title = input$amova_titletext, fill = input$amova_legendtext) +
    geom_text(aes(x =input$amova_percpos, y = ypos, label = paste(round(Proportion,2), "%")), size = labelsize, show_guide = FALSE)+
    theme(legend.title = element_text(size = input$amova_legend),
          legend.text = element_text(size = input$amova_legend),
          title = element_text(size = input$amova_title),
          legend.position = input$amova_legendposition
          )
  
  #Color palette options
  if(input$amova_colpal == "D"){plot<-plot+ scale_fill_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)}
  else if (input$amova_colpal == "magma"){plot<-plot+scale_fill_viridis(discrete=TRUE, option = "magma") }
  else if (input$amova_colpal == "inferno"){plot<-plot+scale_fill_viridis(discrete=TRUE, option = "inferno")}
  else if (input$amova_colpal == "plasma"){plot<-plot+scale_fill_viridis(discrete=TRUE, option = "plasma") }
  else if (input$amova_colpal == "cividis"){plot<-plot+scale_fill_viridis(discrete=TRUE, option = "cividis") }
 
  else if (input$amova_colpal == "scalecol1"){plot<-plot+scale_fill_brewer(palette = "Set1") }
  else if (input$amova_colpal == "scalecol2"){plot<-plot+scale_fill_brewer(palette = "Set2") }
  else if (input$amova_colpal == "scalecol3"){plot<-plot+scale_fill_brewer(palette = "Set3") }
  
  else if (input$amova_colpal == "scalecolblues"){plot<-plot+scale_fill_brewer(palette = "Blues") }
  else if (input$amova_colpal == "scalecolgreens"){plot<-plot+scale_fill_brewer(palette = "Greens") }
  else if (input$amova_colpal == "scalecolgreys"){plot<-plot+scale_fill_brewer(palette = "Greys") }
  else if (input$amova_colpal == "scalecolreds"){plot<-plot+scale_fill_brewer(palette = "Reds") }
  else if (input$amova_colpal == "scalecolspectral"){plot<-plot+scale_fill_brewer(palette = "Spectral") }
  
  plot
  
})


observe({
  
  output$amovapie<-renderPlot({
    
    amovapie()
    
  }, width = input$amova_X, height = input$amova_Y)
  
})

#############################
##Download AMOVA Pie Chart###
#############################

#PNG
output$downloadamova <- downloadHandler(
  filename <- function() {
    paste(input$projectID, 'AMOVA_piechart.png', sep = "_")
  },
  content <- function(file) {
    
    X<-as.numeric(input$amova_X)
    Y<-as.numeric(input$amova_Y)
    
    png(file, height = X, width =Y)
    
    print(amovapie())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_amovabutton<- renderUI({
  
  req(amovapie)
  
  downloadButton("downloadamova", "Download the plot (.png)")
  
})

#tiff
output$downloadamova_tiff <- downloadHandler(
  filename <- function() {
    paste(input$projectID, 'AMOVA_piechart.tiff', sep = "_")
  },
  content <- function(file) {
    
    X<-as.numeric(input$amova_X)
    Y<-as.numeric(input$amova_Y)
    
    tiff(file, height = X, width =Y)
    
    print(amovapie())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_amovabutton_tiff<- renderUI({
  
  req(amovapie)
  
  downloadButton("downloadamova_tiff", "Download the plot (.tiff)")
  
})

