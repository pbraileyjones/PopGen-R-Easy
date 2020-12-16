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



output$amovapie <- renderPlotly({
  
  amova.tab<-amova.tab()
  plot_ly(amova.tab,
          labels = rownames(amova.tab), 
          values = amova.tab$Proportion, 
          type = 'pie', 
          textposition = 'outside',
          textinfo = 'label+percent') %>%
    layout(title = 'AMOVA',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
})