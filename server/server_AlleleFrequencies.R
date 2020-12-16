#TAB: GENETIC DIVERSITY

######ALLELE FREQUENCIES

#Calculate allele frequencies over the entire dataset
freqtabind <- reactive({
  genind <-genind_filt()
  freq <- as.data.frame(adegenet::makefreq(genind))
})

#Calculate allele frequencies per population
freqtabpop<-reactive({
  genind <- genind_filt()
  freq <- as.data.frame(adegenet::makefreq(genind))
  strat<-genind@pop
  freq$pop<-strat
  #Make the function
  funx <- function(x) {sum(x, na.rm=TRUE) / length(x) }
  #Summarize frequencies by pop
  freq2<-freq %>% 
    group_by(pop) %>%
    summarise_all(list(funx))
  pop<-freq2$pop
  freq2<-as.data.frame(t(freq2))
  colnames(freq2)<- pop
  freq2<-freq2[-1,]
  freq2<-as.data.frame(t(freq2))
})

#Visualise and download table of allele frequencies for the whole dataset
output$freqtabind <- renderDataTable({
  temp<-freqtabind()
  DT::datatable(temp)
})

#Visualise and download table of allele frequencies per population
output$freqtabpop <- renderDataTable({
  temp<-freqtabpop()
  DT::datatable(temp)
})