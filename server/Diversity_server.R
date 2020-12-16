#####GENETIC DIVERSITY

#Observed Heterozygosity

Hotab<-reactive({
  
  genind<-genindanalysis()
  
  #Observed heterozigosity
  Ho<-NULL;
  for (i in unique(genind$pop)){
    temp<-poppr::popsub(genind, sublist = i, drop = TRUE)
    temp<-summary(temp)
    temp<-temp$Hobs
    Ho<-cbind(Ho, temp)
  }
  colnames(Ho)<-unique(genind$pop)
  Ho
  
})

Homean<- reactive({
  
  req(Hotab)
  
  Ho<-Hotab()
  
  finalHo=round(as.data.frame(colMeans(Ho)),3)
  SDHo=matrixStats::colSds(as.matrix(Ho))
  SEHo=SDHo/sqrt(nrow(Ho))
  finalHo$SE=round(SEHo,3)
  finalHo$pop=rownames(finalHo)
  names(finalHo)=c("Ho","SE_Ho", "pop")
  finalHo["Ho \u00b1 SE"]=do.call(paste, c(finalHo[c("Ho", "SE_Ho")], sep = " \u00b1 "))
  finalHo
  
})

#Expected Heterozygosity (He)

Hetab<- reactive({
  
  genind2<-genindanalysis()
  
  He<-NULL;
  for (i in unique(genind2$pop)){
    temp<-poppr::popsub(genind2, sublist = i, drop = TRUE)
    temp<-summary(temp)
    temp<-temp$Hexp
    He<-cbind(He, temp)
  }
  colnames(He)<-unique(genind2$pop)
  He
  
})

Hemean<- reactive({
  
  req(Hetab)
  
  He<-Hetab()
  
  He<-as.data.frame(He)
  finalHe=round(as.data.frame(colMeans(He)),3)
  SDHe=matrixStats::colSds(as.matrix(He))
  SEHe=SDHe/sqrt(nrow(He))
  finalHe$SE=round(SEHe,3)
  finalHe$pop=rownames(finalHe)
  names(finalHe)=c("He","SE_He", "pop")
  finalHe["He \u00b1 SE"]=do.call(paste, c(finalHe[c("He", "SE_He")], sep = " \u00b1 "))
  finalHe
  
})

#Effective number of alleles (Ae)

Aetab<- reactive ({
  
  req(Hetab)
  
  He<-Hetab()
  
  Ae=1/(1-He)
  Ae<-as.data.frame(Ae)
  HmeanAe=round(as.data.frame(sapply(Ae,mmod::harmonic_mean)),3)
  HmeanAe$pop=rownames(HmeanAe)
  names(HmeanAe)=c("Ae", "pop")
  HmeanAe
  
})

#Inbreeding Coefficient (FIS)

Fis<- reactive({
  
  genind <- genindanalysis()
  
  #Inbreeding Coefficient (Fis)
  temp<-hierfstat::basic.stats(genind)
  temp<-as.data.frame(temp$Fis)
  temp=tidyr::gather(temp, pop, FIS, na.rm = FALSE, convert = FALSE)
  temp
  
})

Fistab<- reactive({
  
  req(Fis)
  
  temp<-Fis()
  
  finalFIS<-temp %>% dplyr::group_by(pop) %>%
    dplyr::summarize(
      N = length(FIS) - sum(is.na(FIS)),
      mean = mean(FIS, na.rm = TRUE),
      sd   = sd(FIS, na.rm =TRUE),
      se   = sd / sqrt(N),
    )
  finalFIS$mean<-round(finalFIS$mean, 3)
  finalFIS$sd<-round(finalFIS$sd, 3)
  finalFIS$se<-round(finalFIS$se, 3)
  names(finalFIS)=c("pop","N", "Fis","SD","SE")
  finalFIS["Fis \u00b1 SE"]=do.call(paste, c(finalFIS[c("Fis", "SE")], sep = "\u00b1"))
  finalFIS
  
})

#combined table



div.tab<-reactive({
  
  
  finalFIS<-Fistab()
  finalHo<-Homean()
  finalHe<-Hemean()
  HmeanAe<-Aetab()
  
  final <- Reduce(function(x,y) merge(x =x, y = y, by = "pop"),
                  list(HmeanAe[c("pop", "Ae")],
                       finalHe[c("pop","He \u00b1 SE")],
                       finalHo[c("pop", "Ho \u00b1 SE")],
                       finalFIS[c("pop","Fis \u00b1 SE" )]))
  
  
})


output$divtab <- renderDataTable(div.tab(), extensions = 'Buttons', 
                                    options = list(dom = 'Bfrtip',
                                                   buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_GeneticDiversity_SummaryTable")), list(extend='excel', filename= paste(input$projectID, "_GeneticDiversity_SummaryTable")) )),
                                    caption = "Summary of mean genetic diversity statistics 
                                 per population +- standard error (SE) where appropriate. 
                                 Statistics include the effective number of alleles per locus (Ae), 
                                 the mean expected heterozygosity under Hardy-Weinberg Equilibrium 
                                 (He), the mean observed heterozygosity (Ho), 
                                 and the inbreeding coefficient F-statistic (FIS).",
                                    rownames=FALSE
)



LDOutput<-eventReactive(input$LDpopGO,{
  
  genind<-genind_filt()
  
  link<-NULL;
  for (i in unique(genind$pop)) {
    temp<-poppr::popsub(genind, sublist = i, drop = TRUE)
    x<-poppr::ia(temp, sample= as.numeric(input$LDpop_perm)) 
    link<-rbind(link,x)
  }
  rownames(link)<-unique(genind$pop)
  link<-round(link,3)
  link
  y<-rownames(link)
  link<-cbind(y,link)
  colnames(link)<-c("pop","Ia","p-value.Ia", "rbarD","p-value.rD")
  link
  
})


output$LDpop <- renderDataTable(LDOutput(), extensions = 'Buttons', 
                                 options = list(dom = 'Bfrtip',
                                                buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_LinkageDisequilibrium_Table")), list(extend='excel', filename= paste(input$projectID, "LinkageDisequilibrium_Table")) )),
                                 caption = "Linkage Disequilibrium statistics by population",
                                 rownames=FALSE
)


#Statistics

#Fis permutations

FISstatCI<-reactive({
  
  req(div.tab)
  
  genind<-genindanalysis()
  
  temp<-hierfstat::basic.stats(genind)
  temp<-as.data.frame(temp$Fis)
  temp=tidyr::gather(temp, pop, FIS, na.rm = FALSE, convert = FALSE)
  
  
  #Statistical analysis
  pop<-unique(genind$pop)
  bootFIS<-hierfstat::boot.ppfis(genind, nboot=999, diploid = TRUE, quantile = c(0.95))
  bootFIS<-as.data.frame(bootFIS$fis.ci)
  bootFIS$ci<-paste("(",bootFIS$ll,",", bootFIS$hl,")")
  bootFIS<-cbind(pop, bootFIS)
  bootFIS
  
})

output$FISstatCI <- renderDataTable(FISstatCI(), extensions = 'Buttons', 
                                 options = list(dom = 'Bfrtip',
                                                buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_GeneticDiversity_InbreedingCoefficient_StatsTable")), list(extend='excel', filename= paste(input$projectID, "_GeneticDiversity_InbreedingCoefficient_StatsTable")) )),
                                 caption = "Mean intra-population FIS values and confidence intervals calculated by bootstrapping values over 999 iterations. If 0 does not fall in the range of the lower and upper bounds of the confidence intervals, this can be interpreted as a significant deviation from such.",
                                 rownames=FALSE
)


output$renderFIScomp <- renderUI({
  
  req(FISstatCI)
  req(div.tab)
  
  div(style = 'overflow-x: scroll',
      
      withSpinner(DT::dataTableOutput("FISstatCI"))
      
  )
  
})
  


#Ho differences

Hostat<- reactive({
  
  req(Hotab)
  
  Ho<-Hotab()
  
  Ho<-as.data.frame(Ho)
  Ho
  
  Ho<-tidyr::pivot_longer(Ho, 1:as.numeric(length(colnames(Ho))), names_to = "pop" )
  
  kr<-kruskal.test(Ho$value~Ho$pop)
  
  Hostatsum<-NULL
  Hostatsum<-as.data.frame(cbind(Hostatsum, kr$parameter, kr$statistic, kr$p.value))
  colnames(Hostatsum)<-c("df", "Chi-squared", "p-value")
  Hostatsum
  
})

Hostattab<-reactive({
  
  Hostatsum<-Hostat()
  
  DT::datatable(Hostatsum,
                rownames = FALSE,
                caption = paste("Statistical output of the kruskall-wallis test comparing observed heterozygosity across populations"),
                options = list(
                  dom = 'lti'))
  
})

output$Hostat <- DT::renderDataTable({
  
  req(Hostattab)
  
  Hostat<-Hostattab()
  
})

Hoposthoc<-reactive({
  
  req(Hotab)
  
  Ho<-Hotab()
  
  Ho<-as.data.frame(Ho)
  
  
    pht<-pairwise.wilcox.test(Ho$value, Ho$pop,
                              p.adjust.method = "BH")
    y<-as.data.frame(pht$p.value)  
   
    DT::datatable(y,
                  caption = paste("Post-Hoc comparisons of observed heterozygosity across populations. Post-hoc comparisons were performed using a pairwise Wilcox test with Bonferroni corrections for multiple comparisons."),
                  options = list(
                    dom = 'lti'))
      
      
})

output$Hoposthoc<-DT::renderDataTable({
  
  req(Hoposthoc)
  
  Hoposthoc()
  
  
})

output$renderHostat<-renderUI({
  
  Hostat<-Hostat()
  
  if (Hostat[3] <0.05) {
    
    DT::dataTableOutput("Hoposthoc")
    
  }
  
})


Hestat<- reactive({
  
  req(Hetab)
  
  He<-Hetab()
  
  He<-as.data.frame(He)
  He
  
  He<-tidyr::pivot_longer(He, 1:as.numeric(length(colnames(He))), names_to = "pop" )
  
  kr<-kruskal.test(He$value~He$pop)
  
  Hestatsum<-NULL
  Hestatsum<-as.data.frame(cbind(Hestatsum, kr$parameter, kr$statistic, kr$p.value))
  colnames(Hestatsum)<-c("df", "Chi-squared", "p-value")
  Hestatsum
  
})

Hestattab<-reactive({
  
  Hestatsum<-Hestat()
  
  DT::datatable(Hestatsum,
                rownames = FALSE,
                caption = paste("Statistical output of the kruskall-wallis test comparing observed heterozygosity across populations"),
                options = list(
                  dom = 'lti'))
  
})

output$Hestat <- DT::renderDataTable({
  
  req(Hestattab)
  
  Hestattab()
  
})

Heposthoc<-reactive({
  
  req(Hestat)
  req(Hetab)
  
  Hostatsum<-Hestat()
  He<-Hetab()
  
  He<-as.data.frame(He)
  
  pht<-pairwise.wilcox.test(He$value, He$pop,
                            p.adjust.method = "BH")
  y<-as.data.frame(pht$p.value)  
  
  DT::datatable(y,
                caption = paste("Post-Hoc comparisons of expected heterozygosity across populations. Post-hoc comparisons were performed using a pairwise Wilcox test with Bonferroni corrections for multiple comparisons."),
                options = list(
                  dom = 'lti'))
  
  
})

output$Heposthoc<-DT::renderDataTable({
  
  req(Heposthoc)
  
  Heposthoc()
  
  
})

output$renderHestat<-renderUI({
  
  Hestat<-Hestat()
  
  if (Hestat[3] < 0.05) {
    
    DT::dataTableOutput("Heposthoc")
    
  }
  
  
})

#Locus measures

#Genotype

GlobalHW.tab<-eventReactive(input$HWGO,{
  
  genind<-genind_filt()
  
  G.hwe<-as.data.frame(pegas::hw.test(genind, B=0))
  colnames(G.hwe)<-c("chi^2","df","p")
  G.hwe<-G.hwe[c("chi^2","p")]
  G.hwe$Bon.p <- p.adjust(G.hwe$p, method = "bonferroni", n = length(G.hwe$p))
  G.hwe.sig<- G.hwe[G.hwe$Bon.p < 0.05,]
  G.hwe.sig
  G.hwe.sig$Locus<- rownames(G.hwe.sig)
  G.hwe.sig
  
})

output$GlobalHW.tab<-renderTable({
  
  req(GlobalHW.tab)
  
  temp<-GlobalHW.tab()
  
  temp
})

LocalHW.tab <- eventReactive(input$HWGO,{
  
  genind<-genind_filt()
  
  L.hwe <- seppop(genind) %>% lapply(pegas::hw.test, B =0)
  temp<-as.data.frame(L.hwe[1])
  L.hwe <- plyr::ldply(L.hwe, data.frame)
  L.hwe$locus<-rownames(temp)
  colnames(L.hwe)<-c("population", "chi^2","df", "p", "locus")
  L.hwe<-L.hwe[c("population", "locus", "chi^2", "p")]
  L.hwe$Bon.p<- p.adjust(L.hwe$p, method = "bonferroni", n = length(L.hwe$p))
  L.hwe.sig<-L.hwe[L.hwe$Bon.p < 0.05,]
  L.hwe.sig
  
})

output$LocalHW.tab<-renderTable({
  
  req(LocalHW.tab)
  
  temp<-LocalHW.tab()
  
  temp
})

LDloc<-eventReactive(input$LDlocGO,{
  
  genind<-genind_filt()
  
  temp<-pair.ia(genind)
  plot(temp)
  
})

output$LDlocplot<-renderTable({
  
  req(LDloc)
  
  temp<-LDloc()
  
  temp
  
})
