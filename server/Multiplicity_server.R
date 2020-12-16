
#TAB: GENETIC DIVERSITY

#####GENETIC MULTIPLICITY


Ntab<-reactive({
  
  genind <-genindanalysis()
  
  N<-summary(genind)
  N<-as.data.frame((N$n.by.pop))
  N$pop<-rownames(N)
  colnames(N)<-c("N", "pop")
  N
  
})


Nmintab<-reactive({
  
  genind <-genindanalysis()
  
  N<-summary(genind)
  N<-as.data.frame((N$n.by.pop))
  N$pop<-rownames(N)
  colnames(N)<-c("Nmin", "pop")
  N$Nmin<-min(N$Nmin)
  N
  
})

#Na

Natab<- reactive({
  
 
  genind <-genindanalysis()
  
  Nallele=NULL;
  for (i in unique(genind$pop))
  {
    tmp=Na(genind, population=i)
    Nallele=rbind(Nallele,tmp)
  }
  Nallele=round(as.data.frame(rowMeans(Nallele)),3)
  rownames(Nallele)=unique(genind$pop)
  names(Nallele)="A"
  Nallele$pop=rownames(Nallele)
  Nallele
  
})

#Polymorphic Loci

poly<-reactive({
  
  genind<-genindanalysis()
  
  mon<-NULL;
  pol<-NULL;
  per<-NULL
  for (i in unique(genind$pop)) {
    temp<-poppr::popsub(genind, sublist =i, drop= TRUE)
    temp<-summary(temp)
    temp
    loc<-temp$loc.n.all
    total<-length(temp$loc.n.all)
    
    monomorphs<-as.matrix((table(loc) [names(table(loc)) == 1]))
    if(is.na(monomorphs[1]) == TRUE) {
      monomorphs = 0
    }
    polymorphs<-as.matrix(total - monomorphs)
    P<-as.matrix((polymorphs/total) *100)
    mon<-rbind(mon, monomorphs)
    pol<-rbind(pol, polymorphs)
    per<-rbind(per, P)
  }
  
  pop<-as.data.frame(unique(genind$pop))
  FinalP<-cbind(pop, total,mon, pol, per)
  FinalP
  colnames(FinalP)<-c("pop", "total loci", "monomorphic loci", "polymorphic loci", "%P")
  FinalP$`%P`<-round(FinalP$`%P`,0)
  FinalP
  
})

#Rarefied allelic richness

rare<- reactive({
 
  genind<-genindanalysis()
  
  # Extract the data to use
  data.list = list()
  data.list$tab <- genind@tab
  data.list$loc.fac <- factor(genind@loc.fac)
  data.list$pop <- factor(genind@pop)
  
  # Run rarefaction analyses, using smallest population as minimal cutoff
  min.size <- min(table(data.list$pop))
  rarefied.values <- rarefiedMatrices(data = data.list, 
                                      g = min.size, 
                                      display.progress = TRUE)
  rarefied.values
})

Armin <- reactive({
  
  req(rare)
  
  rarefied.values <- rare()
  
  richness.matrix<-rarefied.values$richness
  
}) 

pArmin <- reactive({
  
  req(rare)
  
  rarefied.values <- rare()
  
  private.matrix<-rarefied.values$private
  
}) 

Ar<- reactive({
  
  req(Armin)
  
  richness.matrix<-Armin()
  richness.matrix<-as.data.frame(richness.matrix)
  
  richness.matrix=tidyr::gather(richness.matrix, pop, Ar, na.rm = TRUE, convert = FALSE)
  
  finalAr<-richness.matrix %>% dplyr::group_by(pop) %>%
    dplyr::summarize(
      N = length(Ar) - sum(is.na(Ar)),
      mean = mean(Ar, na.rm = TRUE),
      sd   = sd(Ar, na.rm =TRUE),
      se   = sd / sqrt(N),
    )
  
  finalAr$mean<-round(finalAr$mean, 3)
  finalAr$sd<-round(finalAr$sd, 3)
  finalAr$se<-round(finalAr$se, 3)
  names(finalAr)=c("pop","N", "Ar","SD","SE")
  finalAr["Ar \u00b1 SE"]=do.call(paste, c(finalAr[c("Ar", "SE")], sep = "\u00b1"))
  finalAr
  
})

pAr<- reactive({
  
  req(pArmin)
  
  private.matrix<-pArmin()
  
  private.matrix<-as.data.frame(private.matrix)
  
  private.matrix=tidyr::gather(private.matrix, pop, pAr, na.rm = TRUE, convert = FALSE)
  
  finalpAr<-private.matrix %>% dplyr::group_by(pop) %>%
    dplyr::summarize(
      N = length(pAr) - sum(is.na(pAr)),
      mean = mean(pAr, na.rm = TRUE),
      sd   = sd(pAr, na.rm =TRUE),
      se   = sd / sqrt(N),
    )
  
  finalpAr$mean<-round(finalpAr$mean, 3)
  finalpAr$sd<-round(finalpAr$sd, 3)
  finalpAr$se<-round(finalpAr$se, 3)
  names(finalpAr)=c("pop","N", "pAr","SD","SE")
  finalpAr["pAr \u00b1 SE"]=do.call(paste, c(finalpAr[c("pAr", "SE")], sep = "\u00b1"))
  finalpAr
  
})

Arstats<-reactive({
  
  req(Armin)
  
  richness.matrix<-Armin()
  
  richness.matrix<-as.data.frame(richness.matrix)
  
  Ar<-tidyr::gather(richness.matrix, pop, Ar, na.rm = TRUE, convert = FALSE)
  
  
    kr<-kruskal.test(Ar$Ar~Ar$pop)
    
    Arstatsum<-NULL
    Arstatsum<-as.data.frame(cbind(Arstatsum, kr$parameter, kr$statistic, kr$p.value))
    colnames(Arstatsum)<-c("df","Chi-squared", "p-value")
    Arstatsum
  
  
})

output$Arstats<-DT::renderDataTable({
  
  req(Arstats)
  
  temp<- Arstats()
  datatable(temp,
            rownames = FALSE,
            caption = paste("Statistical output of the kruskall-wallis test comparing allelic richness across populations"),
            options = list(
              dom = 'lti',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
            extensions = 'Buttons', 
  )
  
})


pArstats<-reactive({
  
  req(pArmin)
  
  private.matrix<-pArmin()
  
  private.matrix<-as.data.frame(private.matrix)
  
  pAr<-tidyr::gather(private.matrix, pop, pAr, na.rm = TRUE, convert = FALSE)
  
  kr<-kruskal.test(pAr$pAr~pAr$pop)
  
    Arstatsum<-NULL
    Arstatsum<-as.data.frame(cbind(Arstatsum, kr$parameter, kr$statistic, kr$p.value))
    colnames(Arstatsum)<-c("df", "Chi-squared", "p-value")
    Arstatsum
  
  
})


output$pArstats <- renderDataTable(pArstats(), extensions = 'Buttons', 
                                options = list(dom = 'Bfrtip',
                                               buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_Multiplicity_Stats_pAr")), list(extend='excel', filename= paste(input$projectID, "_Multiplicity_Stats_pAr")) )),
                                caption = "Statistical output of the kruskall-wallis test comparing private allelic richness across populations",
                                rownames=TRUE
)


output$Arposthoc<-DT::renderDataTable ({
  
  req(Arstats)
  req(Armin)
  
  richness.matrix<-Armin()
  aov<-Arstats()
  
  richness.matrix<-as.data.frame(richness.matrix)
  
  Ar<-tidyr::gather(richness.matrix, pop, Ar, na.rm = TRUE, convert = FALSE)
  
  
  
    
    pht<-pairwise.wilcox.test(Ar$Ar, Ar$pop,
                              p.adjust.method = "BH")
    y<-as.data.frame(pht$p.value)
  
  
  DT::datatable(y,
                caption = paste("Post-Hoc comparisons of allelic richness across populations. Post-hoc comparisons were performed using a pairwise Wilcox test with Bonferroni corrections for multiple comparisons."),
                options = list(
                  dom = 'lti'))
  
})



output$pArposthoc<-DT::renderDataTable({
  
  req(pArstats)
  req(pArmin)
  
  private.matrix<-pArmin()
  aov<-pArstats()
  
  private.matrix<-as.data.frame(private.matrix)
  
  pAr<-tidyr::gather(private.matrix, pop, pAr, na.rm = TRUE, convert = FALSE)
  
 
    
    pht<-pairwise.wilcox.test(pAr$pAr, pAr$pop,
                              p.adjust.method = "BH")
    y<-as.data.frame(pht$p.value)
  
  
  DT::datatable(y,
                caption = paste("Post-Hoc comparisons of private allelic richness across populations. Post-hoc comparisons were performed using a pairwise Wilcox test with Bonferroni corrections for multiple comparisons."),
                options = list(
                  dom = 'lti'))
  
  
})

output$renderARposthoc<-renderUI({
  
  Arstats<-Arstats()
  
  if (Arstats[3] < 0.05 ) {
    
    tagList(
    
    hr(),
    
    DT::dataTableOutput("Arposthoc")
    
    )
  }
  
})

output$renderpARposthoc<-renderUI({
  
  pArstats<-pArstats()
  
  if (pArstats[3] < 0.05 ) {
    
    tagList(
    
    hr(),
    
    DT::dataTableOutput("pArposthoc")
    
    )
    
  }
  
})



#Combined Table

mult.tab<-reactive ({
  
  req(pAr)

  Ntab<-Ntab()  
  Nmintab<-Nmintab()
  Nallele<-Natab()
  FinalP<-poly()
  finalAr<-Ar()
  finalpAr<-pAr()  
  
  final=Reduce(function(x,y) merge(x =x, y = y, by = "pop"), 
               list(Ntab, 
                    Nallele, FinalP[c("%P", "pop")],  
                    finalAr[c("pop", "Ar \u00b1 SE")],
                    finalpAr[c("pop", "pAr \u00b1 SE")],
                    Nmintab
               ))

})

projectID2<-reactive({
  req(input$projectID)
  
  projectID
  
})

output$mult.tab <- renderDataTable(mult.tab(), extensions = 'Buttons', 
                              options = list(dom = 'Bfrtip',
                                             buttons = list('copy', list(extend ='csv', filename=paste(input$projectID, "_GeneticMultiplicity_Table1")),list(extend='excel',filename=paste(input$projectID,"_GeneticMultiplicity_Table1")))),
                              caption = "Summary of mean genetic multiplicity statistics 
                              per population. Statistics include the number of individuals 
                              observed (N), the mean number of observed 
                              alleles per locus (A), the percentage of polymorphic 
                              loci observed (%P), the mean rarefied allic richness (Ar),
                              the mean rarefied private allelic richnes (pAr) and the minimum
                              sampling depth used to calculate rarefied richness values (Nmin)",
                              rownames=FALSE
)



#############################
#####Rarefaction Curves######
#############################

#Calculate allelic richness and generate figure
ArRareCurve <- eventReactive(input$rarecurveGO, {

  genind<-genindanalysis()
  
  
  # Extract the data to use
  data.list = list()
  data.list$tab <- genind@tab
  data.list$loc.fac <- factor(genind@loc.fac)
  data.list$pop <- factor(genind@pop)
  
  # Run rarefaction analyses for a series of sample sizes to generate a rarefaction curve to plot
  min.size <- min(table(data.list$pop))
  
  if (input$arsteps == "all") {
    
    sample.size <- seq(from = 1, min.size) 
    }
  
  else if (input$arsteps == "partial")  {
    
    sample.size <- seq(from = 1, to = min.size, by = (min.size/30))
    
  }
  
  
  y <- NULL;
  x<-NULL;
  z<-NULL;
  
  for (i in sample.size)
  {
    rarefied.values <- rarefiedMatrices(data = data.list, 
                                        g = i, 
                                        display.progress = TRUE)
    richness.matrix <- as.data.frame(rarefied.values$richness)
    StdErr<-sapply(richness.matrix,function(x)sd(x, na.rm = TRUE)/sqrt(length(x)))
    StdDev<-sapply(richness.matrix,function(x)sd(x, na.rm = TRUE))
    Means<-colMeans(richness.matrix, na.rm = TRUE)
    y<-rbind(y, Means)
    x<-cbind(x, StdErr)
    z<-cbind(z, StdDev)
    
  }
  y<-as.data.frame(y)
  x<-as.data.frame(t(x))
  z<-as.data.frame(t(z))
  
  
  ARtab<-tidyr::gather(y, Population, AR)
  ARtab$sample_size<-sample.size
  
  ARtab<-tidyr::gather(y, Population, AR)
  x<-tidyr::gather(x, Population, SE)
  z<-tidyr::gather(z, Population, SD)
  ARtab$SE <- x$SE
  ARtab$SD <- z$SD
  ARtab$sample_size<-sample.size
  
  ARtab
})


ArRareCurve2<-reactive({
  
  req(ArRareCurve)
  ARtab<-ArRareCurve()
  
  plot<-ggplot(data=ARtab, mapping = aes(sample_size, AR, color=Population, group=Population))+
    geom_point(size=1.5)
    
    if (input$Arcurve_error == "se_bar") {plot<-plot + geom_errorbar(aes(ymin = AR-ARtab$SE, ymax = AR + ARtab$SE, alpha = 0.1))} 
    else if (input$Arcurve_error == "sd_bar") {plot<-plot + geom_errorbar(aes(ymin = AR-SD, ymax = AR + SD, alpha = 0.1))} 
    else if (input$Arcurve_error == "se_col") {plot<-plot + geom_ribbon(aes(ymin = AR-SE, ymax = AR + SE, alpha = 0.1, fill = Population))}
    else if (input$Arcurve_error == "sd_col") {plot<-plot + geom_ribbon(aes(ymin = AR-SD, ymax = AR + SD, alpha = 0.1, fill = Population))} 
    else if (input$Arcurve_error == "none") {}
  
  plot<- plot +   
  geom_line(size=1.5)+
    scale_alpha(guide= "none")+
    labs(x="Sample Size", y="Mean Rarefied Allelic Richness", title=input$Arcurve_titletext, color=input$Arcurve_legendtext)+
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      title = element_text(size = input$Arcurve_title),
          legend.text = element_text(size=input$Arcurve_legend),
          axis.text = element_text(size = input$Arcurve_axistext),
          axis.title = element_text(size = input$Arcurve_axistitle),
          legend.title = element_text(size=input$Arcurve_legend)) +
    theme(legend.position = input$Arcurve_legendposition)
  
  if(input$Arcurve_colpal == "D"){plot<-plot+ scale_color_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)}
  else if (input$Arcurve_colpal == "magma"){plot<-plot+scale_color_viridis(discrete=TRUE, option = "magma") + scale_fill_viridis(discrete=TRUE, option = "magma")}
  else if (input$Arcurve_colpal == "inferno"){plot<-plot+scale_color_viridis(discrete=TRUE, option = "inferno") + scale_fill_viridis(discrete=TRUE, option = "inferno")}
  else if (input$Arcurve_colpal == "plasma"){plot<-plot+scale_color_viridis(discrete=TRUE, option = "plasma") + scale_fill_viridis(discrete=TRUE, option = "plasma")}
  else if (input$Arcurve_colpal == "cividis"){plot<-plot+scale_color_viridis(discrete=TRUE, option = "cividis") + scale_fill_viridis(discrete=TRUE, option = "cividis")}
  
  plot
  
})

#Render Plot
observe({

output$ArRareCurve <- renderPlot({
  req(ArRareCurve2)
  ArRareCurve2()
}, width = input$Arcurve_X, height = input$Arcurve_Y)
})

#######################
##Download Ar Curves###
#######################

#PNG
output$downloadARcurve <- downloadHandler(
  filename <- function() {
    paste(input$projectID, 'AllelicRichness_RarefactionCurve.png', sep = "_")
  },
  content <- function(file) {
    
    X<-as.numeric(input$Arcurve_X)
    Y<-as.numeric(input$Arcurve_Y)
    
    png(file, height = X, width =Y)
    
    print(ArRareCurve())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_ArRareCurvebutton<- renderUI({

  req(ArRareCurve)
  
  downloadButton("downloadARcurve", "Download the plot (.png)")

})

#TIFF
output$downloadARcurve_tiff <- downloadHandler(
  filename <- function() {
    paste(input$projectID, 'AllelicRichness_RarefactionCurve.tiff', sep = "_")
  },
  content <- function(file) {
    
    X<-as.numeric(input$Arcurve_X)
    Y<-as.numeric(input$Arcurve_Y)
    
    tiff(file, height = X, width =Y)
    
    print(ArRareCurve())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_ArRareCurvebutton_tiff<- renderUI({
  
  req(ArRareCurve)
  
  downloadButton("downloadARcurve_tiff", "Download the plot (.tiff)")
  
})


#######################
##Private Ar Curves####
#######################

pArRareCurve <- eventReactive(input$prarecurveGO, {

  
  genind<-genindanalysis()
  
  
  # Extract the data to use
  data.list = list()
  data.list$tab <- genind@tab
  data.list$loc.fac <- factor(genind@loc.fac)
  data.list$pop <- factor(genind@pop)
  
  # Run rarefaction analyses for a series of sample sizes to generate a rarefaction curve to plot
  min.size <- min(table(data.list$pop))
  
  if (input$parsteps == "all") {
  
  sample.size <- seq(from = 1, to = min.size) }
  
  else if (input$parsteps == "partial")  {
  
  sample.size <- seq(from = 1, to = (min.size), by = (min.size/30))
  
  }
  
  y <- NULL;
  x<-NULL;
  
  for (i in sample.size)
  {
    rarefied.values <- rarefiedMatrices(data = data.list, 
                                        g = i, 
                                        display.progress = TRUE)
    private.matrix <- as.data.frame(rarefied.values$private)
    Std<-sapply(private.matrix,function(x)sd(x, na.rm = TRUE)/sqrt(length(x)))
    Means<-colMeans(private.matrix, na.rm = TRUE)
    y<-rbind(y, Means)
    x<-cbind(x, Std)
    
  }
  y<-as.data.frame(y)
  x<-as.data.frame(t(x))
  
  
  
  pARtab<-tidyr::gather(y, Population, pAR)
  pARtab$sample_size<-sample.size
  
  pARtab<-tidyr::gather(y, Population, pAR)
  x<-tidyr::gather(x, Population, SE)
  pARtab$SE <- x$SE
  pARtab$sample_size<-sample.size
  
  pARtab
})

pArRareCurve2<-reactive({
  
  req(pArRareCurve)
  
  pARtab<-pArRareCurve()
  
  ggplot(data=pARtab, mapping = aes(sample_size, pAR, color=Population, group=Population))+
    geom_point(size=1.5)+
    geom_line(size=1.5)+
    scale_alpha(guide = "none") +
    geom_errorbar(aes(ymin = pAR-pARtab$SE, ymax = pAR + pARtab$SE, alpha = 0.25)) +  
    scale_color_viridis(discrete=TRUE) +
    labs(x="Sample size", y="Mean private allelic richness (pAR)", title="Rarefaction curves for private allelic richness", color="Population")+
    theme_bw()+
    theme(title = element_text(size = input$pArcurve_title),
           legend.text = element_text(size=input$pArcurve_legend),
           axis.text = element_text(size = input$pArcurve_axistext),
           axis.title = element_text(size = input$pArcurve_axistitle),
           legend.title = element_text(size=input$pArcurve_legend)) +
    theme(legend.position = input$pArcurve_legendposition)
  
})

observe({

output$pArRareCurve <- renderPlot({
  
  req(pArRareCurve2)
  
  prare <- pArRareCurve2()
  prare
  
}, width = input$pArcurve_X, height = input$pArcurve_Y)

})



#######################
##Download pAr Curves###
#######################

#PNG
output$downloadpArcurve <- downloadHandler(
  filename <- function() {
    paste(input$projectID, 'PrivateAllelicRichness_RarefactionCurve.png', sep = "_")
  },
  content <- function(file) {
    
    X<-as.numeric(input$pArcurve_X)
    Y<-as.numeric(input$pArcurve_Y)
    
    png(file, height = X, width =Y)
    
    print(ArRareCurve())
    
    dev.off()
  },
  contentType = "image/png"
)

output$DL_pArcurve_button<- renderUI({
  
  req(ArRareCurve)
  
  downloadButton("downloadpArcurve", "Download the plot (.png)")
  
})

#TIFF
output$downloadpArcurve_tiff <- downloadHandler(
  filename <- function() {
    paste(input$projectID, 'PrivateAllelicRichness_RarefactionCurve.tiff', sep = "_")
  },
  content <- function(file) {
    
    X<-as.numeric(input$pArcurve_X)
    Y<-as.numeric(input$pArcurve_Y)
    
    tiff(file, height = X, width =Y)
    
    print(ArRareCurve())
    
    dev.off()
  },
  contentType = "image/tiff"
)

output$DL_pArcurve_button_tiff<- renderUI({
  
  req(ArRareCurve)
  
  downloadButton("downloadpArcurve_tiff", "Download the plot (.tiff)")
  
})




















#vgam Calculation

vgamtab<- reactive({

genind <-genindanalysis()

vgam=NULL;
for (i in unique(genind$pop))
{
  temp<-Vgamcalc(genind, population = i)
  vgam=rbind(vgam,temp)
}
vgam<-as.data.frame(vgam)
rownames(vgam)=unique(genind$pop)
names(vgam)="vgam"
vgam$pop=rownames(vgam)
vgam

})

#LGP Calculation

lgptab<- reactive({
  
  genind <-genindanalysis()
  
  LGP=NULL;
  for (i in unique(genind$pop))
  {
    temp<-LGPcalc(genind, population = i)
    LGP=rbind(LGP,temp)
  }
  LGP<-as.data.frame(LGP)
  rownames(LGP)=unique(genind$pop)
  names(LGP)="LGP"
  LGP$pop=rownames(LGP)
  LGP
  
})

#Combined Table

mult.tab2<-reactive ({
  
  req(lgptab)
  
  LGP<-lgptab()
  vgam<-vgamtab()
  
  final=Reduce(function(x,y) merge(x =x, y = y, by = "pop"), 
               list(LGP, vgam))
  
  
})


output$mult.tab2 <- renderDataTable(mult.tab2(), extensions = 'Buttons', 
                                   options = list(dom = 'Bfrtip',
                                                  buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_GeneticMultiplicity_Table2")), list(extend='excel', filename= paste(input$projectID, "_GeneticMultiplicity_Table2")) )),
                                   caption = "Summary of descriptive genetic multiplicity statistics per population. Statistics include latent genetic potential (LGP) and hypothetical multilocus gametic diversity (vgam).",
                                   rownames=FALSE
)


