#######################################
#######################################
####Pairwise population comparisons####
#######################################
#######################################

#Bootstrapped 95 % confidence intervals can be used to distinguish Fst values between populations.
#A lower confidence interval above zero indicates that the two populations are likely divergent
#A lower confidence interval below zero indicates that the two populations are similar to one another

#####################################
####FST Distance + Bootstrapping#####
#####################################

#Calculate pairwise Fst values and bootstrap confidence intervals
pwfst_tab <- eventReactive(input$pwfstGO,{
  
  genind<-genind_filt()

#Calculate distances
mat<-genet.dist(genind, diploid = TRUE, method = "WC84")
#Turn dist object in to a matrix object
mat<-as.matrix(mat)


#Bootstrap confidence intervals
temp<-boot.ppfst(genind, nboot = 999)
#Add bootstrap intervals to the distance matrix
upperTriangle(mat)<- paste(round(upperTriangle(temp$ll),5), "-", round(upperTriangle(temp$ul), 5))
#Create data frame for DT visualization and download
as.data.frame(mat)


}) #end of pwfst

#Output data table of pairwise Fst values and confidence intervals
output$pwfst_tab <- renderDataTable(pwfst_tab(), extensions = 'Buttons', 
                                 options = list(dom = 'Bfrtip',
                                                buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_PairwiseFSTComparisons")), list(extend='excel', filename= paste(input$projectID, "_PairwiseFSTComparisons")) )),
                                 caption = "Pairwise comparisons of pairwise Fst between populations. Pairwise Fst values are found in the lower triangle, and the upper and lower bounds of bootstrapped (10,000 permutations) confidence intervals are found in the upper triangle. Pairwise confidence intervals which overlap with zero are indicative that populations are statistically similar to one another",
                                 rownames=FALSE
)

#Create dendogram / phylogenetic tree based on genetic distances
pwfst.tree<-reactive({
  
  
  genind<-genind_filt()
  
  #Function from: https://gist.github.com/zkamvar/22ee313c7351cd03cd90b913bf3ad46a
  fungen <- function(pop_column) {
    force(pop_column)
    function(x) {
      dat <- cbind(pop_column, x)
      res <- nj(genet.dist(dat, method = "WC84"))
      res$tip.label <- levels(dat[[1]])
      res
    }
  }
  
  n<-genind2hierfstat(genind)
  genet.tree_pop <- fungen(n[1])
  
  phylo_tree <- genet.tree_pop(n[-1])
  plot(phylo_tree,
       main = "Pairwise Fst",
       sub = "Neighbour-joining tree of pairwise Fst values.\nThe scale of distances is given below the tree.")
  axisPhylo()
  
  
})

#Render Fst dendogram / phylo tree


observe({
output$pwfts.tree<-renderPlot({
  
  req(pwfst.tree)
  
  pwfst.tree()
  
}, height = 400, width = 600)
})

################################
####Other genetic distances#####
################################

#There is no bootstrapping in this implementation- there may be in a future update but Fst is more commonly used

#Generic function to calculate pairwise genetic distances available in the 'hierfstat' package based on the users choice
pwd<-reactive({
  
  genind<-genind_filt()
  
  Ds=as.data.frame(as.matrix(hierfstat::genet.dist(genind, method=input$gendist_other)))
  row.names(Ds)=unique(genind$pop)
  names(Ds)=unique(genind$pop)
  gdata::upperTriangle(Ds)<-NA
  Ds  
  
}) #End of pwd

#Output data table of pairwise genetic distances and confidence intervals
output$pwd <- renderDataTable(pwd(), extensions = 'Buttons', 
                                options = list(dom = 'Bfrtip',
                                               buttons = list('copy', list(extend='csv', filename = paste(input$projectID,"_Pairwise_",input$gendist_other,"_Comparisons")), list(extend='excel', filename= paste(input$projectID,"_Pairwise",input$gendist_other,"Comparisons")) )),
                                caption = paste("Pairwise comparisons of ", input$gendist_other, " distance between populations" ),
                                rownames=TRUE
                              )

#Create dendogram / phylogenetic tree based on genetic distances
ds.tree<- reactive({
  
  req(pwd)
  
  genind <- genind_filt()
  
  mat<-hierfstat::genet.dist(genind, diploid = TRUE, method = input$gendist_other  )
  
  ds.tree<-nj(mat)
  tip.label<-unique(genind$pop)
  ds.tree$tip.label<-as.character(tip.label)
  plot(ds.tree,
       main = "Pairwise Ds",
       sub = "Neighbour-joining tree of pairwise Ds values.\nThe scale of distances is given below the tree.")
  axisPhylo()
  
  
})

#Render dendogram / phylo tree
output$ds.tree <- renderPlot({
  
  ds.tree<-ds.tree()
  
}, height = 400, width = 600)