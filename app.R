#########################
####Environment SetUp####
#########################

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
#packages_BioC = c("SNPRelate", )

#packages_CRAN = c("dplyr", "adegenet", "shinyjs", "shinycssloaders", "poppr" )

## Now load or install&load all
#package.check <- lapply(
#  packages,
#  FUN = function(x) {
#    if (!require(x, character.only = TRUE)) {
#      install.packages(x, dependencies = TRUE)
#      library(x, character.only = TRUE)
#    }
#  }
#)


#######GLOBAL.R

###Package install

#BiocManager::install("SNPRelate")
#install.packages("formattable")
#install.packages("data.table")
#install.packages("shinyjs")
#devtools::install_github('andrewsali/shinycssloaders')
#install.packages("poppr")
#install.packages("adegenet")
#install.packages("shinydashboard")
#install.packages("shiny")
#install.packages("plotly")
#install.packages("dplyr")
#install.packages("viridis")
#install.packages("ade4")
#install.packages("dartR")
#install.packages("radiator")
#install.packages("gdata")
#install.packages("DescTools")
#install.packages("multcompView")
#instll.packages("pegas")
#install.packages("matrixStats")
#install.packages("mmod")
#install.packages("PopGenReport")
#install.packages("ggplot2")
#install.packages("shinyWidgets")
#install.packages("formattable")
#install.packages("pophelper")
#install.packages("adegenet")
#install.packages("poppr")
#install.packages("soc.ca")
#install.packages("radiant.data")
#install.packages("pcadapt")
#install.packages("periscope")


##dartR install
#################PYTHON
#################BiocManager::install("SNPRelate")
#################BiocManager::install("qvalue")
#################install.packages("dartR")

#radiator install
#################devtools::install_github("thierrygosselin/radiator")

#pophelper install
##################devtools::install_github('royfrancis/pophelper')



###Access libraries
library(SNPRelate)
library(dplyr)
library(adegenet)
library(shinyjs)
library(shinycssloaders)
library(poppr)
library(adegenet)
library(shinydashboard)
library(shiny)
library(plotly)
library(dplyr)
library(viridis)
library(ade4)
library(dartR)
library(gdata)
library(DescTools)
library(multcompView)
library(pegas)
library(matrixStats)
library(mmod)
library(genetics)
library(PopGenReport)
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(data.table)
library(formattable)
library("shinyWidgets")
library(pophelper)
library(DT)
library(tidyr)
library(pophelper)
#install.packages("soc.ca")
library(soc.ca)
#install.packages("radiant.data")
library(radiant.data)
#library(pcadapt)
#install.packages("periscope")
library(periscope)
library(hierfstat)



#Custom Scripts

#Na alleles
Na=function (x, population)
{
  sub=poppr::popsub(x, population, drop=TRUE)
  sum=summary(sub)
  sum$loc.n.all
}


#LGP
LGPcalc=function (x, population = "ALL")
{
  z = poppr::popsub(x, population, drop = TRUE) #seperates genind by population
  sum=summary(z)  #generates summary table of genind object including measures of 'number of alleles' and 'expected heterozygosity' which are needed to calculate LGP
  q=(1/(1-sum$Hexp))  #Calculate number of effective alleles
  LGP=(sum(sum$loc.n.all-q)) #calculate LGP
}

#vgam
Vgamcalc=function (x, population = "ALL") #Function to calculate vgam from a genind object
{
  z=poppr::popsub(x, sublist = population, drop = TRUE) #subset genind by population
  sum=summary(z) #generate summary statistics including expected heterozygosity
  q=as.matrix(1/(1-sum$Hexp)) #calculate effective number of alleles from expected heterozygosity
  vgam=colProds(q,method = c("direct")) #calculate hypothetical gametic multi-locus diversity as product of all Aes across all loci
  vgam
}



###Script from vegan package
#Create the function 'veganCovEllipse'
#This is already a hidden function in the vegan package, but is needed to be performed as it's own 'thing' here
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  Q <- chol(cov, pivot = TRUE)
  o <- attr(Q, "pivot")
  t(center + scale * t(Circle %*% Q[, o]))
}



# Function to export to STRUCTURE format from genind object.
# genind objects are created in the R package adegenet.  The function below is an R function.
# Lindsay V. Clark, 26 July 2015

# obj: genind object
# file: file name to write
# pops: whether to include population info in the file
# Function is flexible with regards to ploidy, although genotypes are
# considered to be unambiguous.
# Missing data must be recorded as NA in obj@tab.

# example use: 
# data(nancycats)
# genind2structure(nancycats, file="nancy_structure.txt", pops=TRUE)

genind2structure <- function(obj, file="", pops=FALSE){
  if(!"genind" %in% class(obj)){
    warning("Function was designed for genind objects.")
  }
  
  # get the max ploidy of the dataset
  pl <- max(obj@ploidy)
  # get the number of individuals
  S <- adegenet::nInd(obj)
  # column of individual names to write; set up data.frame
  tab <- data.frame(ind=rep(indNames(obj), each=pl))
  # column of pop ids to write
  if(pops){
    popnums <- 1:adegenet::nPop(obj)
    names(popnums) <- as.character(unique(adegenet::pop(obj)))
    popcol <- rep(popnums[as.character(adegenet::pop(obj))], each=pl)
    tab <- cbind(tab, data.frame(pop=popcol))
  }
  loci <- adegenet::locNames(obj) 
  # add columns for genotypes
  tab <- cbind(tab, matrix(-9, nrow=dim(tab)[1], ncol=adegenet::nLoc(obj),
                           dimnames=list(NULL,loci)))
  
  # begin going through loci
  for(L in loci){
    thesegen <- obj@tab[,grep(paste("^", L, "\\.", sep=""), 
                              dimnames(obj@tab)[[2]]), 
                        drop = FALSE] # genotypes by locus
    al <- 1:dim(thesegen)[2] # numbered alleles
    for(s in 1:S){
      if(all(!is.na(thesegen[s,]))){
        tabrows <- (1:dim(tab)[1])[tab[[1]] == indNames(obj)[s]] # index of rows in output to write to
        tabrows <- tabrows[1:sum(thesegen[s,])] # subset if this is lower ploidy than max ploidy
        tab[tabrows,L] <- rep(al, times = thesegen[s,])
      }
    }
  }
  
  # export table
  write.table(tab, file=file, sep="\t", quote=FALSE, row.names=FALSE)
}


#from: https://gist.github.com/psychemedia/150cb9901529da58124a
numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}






###Scripts to be used that are not part of packages


# Private allele rarefaction functions
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2017-05-16

################################################################################
# SUMMARY
# Kalinowski's algorithms for correcting for uneven sampling to estimate allele 
# richness and private allele frequency

################################################################################
# Probability of allele absence in rarefied sample
# @param N.col vector of allele counts for a single population
# @param g gene sample size; generally the number of haploids or twice the 
#   number of diploid individuals in the smallest population sample size
# @description Calculates the probability of not observing each allele in a 
#   given population for a rarefied sample of size \code{g}
# @return vector of probabilities for single population
# @examples 
# N.matrix <- matrix(data = c(3, 6, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# # Allele probabilities for population 1 given sample of 2 genes
# calcQ.v(N.col = N.matrix[, 1], g = 2)
# # Allele probabilities for population 2 given sample of 4 genes
# calcQ.v(N.col = N.matrix[, 2], g = 4)
calcQ.v <- function(N.col, g) {
  if(length(N.col) < 2 || sum(N.col) == 0 || sum(N.col) < g) {
    return(rep(x = NA, times = length(N.col)))
  }
  Nj <- sum(N.col)
  product <- rep(x = 1, times = length(N.col))
  u <- 0
  
  # There is a quicker way to do this, given:
  # For each allele, the numerator is an integer range: 
  # (Nj - Nij) to (Nj - Nij - (g - 1))
  # And the corresponding denominator range (identical across alleles):
  # (Nj) to (Nj - (g - 1))
  # It is easy to create the vector of denominators:
  #   denominators <- seq(from = Nj, to = Nj - (g - 1))
  # But numerators will need to be a matrix...
  
  while (u < g && sum(product) > 0) {
    numerator <- Nj - N.col - u
    denominator <- Nj - u
    product <- product * (numerator / denominator)
    u <- u + 1
  }
  return(product)
}

################################################################################
# Probability of allele presence in rarefied sample
# @param N.col vector of allele counts for a single population
# @param g gene sample size; generally the number of haploids or twice the 
#   number of diploid individuals in the smallest population sample size
# @description Calculates the probability of observing each allele in a given 
#   population for a rarefied sample of size \code{g}
# @return vector of probabilities for single population
# @examples 
# N.matrix <- matrix(data = c(3, 6, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# # Allele probabilities for population 1 given sample of 2 genes
# calcP.v(N.col = N.matrix[, 1], g = 2)
# # Allele probabilities for population 2 given sample of 4 genes
# calcP.v(N.col = N.matrix[, 2], g = 4)
calcP.v <- function(N.col, g) {
  prob <- 1 - calcQ.v(N.col = N.col, g = g)
  return(prob)
}

################################################################################
# Rarefied allele richness for a single population
# @param N.col vector of allele counts for a single population
# @param g gene sample size; generally the number of haploids or twice the 
#   number of diploid individuals in the smallest population sample size
# @description Calculates allele richness for a rarefied sample of size 
#   \code{g}
# @return Allele richness in a single population rarefied to size \code{g}
# @examples 
# N.matrix <- matrix(data = c(3, 6, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# # Allele richness for population 1 given sample of 2 genes
# calcRichness.all(N.col = N.matrix[, 1], g = 2)
# # Allele richness for population 2 given sample of 4 genes
# calcRichness.all(N.col = N.matrix[, 2], g = 4)
calcRichness <- function(N.col, g) {
  sum.richness <- sum(calcP.v(N.col = N.col, g = g))
  return(sum.richness)
}

################################################################################
# Rarefied allele richness for each population
# @param N matrix of allele counts for all populations, with alleles as rows 
#   and populations as columns
# @param g gene sample size; generally the number of haploids or twice the 
#   number of diploid individuals in the smallest population sample size
# @description Calculates allele richness for a rarefied sample of size 
#   \code{g}
# @return A vector of allele richness, where each element is the richness for 
#   a single population indexed by columns in \code{N}
# @examples 
# N.matrix <- matrix(data = c(3, 6, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# # Allele richness for all populations given sample of 4 genes
# calcRichness.all(N = N.matrix, g = 4)
calcRichness.all <- function(N, g) {
  all.rich <- apply(X = N, MARGIN = 2, FUN = function(x) {calcRichness(N.col = x, g = g)})
  # Preserve population names if they exist in N
  names(all.rich) <- colnames(N)
  return(all.rich)
}

################################################################################
# Rarefied private allele count for single population
# @param N matrix of allele counts for all populations, with alleles as rows 
#   and populations as columns
# @param g gene sample size; generally the number of haploids or twice the 
#   number of diploid individuals in the smallest population sample size
# @param j index of the population for which to perform calculations
# @description Calculates the expected number of private alleles in population
# \code{j} given a sample of \code{g} genes
# @return A vector of length 1, with the expected number of private alleles for
# the given population
# @examples 
# N.matrix <- matrix(data = c(3, 6, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# # Private alleles in population 1, for a sample size of 2 genes
# calcPrivate(N = N.matrix, g = 2, j = 1)
# # Private alleles in population 2, for a sample size of 4 genes
# calcPrivate(N = N.matrix, g = 4, j = 2)
calcPrivate <- function(N, g, j) {
  # TODO: update to mirror calcPrivate.all treatment of populations with 
  # sampling below g; will affect *all* populations if any one population
  # has fewer than g genes sampled
  # sum over all alleles
  # Pijg * [(]prod from j'=1 to J, j' != j (Qij'g)]
  m <- nrow(N)
  private.sum <- 0
  Pijg <- calcP.v(N.col = N[, j], g = g) # a vector of allele probabilities
  
  # Now we need a vector of the Q products
  Q.matrix <- apply(X = N, MARGIN = 2, FUN = function(x) {calcQ.v(N.col = x, g = g)})
  Q.matrix[, j] <- 1 # Dummy coding, so product of row is unaffected for column j
  Q.products <- apply(X = Q.matrix, MARGIN = 1, FUN = prod)
  
  # Have vector Pijg and vector of product Qij'g
  # Multiply corresponding elements in each, then sum result
  pi.hat <- sum(Pijg * Q.products)
  return(pi.hat)
}

################################################################################
# Rarefied private allele count for all populations
# @param N matrix of allele counts for all populations, with alleles as rows 
#   and populations as columns
# @param g gene sample size; generally the number of haploids or twice the 
#   number of diploid individuals in the smallest population sample size
# @description Calculates the expected number of private alleles for a sample 
# of \code{g} genes
# @return A vector of \code{nrow(N)}, with the expected number of private 
# alleles for each population
# @examples 
# N.matrix <- matrix(data = c(3, 6, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# # Private alleles for sample of 2 genes
# calcPrivate.all(N = N.matrix, g = 2)
calcPrivate.all <- function(N, g) {
  # Start by reducing matrix to only those populations with at least g sampled 
  # genes
  cols.keep <- which(colSums(x = N, na.rm = TRUE) >= g)
  N.analyze <- N[, cols.keep]
  private.alleles.return <- rep(NA, times = ncol(N))
  
  if (is.matrix(N.analyze) && dim(N.analyze)[1] > 1) {
    # Both P and Q matrix have alleles in rows, and populations in columns
    # P114 P124 P134 P144
    # P214 P224 P234 P244
    # P314 P324 P334 P344
    # P414 P424 P434 P444
    P.matrix <- apply(X = N.analyze, MARGIN = 2, FUN = function(x) {calcP.v(N.col = x, g = g)})
    Q.matrix <- apply(X = N.analyze, MARGIN = 2, FUN = function(x) {calcQ.v(N.col = x, g = g)})
    
    # Private allele calculation:
    # For population 1, it is the sum of (three alleles, four populations):
    # P114 x (Q124 x Q134 x Q144)
    # P214 x (Q224 x Q234 x Q244)
    # P314 x (Q324 x Q334 x Q344)
    # 
    # For population 2, it is the sum of (three alleles, four populations):
    # P124 x (Q114 x Q134 x Q144)
    # P224 x (Q214 x Q234 x Q244)
    # P324 x (Q314 x Q334 x Q344)
    
    # private allele count vector; each element corresponds to a population
    private.alleles <- numeric(ncol(N.analyze))
    for (j in 1:ncol(N.analyze)) {
      corrected.Q.matrix <- Q.matrix
      corrected.Q.matrix[, j] <- 1 # Dummy coding, so product of row is unaffected for column j
      Q.products <- apply(X = corrected.Q.matrix, MARGIN = 1, FUN = prod)
      private.alleles[j] <- sum(P.matrix[, j] * Q.products)
    }
    
    # Now we need a vector of length equivalent to the original number of columns 
    # in N, including those with too few sampled genes
    private.alleles.return[cols.keep] <- private.alleles
    
  }  
  # Preserve population names if they exist in N
  names(private.alleles.return) <- colnames(x = N)
  return(private.alleles.return)
}


################################################################################
# Private allele and allele richness for all loci and all populations
# @param data list with information on allele counts; see 'Details'
# @param g gene sample size; generally the number of haploids or twice the 
#   number of diploid individuals in the smallest population sample size
# @param display.progress logical indicating whether or not to display a 
#   progress indicator bar
# @description Calculates the expected number of alleles and private alleles 
#   for a sample of \code{g} genes
# @return A list of two matrix elements: \code{richness}, which contains the 
#   estimated number of alleles in a rarefied sample for each population at 
#   each locus and \code{private}, which contains the estimated number of 
#   private alleles in a rarefied sample for each population at each locus
# @details Calculations are designed to act on the list \code{data}, which 
#   should have the following three elements: \code{tab}, a data frame of 
#   allele counts, most likely this will be the \code{@tab} element from an 
#   \code{adegenet::genind} object; \code{loc.fac}, a vector of type factor 
#   with the locus level for each column in \code{data$tab}; and \code{pop}, 
#   a vector of type factor with the population level for each row in 
#   \code{data$tab}
# @examples 
# \dontrun{
# # Use subset of data from genind object `genind.data`
# test.data = list()
# test.data$tab <- genind.data@tab[pop.rows, 1:20]
# test.data$loc.fac <- factor(genind.data@loc.fac[1:20])
# test.data$pop <- factor(genind.data@pop[pop.rows])
# rarefied.values <- rarefiedMatrices(data = test.data, g = 10, display.progress = TRUE)
# }

rarefiedMatrices <- function(data, g = 2, display.progress = FALSE) {
  
  # Establish matrices that will hold the final results
  richness.matrix <- matrix(data = 0, 
                            nrow = length(levels(data$loc.fac)),
                            ncol = length(levels(data$pop)))
  
  private.matrix <- matrix(data = 0, 
                           nrow = length(levels(data$loc.fac)),
                           ncol = length(levels(data$pop)))
  
  colnames(richness.matrix) <- colnames(private.matrix) <- as.character(levels(data$pop))
  rownames(richness.matrix) <- rownames(private.matrix) <- as.character(levels(data$loc.fac))
  
  progress.bar <- NULL
  if (display.progress) {
    progress.bar <- txtProgressBar(min = 1, max = length(levels(data$loc.fac)), style = 3)
  }
  # Loop over each locus, doing richness and private calculations for each
  for (locus.index in 1:length(levels(data$loc.fac))){
    if (display.progress) {
      setTxtProgressBar(pb = progress.bar, value = locus.index)
    }
    
    # Extract the name of the current locus
    locus.id <- levels(data$loc.fac)[locus.index]
    # Subset data for that one locus
    locus.data <- as.data.frame(data$tab[, data$loc.fac == locus.id])
    # TODO: This needs only happen once?
    locus.data$pop <- data$pop
    
    # Do the allele counts for the locus
    allele.counts <- locus.data %>% 
      dplyr::group_by(pop) %>% 
      dplyr::summarize_all(funs(sum), na.rm = TRUE)
    
    # Pull out counts (first column is pop id)
    count.matrix <- as.matrix(allele.counts[, c(2:ncol(allele.counts))])
    
    # Set row names from values of pop (odd syntax to extract one column 
    # from the allele.counts tibble)
    rownames(count.matrix) <- as.character(allele.counts[[1]])
    colnames(count.matrix) <- gsub(pattern = paste0(as.character(locus.id), "."),
                                   replacement = "", 
                                   x = colnames(count.matrix))
    
    # Transpose for appropriate allele x pop format needed by functions
    N.matrix <- t(count.matrix)
    richness.matrix[locus.index, ] <- calcRichness.all(N = N.matrix, g = g)
    private.matrix[locus.index, ] <- calcPrivate.all(N = N.matrix, g = g)
  }
  if (!is.null(progress.bar)) {
    close(progress.bar)
  }
  return(list(richness = richness.matrix, private = private.matrix))
}



#Advanced Shiny Features

appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

#RUN SHINY APP

ui<- source(file.path("ui", "ui.R"),  local = TRUE)$value

server<- source(file.path("server", "server.R"),  local = TRUE)$value

shinyApp(ui, server)

