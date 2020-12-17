# easyRpopgen

Repository for all files associated with the R-Shiny app 'easyRpopgen"

#### **Run easyRpopgen from Github**

```{r, eval = FALSE}

##########################
####Environment Set-Up####
##########################

#To install packages from Bioconductor you need BiocManager
if (!require('BiocManager'))
  install.packages("BiocManager")
library(BiocManager)

#To install packages from GitHub you need devtools
if (!require('devtools'))
  install.packages("devtools")
library(devtools)

#CRAN packages
packages_CRAN = c("dartR","dplyr", "adegenet", "shinyjs", "shinycssloaders", "poppr", "adegenet", "shinydashboard", "shiny", "viridis", "ade4", "dartR", "gdata", "DescTools", "pegas","matrixStats", "mmod", "genetics", "PopGenReport", "ggplot2", "ggExtra", "ggpubr", "data.table", "formattable", "shinyWidgets", "pophelper", "DT", "tidyr", "soc.ca", "hierfstat") #List of packages

package.check <- lapply( #Checks whether packages are installed and if not it installs them
  packages_CRAN,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#Bioconductor Packages
if (!require('SNPRelate')) 
  BiocManager::install('SNPRelate')
if (!require('qvalue')) 
  BiocManager::install('qvalue')

#GitHub Package
devtools::install_github('royfrancis/pophelper')

##############################################
####Run the Shiny App from this Repository####
##############################################

runGitHub("easyRpopgen", "pbraileyjones", refs = "main")

```
