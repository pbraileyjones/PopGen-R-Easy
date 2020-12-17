# ***easyRpopgen* v 1.0**

### **Introduction to *easyRpopgen***

*easyRpopgen* is a Shiny application written using the open-source R coding language, through which users can interact with a graphical user interface to carry out a comprehensive suite of fundamental population genetics analyses and generate publication quality figures based around this. The computational core underlying the interface combines the functionalities of multiple pre-existing R packages, publicly available functions and novel functions written by the authors. The primary objective of the application is to provide a seamless user interface for those unfamiliar with R. This application is also intended to serve as a learning tool for new users, and as a starting point R-based analysis pipeline for more experienced users. Users can access  the whole application script, or view simplified snippets of script intended to capture the underlying logic of each step, which they can use to recreate the application results and expand upon outside of the application.

### **Installation**

To run *easyRpopgen* you will need to have a version of [RStudio](https://rstudio.com/products/rstudio/download/#download) and [R](https://cran.r-project.org/src/base/R-4/) on your computer. The app was developed using R v 4.02 and RStudio v 1.3.1093. Once you have RStudio set up, you simply need to copy the following script and run it in your console. It will check for the necessary R packages needed to run the computational core of the Shiny Application and download the ones it needs if they are not already present ("Environment Set-Up"). It will then download the Shiny app repository and open this with the *RunGitHub()* function.

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
packages_CRAN = c("dartR","dplyr", "adegenet", "shinyjs", "shinycssloaders", "poppr", "adegenet", "shinydashboard",
"shiny", "viridis", "ade4", "dartR", "gdata", "DescTools", "pegas","matrixStats", "mmod", "genetics", "PopGenReport",
"ggplot2", "ggExtra", "ggpubr", "data.table", "formattable", "shinyWidgets", "pophelper", "DT", "tidyr", "soc.ca",
"hierfstat") #List of packages

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

### **Application Overview**

The application is split in to five sections:

- Data Import

- Data Filtering

- Genetic Variation

- Population Structure

- Ordinations

### **Data Input**
