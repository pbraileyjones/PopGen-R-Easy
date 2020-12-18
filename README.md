# ***PopGen-R-Easy* v 1.0**

### **Introduction to *easyRpopgen***

**Population Genetics is EASY with PopGen-R-Easy!!**

*PopGen-R-Easy* is a Shiny application written using the open-source R coding language, through which users can interact with a graphical user interface to carry out a comprehensive suite of fundamental population genetics analyses and generate publication quality figures based around this. The computational core underlying the interface combines the functionalities of multiple pre-existing R packages, publicly available functions and novel functions written by the authors. The primary objective of the application is to provide a seamless user interface for those unfamiliar with R. This application is also intended to serve as a learning tool for new users, and as a starting point R-based analysis pipeline for more experienced users. Users can access  the whole application script, or view simplified snippets of script intended to capture the underlying logic of each step, which they can use to recreate the application results and expand upon outside of the application.

This application was developed as part of the EU funded [*LIFEGENMON* project](http://www.lifegenmon.si/) involving [six partners](http://www.lifegenmon.si/partnerspeople/). (Slovenian Forestry Institute, Slovenia Forest Service, CNVOS, ASP, Aristotle University of Thessaloniki and GDDAY-DAMT) from three countries (Germany, Slovenia, and Greece). It therefore provides the means to calculate a majority of verifiers of genetic maintenance used in the forest genetic monitoring regime designed by the project. We hope that you also find this application useful in your own population genetics research.

### **Installation**

To run *easyRpopgen* you will need to have a version of [RStudio](https://rstudio.com/products/rstudio/download/#download) and [R](https://cran.r-project.org/src/base/R-4/) on your computer. The app was developed using R v 4.02 and RStudio v 1.3.1093. Once you have RStudio set up, you simply need to copy the following script and run it in your console. This will then download the Shiny app repository and open this with the *RunGitHub()* function. The application R script (*App.R*) will check that your R environment has all of the needed packages installed and loaded, and do this for you if not.

```{r, eval = FALSE}

install.packages("shiny")
library("shiny")

runGitHub("easyRpopgen", "pbraileyjones", ref = "main")

```

### **Application Overview**

The application is split in to five sections:

- Data Import

- Data Filtering

- Genetic Variation

- Population Structure

- Ordinations

### **Data Input**

---

## **Acknowlegments**

### **App Creators and Contributors** 

**Phil Brailey-Jones pbraileyjones@gmail.com (contact for any queries directly related to the application**

Rok Damjanic rok.damjanic@gozdis.si

Natasa Sibanc natasa.sibanc@gozdis.si

Marjana Westergren marjana.westergren@gozdis.si

Marko Bajc marko.bajc@gozdis.si

### **LifeGenMon Project Coordinator**

Hojka Kraigher hojka.kraigher@gozdis.si 
