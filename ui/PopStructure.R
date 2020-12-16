
        navbarPage("Genetic Structure",
                   source(file.path("ui", "PopStructure_AMOVA.R"),  local = TRUE)$value,
                   
                   source(file.path("ui", "PopStructure_Pairwisecomparisons.R"),  local = TRUE)$value,
                   
                   source(file.path("ui", "PopStructure_snapclustdapc.R"),  local = TRUE)$value,
                   
                   source(file.path("ui", "PopStructure_StructureAnalysis.R"),  local = TRUE)$value
                           
                            #end of tabpanel 'snapclust + DAPC'
        ) #End of Navpage
