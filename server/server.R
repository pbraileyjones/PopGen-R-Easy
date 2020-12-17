#Define server logic

server <- function(input, output,session) {
  
  source(file.path("server", "DataImport_server.R"),  local = TRUE)$value
  
  source(file.path("server", "DataFilter_server.R"),  local = TRUE)$value
  
  source(file.path("server", "Diversity_server.R"),  local = TRUE)$value
  
  source(file.path("server", "GeneticVariation_stratadefinitions.R"),  local = TRUE)$value
  
  source(file.path("server", "Multiplicity_server.R"),  local = TRUE)$value
  
  source(file.path("server", "PopulationStructure_server.R"),  local = TRUE)$value
  
  source(file.path("server", "Ordinations_server.R"),  local = TRUE)$value

} 

#END OF SERVER

