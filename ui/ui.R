# Define UI for data upload app 

sidebar<-dashboardSidebar(
  hr(), #line ----
  sidebarMenu (id = "tabs",
               menuItem ("Data Import", tabName = "imp", icon = icon ("file-signature")),
               menuItem ("Data Filtering", tabName = "filt", icon = icon("filter")),
               menuItem ("Genetic Diversity", tabName = "div", icon = icon ("dna")),
               menuItem ("Population Structure", tabName = "gst", icon = icon ("globe-africa")),
               menuItem ("Ordinations", tabName = "ordinations", icon = icon ("project-diagram")),
               menuItem ("Codes", icon = icon("file-text-o"),
                         menuSubItem("global.R", tabName = "global", icon = icon("angle-right")),
                         menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                         menuSubItem("server.R", tabName = "server", icon = icon ("angle-right"))
               ),
               menuItem ("ReadMe", tabName = "readme", icon = icon("mortar-board")),
               menuItem ("About", tabName = "about", icon = icon("question"))
  ), #END OF 'sidebarMenu'
  
  hr() #line----

  ) #END OF 'sidebar'

body <-dashboardBody(
  
  #options(spinner.color="#00a65b"), #Color and size setting for all loading plot spinners
  #options(spinner.size = 2.5),
  
  tabItems(
    
    #DATA IMPORT R FILE
    tabItem (tabName = "imp", #DATA FILTERING
    source(file.path("ui/DataImport.R"),  local = TRUE)$value,
    ),
    
    #DATA FILTERING R FILE
    tabItem (tabName = "filt", #DATA FILTERING
             source(file.path("ui/DataFilter.R"),  local = TRUE)$value,
    ),
    
    #GENETIC VARIATION R FILE
    tabItem (tabName = "div", #DATA FILTERING
             source(file.path("ui/GeneticVariation.R"),  local = TRUE)$value,
    ),
    
    #POPULATION STRUCTURE R FILE
    tabItem (tabName = "gst", #DATA FILTERING
             source(file.path("ui/PopStructure.R"),  local = TRUE)$value,
    ),
    
    #ORDINATION R FILE
    tabItem (tabName = "ordinations", #DATA FILTERING
             source(file.path("ui/Ordinations.R"),  local = TRUE)$value,
    ),
    
    #ORDINATION R FILE
    tabItem (tabName = "about", #ABOUT
             source(file.path("ui/About.R"),  local = TRUE)$value,
    )#
    
  )#END OF 'tabItems'
  
) #END OF 'DashboardBody'


dbHeader<-dashboardHeader(title = "easyRpopgen")

ui<-dashboardPage(skin = "green",
                  dbHeader,
                  sidebar,
                  body)



