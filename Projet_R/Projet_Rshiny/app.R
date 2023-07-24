

# Chargement des bibliothèques nécessaires
library(shiny)         # Pour créer l'application Shiny
library(leaflet)       # Pour créer des cartes interactives
library(sf)            # Pour gérer des données spatiales
library(rnaturalearth) # Pour obtenir les données géographiques du monde
library(dbscan)        # Pour effectuer la clusterisation spatiale
library(cluster)       # Pour l'analyse de clusters
library(ggplot2)       # Pour créer des visualisations de données
library(shinythemes)   # Pour appliquer un thème à l'application Shiny

# Chargement des données depuis le fichier CSV
ACLED_Western_Africa <- read.csv("ACLED-Western_Africa.csv")

# Clusterisation des données
clusters <- dbscan(ACLED_Western_Africa[, c("latitude", "longitude")], eps = 0.1, minPts = 5)
ACLED_Western_Africa$cluster <- as.factor(clusters$cluster)

# UI
ui <- fluidPage(
  # Appliquer un thème à l'application
  theme = shinytheme("superhero"),  # Vous pouvez remplacer "superhero" par un autre thème de votre choix
  
  titlePanel("CREPIN"),
  
  navbarPage(
    "MON APPLICATION",
    
    # Onglet - Carte de l'Afrique de l'Ouest
    tabPanel(
      "CARTE DU MONDE PAR EVENEMENT",
      
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("event_filter", "PAYS :", choices = unique(ACLED_Western_Africa$pays), selected = unique(ACLED_Western_Africa$pays)),
          br(),
          actionButton("filter_button", "Filtrer")
        ),
        
        mainPanel(
          leafletOutput("map")
        )
      )
    ),
    
    # Onglet - Filtrage des événements
    tabPanel(
      "FILTRAGE",
      
      sidebarLayout(
        sidebarPanel(
          selectInput("country_filter", "PAYS :", choices = unique(ACLED_Western_Africa$pays)),
          br(),
          selectInput("event_type_filter", "EVENEMENTS :", choices = unique(ACLED_Western_Africa$type)),
          br(),
          sliderInput("year_filter", "ANNEE :", min = min(ACLED_Western_Africa$annee), max = max(ACLED_Western_Africa$annee), value = c(min(ACLED_Western_Africa$annee), max(ACLED_Western_Africa$annee)))
        ),
        
        mainPanel(
          fluidRow(
            column(
              width = 2,
            ),
            column(
              width = 10,
              h3("CARTE"),
              leafletOutput("filtered_map")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Carte de l'Afrique de l'Ouest - Carte par pays
  output$map <- renderLeaflet({
    filtered <- subset(ACLED_Western_Africa, pays %in% input$event_filter)
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Filtrage des événements
  filteredData <- reactive({
    subset(ACLED_Western_Africa, pays == input$country_filter & type == input$event_type_filter & annee >= input$year_filter[1] & annee <= input$year_filter[2])
  })
  
  # Carte filtrée
  output$filtered_map <- renderLeaflet({
    filtered <- filteredData()
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
