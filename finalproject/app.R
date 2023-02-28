library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(leaflet)
library(leaflet.extras)
library(sf)
library(rgeos)
library(tidyr)

# Load the shapefile data for the boroughs
boro_shapes <- st_read("BoroughBoundaries.geojson")

#Load the shooting data
nypd <- read_csv("NYPD_Shooting_Incident_Data__Historic_.csv", 
                 col_types = cols(OCCUR_DATE = col_date(format = "%m/%d/%Y")))

nypd <- nypd %>%
  mutate(OCCUR_YEAR = year(ymd(OCCUR_DATE))) #creating a year only column 

nypd_sf <- st_as_sf(nypd, coords = c("Longitude", "Latitude"), crs = 4326)
joined_data <- st_join(nypd_sf, boro_shapes, join = st_within)


# Define colors for each boro
boro_colors <- c("Brooklyn" = "#FF0000", "Queens" = "#00FF00", 
                 "Manhatten" = "#0000FF", "Bronx" = "#FFA500", 
                 "Staten Island" = "#800080")


ui <- fluidPage(
  titlePanel("New York Map"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId ="boro", 
                  label = "Select a Borough", 
                  choices = c("Brooklyn", "Queens", "Manhatten", "Bronx", "Staten Island"), 
                  selected = NA),
      selectInput(inputId = "year", 
                  label = "Select a Year", 
                  choices = c(2019, 2020, 2021), 
                  selected = 2021,
                  multiple = TRUE),
      selectInput(inputId = "death",
                  label = "Specify if the shooting was fatal or not:",
                  choices = unique(sort(nypd$STATISTICAL_MURDER_FLAG)),
                  selected = NA, 
                  multiple = TRUE),
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server
server <- function(input, output) {
  
  #Main reactive function to change with each input
  filtered_nypd <- reactive({
    req(input$boro, input$year, input$death)
    joined_data %>% 
      filter(OCCUR_YEAR == input$year & boro_name %in% input$boro 
             & STATISTICAL_MURDER_FLAG == input$death)
  })
  
  #Reactive function for polygon layers
  filtered_boro <- reactive({
    req(input$boro)
    filter(boro_shapes, boro_name %in% input$boro)
  })
  
#MAIN LEAFLET MAP WITH CLUSTERS
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
               attribution = "Google", group = "Google") %>%  
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-74.0060, 40.7128, 9) %>%
      addMarkers(data = filtered_nypd(), clusterOptions = markerClusterOptions())
  })
}

#CREATING POLYGON LAYERS WITH BOROUGHS
observe({
  boroInf <- filtered_boro()
  leafletProxy("map", data = boroInf) %>%
    clearGroup(group = "Boroughs") %>%
    addPolygons(popup = ~paste0("<b>", LABEL, "</b>"), group = "Boroughs", layerId = ~boro_names, fill = FALSE, color = ~boro_colors)
})


# Run the app
shinyApp(ui = ui, server = server)
