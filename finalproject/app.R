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
                  selected = 2021),
      selectInput(inputId = "muder",
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
  
  filtered_nypd <- reactiveVal(nypd)
  
  observe({
    filtered_nypd(nypd %>% filter(OCCUR_YEAR == input$year & boro_name %in% input$boro))
  })
  
  output$nypd_map <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
               attribution = "Google", group = "Google") %>%  
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-74.0060, 40.7128, 9) %>%
      addPolygons(data = boro_shapes, color = ~boro_colors, group = "boro_name") %>%
      addMarkers(data = filtered_nypd(), clusterOptions = markerClusterOptions())
  })
}

# Run the app
shinyApp(ui = ui, server = server)
