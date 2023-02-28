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

View(nypd)

# Define colors for each boro
boro_colors <- c("BROOKLYN" = "#FF0000", "QUEENS" = "#00FF00", 
                 "MANHATTAN" = "#0000FF", "BRONX" = "#FFA500", 
                 "STATEN ISLAND" = "#800080")


# Combine polygon with clusters
nypd_map <- leaflet() %>%
  addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
           attribution = "Google", group = "Google") %>%  
  addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
  setView(-74.0060, 40.7128, 9) %>%
  addPolygons(data = boro_shapes, color = ~boro_colors, group = "boro_name") %>%
  addMarkers(data = nypd, clusterOptions = markerClusterOptions())

# Show the map
nypd_map
