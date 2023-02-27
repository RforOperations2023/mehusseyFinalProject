library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(leaflet)
library(leaflet.extras)
library(sf)
library(rgeos)

crime <- st_read("./Crime_Data.geojson")

crime$Offense <- as.numeric(as.factor(crime$Offense))


pal <- colorNumeric(
  palette = "Purples",
  domain = crime$`Offense`)


leaflet() %>%
  addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%  
  addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
  addMarkers(data = crime, ~Longitude, ~Latitude) %>% 
  setView(44.9778,-93.2650, 12)

leaflet(data = crime) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addHeatmap(lng = ~Longitude, lat = ~Latitude, radius = 8) %>% 
  addMarkers(data = crime, ~Longitude, ~Latitude) %>% 
  setView(44.9778,-93.2650, 12)

