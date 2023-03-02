library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
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
                  choices = c("Brooklyn", "Queens", "Manhattan", "Bronx", "Staten Island"), 
                  selected = "Manhattan"),
      selectInput(inputId = "year", 
                  label = "Select a Year", 
                  choices = c(2019, 2020, 2021), 
                  selected = 2021,
                  multiple = TRUE),
      selectInput(inputId = "death",
                  label = "Specify if the shooting was fatal or not:",
                  choices = unique(sort(nypd$STATISTICAL_MURDER_FLAG)),
                  selected = "FALSE", 
                  multiple = TRUE),
      #add a download button
      downloadButton("downloadData", "Download data")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map",
                           fluidRow(leafletOutput(outputId = "map"))
                           ),
                  tabPanel("Graphs",
                           fluidRow(
                             plotlyOutput(outputId = "bar")),
                           fluidRow(
                             plotlyOutput(outputId = "pie"))
                           ),
                  tabPanel("Data Table",
                           fluidRow(DT::dataTableOutput(outputId = "datatable"))
                           ))
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
      setView(-74.0060, 40.7128, 9)
  })

#CREATING POLYGON LAYERS WITH BOROUGHS
observe({
  boroInf <- filtered_boro()
  leafletProxy("map", data = boroInf) %>%
    clearGroup(group = "Boroughs") %>%
    addPolygons(popup = ~paste0("<b>", boro_name, "</b>"), 
                group = "Boroughs", layerId = ~boro_name, color = ~boro_colors)
})

#CLUSTERING BY YEAR 
observe({
  nypdInf <- filtered_nypd()
  leafletProxy("map", data = nypdInf) %>%
    clearGroup(group = "Shootings") %>%
    clearMarkerClusters() %>%
    #addAwesomeMarkers(icon = ~icon[OCCUR_YEAR],
    addCircleMarkers(clusterOptions = markerClusterOptions(),
                      popup = ~paste0("<b>", boro_name, "</b>"),
                      group = "Shootings")
})

#BAR CHART OUTPUT
output$bar <- renderPlotly({
  df <- filtered_nypd()
  year_count <- data.frame(table(df$OCCUR_YEAR))
  ggplotly(
    ggplot(data = year_count, aes(x = Var1, y = Freq)) +
      geom_col() +
      xlab("OCCUR-YEAR") +
      ylab("Count") +
      scale_x_discrete(limits = c("2019", "2020", "2021"))
)
})

#PIE CHART OUTPUT
output$pie <- renderPlotly({
  df <- filtered_nypd()
  boro_count <- data.frame(table(df$BORO))
  fig <- plot_ly(boro_count, labels = ~Var1, values = ~Freq, type = 'pie',
                 text = ~paste0(Freq),
                 marker = list(colors = c('#c584e4', '#82ac64', '#00bbd4', '#fef769'),
                               line = list(color = '#FFFFFF', width = 1)))
})

#DATA TABLE OUTPUT
output$datatable <- DT::renderDataTable({
    DT::datatable(data = filtered_boro() 
    )
  })

#DOWNLOAD BUTTON
output$downloadData <- downloadHandler(
  filename = function() {
    paste("nypd-data-", Sys.Date(), ".csv", sep = "")
  },content = function(file) {
    write.csv(nypd_filtered(), file)
  }
)

}
# Run the app
shinyApp(ui = ui, server = server)
