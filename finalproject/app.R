library(shiny)
library(shinythemes)
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
                 col_types = cols(OCCUR_DATE = col_date(format = "%m/%d/%Y"), 
                                  OCCUR_TIME = col_time(format = "%H:%M:%S"))) 
#forcing the time and date columns to be time/date format for further manipulation

#mutating to create a year shooting occurs column (for user input) and time of day shooting occurs
nypd <- nypd %>%
  mutate(OCCUR_YEAR = year(ymd(OCCUR_DATE))) %>% 
  mutate(OCCUR_HOUR = hour(OCCUR_TIME), #extracting just the hour first
         TIME_OF_DAY = case_when(
           OCCUR_HOUR >= 6 & OCCUR_HOUR <= 11 ~ "Morning",
           OCCUR_HOUR >= 12 & OCCUR_HOUR <= 16 ~ "Afternoon",
           OCCUR_HOUR >= 17 & OCCUR_HOUR <= 21 ~ "Evening",
           OCCUR_HOUR >= 22 | OCCUR_HOUR <= 5 ~ "Night"),
         Icon = as.character(STATISTICAL_MURDER_FLAG)) #variable is a TRUE/FALSE, so making an as.character so it works with the icons

#joining the geojson to my csv so that I can create a polygon layer
nypd_sf <- st_as_sf(nypd, coords = c("Longitude", "Latitude"), crs = 4326) 
joined_data <- st_join(nypd_sf, boro_shapes, join = st_within)

#Making icons for whether or not a murder occured
iconSet <- awesomeIconList(
  "TRUE" = makeAwesomeIcon(icon = "skull-crossbones", markerColor = "red", library = "fa"),
  "FALSE" = makeAwesomeIcon(icon = "heart-pulse", markerColor = "green", library = "fa")
)

#USER INTERFACE
ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel("New York City Shootings 2019-2021"),
  sidebarLayout(
    sidebarPanel(
      h4("Please select inputs below. These will change the map, graphs, and data table."),
      checkboxGroupInput(inputId ="boro", #Input 1: select a borough
                  label = "Select a Borough", 
                  choices = c("Brooklyn", "Queens", "Manhattan", "Bronx", "Staten Island"), 
                  selected = "Manhattan"),
      checkboxGroupInput(inputId = "year", #Input 2: Select a year
                  label = "Select a Year", 
                  choices = c(2019, 2020, 2021), 
                  selected = 2021),
      selectInput(inputId = "death", #Input 3: select if the shooting was fatal or not
                  label = "Specify if the shooting was fatal or not:",
                  choices = unique(sort(nypd$STATISTICAL_MURDER_FLAG)),
                  selected = "TRUE", 
                  multiple = TRUE),
      #add a download button
      downloadButton("downloadData", "Download data") #add a download button
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", #tap 1: leaflet map
                           fluidRow(leafletOutput(outputId = "map"))
                           ),
                  tabPanel("Graphs", #tab 2: graphs (bar and pie chart)
                           fluidRow(
                             plotlyOutput(outputId = "bar")
                           ),
                           br(), #white space between plots
                           fluidRow(
                             plotlyOutput(outputId = "pie")
                           )),
                  tabPanel("Data Table", #tab 3: datatable 
                           fluidRow(DT::dataTableOutput(outputId = "datatable"))
                           ))
      )
    )
  )

#SERVER SIDE
server <- function(input, output) {
  
  #Main reactive function to change with each input
  filtered_nypd <- reactive({
    req(input$boro, input$year, input$death)
    joined_data %>% 
      filter(OCCUR_YEAR == input$year & boro_name %in% input$boro 
             & STATISTICAL_MURDER_FLAG == input$death)
  })
  
  #Separate reactive function for polygon layers (just using the boro_shapes file)
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
      setView(-74.0060, 40.7128, 10) #set the coordinates to NYC and the zoom to 10
  })

#CREATING POLYGON LAYER WITH BOROUGHS
observe({
  boroInf <- filtered_boro()
  leafletProxy("map", data = boroInf) %>%
    clearGroup(group = "Boroughs") %>%
    addPolygons(popup = ~paste0("<b>", boro_name, "</b>"), 
                group = "Boroughs", layerId = ~boro_name)
})

#MARKER LAYER WITH CLUSTERS
observe({
  nypdInf <- filtered_nypd()
  leafletProxy("map", data = nypdInf) %>%
    clearGroup(group = "Shootings") %>%
    clearMarkerClusters() %>%
    addAwesomeMarkers(
      icon = ~iconSet[Icon], #calling the icons from iconSet above
      clusterOptions = markerClusterOptions(), #adding clusters so that it looks cleaner zoomed out
      popup = ~paste0("<b>Borough: </b>", boro_name,
                      "<br><b>Year Occured: </b>", OCCUR_YEAR,
                      "<br><b>Time of Day: </b>", TIME_OF_DAY,
                      "<br><b>Perpetrator Race: </b>", PERP_RACE,
                      "<br><b>Victim Race: </b>", VIC_RACE),
      group = "Shootings")
})

#BAR CHART OUTPUT
output$bar <- renderPlotly({
  df <- filtered_nypd()
  time_count <- data.frame(table(df$TIME_OF_DAY)) #counting shootings by time of day
  ggplotly(
    ggplot(data = time_count, aes(x = Var1, y = Freq, fill = Var1)) +
      geom_col() +
      xlab("Time of Day") +
      ylab("Number of Shootings") +
      ggtitle("Number of Shootings by Time of Day") +
      theme_classic() + 
      theme(legend.position = "none")
)
})

#PIE CHART OUTPUT
output$pie <- renderPlotly({
  df2 <- filtered_nypd()
  age_count <- data.frame(table(df2$VIC_AGE_GROUP)) #counting shootings by victim age group (chose this variable because it's one of the few without NAs - perp variables all have NAs if perp was not identified)
  plot_ly(age_count, labels = ~Var1, values = ~Freq, type = 'pie',
                 text = ~paste0(Freq),
                 marker = list(colors = c('#c584e4', '#82ac64', '#00bbd4', '#fef769', 
                                                   '#FFA500'),
                                                   line = list(color = '#FFFFFF', width = 1))) %>%
    layout(title = "Total Shootings by Victim Age Group",
           legend = list(title = list(text = "Age Group")))
})

#DATA TABLE OUTPUT
output$datatable <- DT::renderDataTable({
  nypd_dt <- filtered_nypd() 
  st_geometry(nypd_dt) <- NULL #deleting the geometry column
    DT::datatable(data = nypd_dt %>% 
                    select(OCCUR_DATE, TIME_OF_DAY, BORO,
                           PRECINCT, STATISTICAL_MURDER_FLAG,
                           PERP_AGE_GROUP, PERP_SEX, PERP_RACE,
                           VIC_AGE_GROUP, VIC_SEX, VIC_RACE), #selecting variables to display
                  options = list(pageLength = 10,
                                 rownames = FALSE)
    )
  })

#DOWNLOAD BUTTON OUTPUT
output$downloadData <- downloadHandler(
  filename = function() {
    paste("nypd-data-", Sys.Date(), ".csv", sep = "")
  },content = function(file) {
    write.csv(filtered_nypd(), file)
  }
)

}
#RUN THE APP
shinyApp(ui = ui, server = server)
