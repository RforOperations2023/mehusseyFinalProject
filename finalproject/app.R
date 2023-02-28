library(shiny)
library(leaflet)
library(httr)
library(dplyr)

ui <- fluidPage(
  titlePanel("New York City Crime Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crimeType", "Crime Type", choices = c("All", "Murder", "Assault", "Robbery", "Burglary", "Theft", "Motor Vehicle Theft"), selected = "All"),
      selectInput("year", "Year", choices = c(2019, 2020, 2021), selected = 2021),
      selectInput(inputId = "neighborhood",
                  label = "Select one or more neighboorhods for all graphs, map and table:",
                  choices = unique(sort(crime$Ward)),
                  selected = "4", 
                  multiple = TRUE),
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  url <- "https://data.cityofnewyork.us/resource/833y-fsy8.json"
  response <- GET(url, query = list($where = paste0("year(occur_date)=", input$year)))
  data <- content(response, as = "text") %>% 
    fromJSON(flatten = TRUE) %>% 
    select(incident_key, occur_date, occur_time, boro, precinct, jurisdiction_code, location_desc, statistical_murder_flag, perp_age_group, perp_sex, perp_race, vic_age_group, vic_sex, vic_race, latitude, longitude)
  
  filtered_data <- reactive({
    if (input$crimeType == "All") {
      data
    } else {
      data %>% filter(location_desc == input$crimeType)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data = filtered_data(), 
                 clusterOptions = markerClusterOptions(),
                 label = ~as.character(incident_key),
                 popup = ~paste0("<b>Date:</b> ", occur_date, "<br>",
                                 "<b>Time:</b> ", occur_time, "<br>",
                                 "<b>Borough:</b> ", boro, "<br>",
                                 "<b>Location:</b> ", location_desc, "<br>",
                                 "<b>Perpetrator Age:</b> ", perp_age_group, "<br>",
                                 "<b>Perpetrator Sex:</b> ", perp_sex, "<br>",
                                 "<b>Perpetrator Race:</b> ", perp_race, "<br>",
                                 "<b>Victim Age:</b> ", vic_age_group, "<br>",
                                 "<b>Victim Sex:</b> ", vic_sex, "<br>",
                                 "<b>Victim Race:</b> ", vic_race, "<br>",
                                 "<b>Latitude:</b> ", latitude, "<br>",
                                 "<b>Longitude:</b> ", longitude, "<br>")
      ) %>%
      addLayersControl(
        overlayGroups = c("Markers"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

shinyApp(ui = ui, server = server)
