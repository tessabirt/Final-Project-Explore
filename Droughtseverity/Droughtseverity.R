#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Drought Severity Over the Years"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year", min = 2000, max = 2022, value = 2000)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server
# Define server
server <- function(input, output) {
  output$map <- renderLeaflet({
    # Create initial map with an initial view
    leaflet() %>%
      addTiles() %>%
      setView(lng = -114.4698, lat = 36.2174, zoom = 10)  # Adjust coordinates and zoom level accordingly
  })
  
  observe({
    # Load GeoJSON data for the selected year
    geojson_url_1 <- paste0("mymap2001", input$year, ".geojson")
    geojson_url_2 <- paste0("mymap2022", input$year, ".geojson")
    
    # Check if GeoJSON files exist
    if (file.exists(geojson_url_1) && file.exists(geojson_url_2)) {
      # Add layers to the map
      leafletProxy("map") %>%
        clearShapes() %>%
        addGeoJSON(geojson_url_1, group = "mymap2001") %>%
        addGeoJSON(geojson_url_2, group = "mymap2022")
    } else {
      # Print a message or take appropriate action if files do not exist
      cat("GeoJSON files not found for the selected year.\n")
    }
  })
}

# Run the app
shinyApp(ui, server)
