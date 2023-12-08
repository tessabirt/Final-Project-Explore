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
library(htmlwidgets)
library(htmltools)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Lake Mead Water Depth Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:", min = 2001, max = 2022, value = 2001, step = 1, sep = "")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("staticPlot")), 
        tabPanel("Table", tableOutput("summaryTable")),
        tabPanel("Map", leafletOutput("map"))
      )
    )
  )
)

# Define server
server <- function(input, output) {

  # Create ggplot based on filtered data
  static_plot <- ggplot(mead_long, aes(x = Year, y = depth, color = depth_type)) +
    geom_line() +
    labs(title = "Yearly Water Depth for Lake Mead",
         x = "Year",
         y = "Water Depth",
         color = "Depth Type") +
    theme_minimal()
  
  # Display static plot
  output$staticPlot <- renderPlot({
    print(static_plot)
  })
  
  filtered_data <- reactive({
    filter(mead_long, Year == input$year)
  })
  
  # Create summary table based on filtered data
  output$summaryTable <- renderTable({
    summary_data <- filtered_data() %>%
      summarise(
        Low = min(depth),
        Mean = mean(depth),
        High = max(depth)
      )
    summary_data
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data = shapefile_2022, fillOpacity = 0.5, color = "red")
  })
}
# Run the app
shinyApp(ui, server)
