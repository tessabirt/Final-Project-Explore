#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(imager)


meaddata <- readxl::read_excel("~/Desktop/Environmental Data Sci/Tessa-Final-Project-Explore/Mead_high_low_mean.xlsx")
mead_long <- meaddata %>%
  tidyr::gather(key = "depth_type", value = "depth", -Year)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Lake Mead Water Depth Analysis"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("staticPlot")), 
        tabPanel("Table", tableOutput("summaryTable"),  sliderInput("year", "Select Year:", min = 2001, max = 2022, value = 2001, step = 1, sep = "")
        ),
        tabPanel("2001", tags$img(src = "LM2001.png")
        ))
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
    dplyr::filter(mead_long, Year == input$year)
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

}
# Run the app
shinyApp(ui, server)
