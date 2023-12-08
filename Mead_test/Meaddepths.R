# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Lake Mead Depth"),
  
  # Sidebar with a slider input for number of bins 
  selectInput("depth_type", "Select Depth Type",
              choices = c("Low", "Mean", "High"),
              selected = "Mean"),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("depth_plot")
  )
) 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selected_data <- reactive({
    filter(mead_long, depth_type == input$depth_type)
  })
  
  output$depth_plot <- renderPlot({
    ggplot(data = selected_data(), aes(x = Year, y = depth)) +
      geom_line() +
      geom_point() +
      labs(title = paste(input$depth_type, "Lake Mead Water Depth"),
           x = "Year", y = "Water Depth")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
