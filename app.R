# 
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lake Mead Depth"),

    # Sidebar with a slider input for number of bins 
    selectInput("depth_type", "Select Depth Type",
                choices = c("Low", "Mean", "High"),
                selected = "Mean"
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("depth_plot")
        )
    ) 

# Define server logic required to draw a histogram
server <- function(input, output) {

  selected_data <- reactive({
    switch(input$depth_type,
           "Low" = mead_long$Low,
           "Mean" = mead_long$yearly_mean,
           "High" = mead_long$High) %>%
      as.data.frame()  # Convert to data frame
  })

   output$depth_plot <- renderPlot({
     ggplot(data = mead_long, aes(x = Year, y = selected_data())) +
       geom_line() +
       geom_point() +
       labs(title = paste(input$depth_type, "Lake Mead Water Depth"),
            x = "Year", y = "Water Depth")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
