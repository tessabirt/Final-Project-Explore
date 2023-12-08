#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

Year <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
Auckland <- c(1760, 1549, 1388, 1967, 1326, 1765, 1814, 1693, 1502, 1751)
Wellington <- c(2176, 3154, 1138, 1196, 2132, 3176, 4181, 5169, 3150, 4175)
Lyttelton <- c(2176, 3154, 1138, 1196, 2132, 3176, 4181, 5169, 3150, 4175)
my_data <- as.data.frame(cbind(Year,Auckland,Wellington, Lyttelton))

ui <- fluidPage(
  titlePanel("New Zealand Annual Mean Sea Level (MSL) Summary"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Annual Mean Sea Level Summary for various locations around NZ."),
      
      selectInput("var", 
                  label = "Choose a Location",
                  choices = c("Auckland",
                              "Lyttelton",
                              "Wellington"),
                  selected = "Auckland"),
      
      sliderInput("range", 
                  label = "Choose a start and end year:",
                  min = min(my_data$Year), max = max(my_data$Year), value = c(2003, 2008),sep = "",)
    ),
    
    mainPanel(
      tableOutput("DataTable")
    )
  )
)
server <- function(input, output) {
  
  output$DataTable <- renderTable({
    dt <- my_data[my_data$Year >= input$range[1] & my_data$Year <= input$range[2],]
    dt[,c("Year",input$var)]
  },include.rownames=FALSE)
  
}
shinyApp(ui, server)
