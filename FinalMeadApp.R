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
library(bslib)
library(sf)
library(leaflet)
library(readxl)
library(plotly)


meaddata <- readxl::read_excel("~/Desktop/Environmental Data Sci/Tessa-Final-Project-Explore/Mead_high_low_mean.xlsx")
mead_long <- meaddata %>%
  tidyr::gather(key = "depth_type", value = "depth", -Year)
basin <- st_read("globalwatershed.shp")
AEPmead <- read_excel("AEPmead.xlsx")
meadstats <- read_excel("MeadStats.xlsx")
meadpie <- data.frame(
  Category = c("California", "Arizona", "Nevada"),
  Value = c(59, 37, 4)
 )

custom_theme <- bs_theme(
  primary = "#4D94FF",  # Set the primary color
  secondary = "#8AA9B6",  # Set the secondary color
  bg = "#E0E0E0",
  fg = "#2E4960",# Set the background color
  dark = "#4D94FF",  # Set the text color
  font_size_base = 16  # Set the base font size
)

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = custom_theme, 
  title = "Lake Mead Water Depth Analysis",
  tabPanel("Depth Data", 
           fluidRow(
             column(
               width = 6,
               p("Lake Mead, located on the Arizona-Nevada border, is a crucial reservoir that serves as a primary water source for millions of people in the southwestern United States. The lake's water is utilized for agricultural irrigation, municipal supply, and hydroelectric power generation, supporting communities in Arizona, Nevada, and California. Its geographical significance lies in its position as the largest reservoir in the United States, created by the Hoover Dam on the Colorado River.
                 However, Lake Mead is facing severe challenges, both environmental and human-induced, that have led to a significant drop in water levels. The region has been grappling with prolonged droughts, exacerbated by climate change, which contribute to decreased inflows into the reservoir. Additionally, increased water demand and the overallocation of the Colorado River's resources further strain Lake Mead.
                 Lake Mead is known for the the prominent 'bath tub rings' around the lake's perimeter. These rings, caused by the deposition of minerals and salts on exposed shorelines as the water recedes, starkly illustrate the extent of the water loss. Since the year 2000, Lake Mead has experienced a substantial decrease in water depth, losing a considerable percentage of its capacity."),
               absolutePanel(
                 fixed = TRUE, bottom = 5, left = 50,
                 width = 700, height = 300,
                 tags$img(src = "bathtubrings.jpg", width = "90%", height = "90%")
               )
             ),
             column(
               width = 6,
               plotOutput("staticPlot"),
               absolutePanel(
                tableOutput("summaryTable"),
                 fixed = TRUE, bottom = 40, right = 270,
               sliderInput("year", "Select Year:", min = 2001, max = 2022, value = 2001, step = 1, sep = "")
             ))
           )
  ),
  tabPanel("Time Lapse",
           fluidRow(
             column(
               width = 8,
           tabsetPanel(
             tabPanel("2001", tags$img(src = "LM01.png", height = 600, width = 800)),
             tabPanel("2020", tags$img(src = "LM20.png", height = 600, width = 800)))),
           column(
             width = 4,
             absolutePanel(
               fixed = TRUE, top = 175, right = 50,
               width = 525, height = 300,
             plotlyOutput("piechart")
           ))
           )),
  tabPanel("Basin",
         leafletOutput("map"),
         fluidRow(
           # Left column for the AEP plot
           column(6, 
              absolutePanel(
              fixed = FALSE, bottom = -10, left = 20,
               width = 650, height = 325, plotOutput("aepPlot"))),
           
           # Right column 
           column(6, 
             absolutePanel(
               fixed = FALSE, bottom = 20, right = -70,
               tags$style(HTML("#parameter_select { width: 300px; }
                     #parameter_table { width: 300px; }")),
                sidebarLayout(
                    sidebarPanel(
                      selectInput("parameter_select", "Select Parameter", choices = unique(meadstats$Parameter), 
                                  width = "400px"),
                      hr(),
                    ),
                    mainPanel(
                      tableOutput("parameter_table")
                    ))
                  )
                 )
                )
              )
            )
  
  
    

# Define server
server <- function(input, output) {
  
  custom_colors <- c("#191970", "#6082B6", "#337DFF")
  
  # Create ggplot based on filtered data
  static_plot <- ggplot(mead_long, aes(x = Year, y = depth, color = depth_type)) +
    geom_line() +
    theme(
      plot.background = element_rect(fill = "#D3D3D3"),  # Background color
      panel.background = element_rect(fill = "#F0F0F0")) + 
    scale_color_manual(values = custom_colors) +
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
  
  output$piechart <- renderPlotly({
    plot_ly(
      data = meadpie,
      labels = ~Category,
      values = ~Value,
      type = "pie",
      textinfo = "percent",
      hoverinfo = "label+percent",
      marker = list(colors = custom_colors)
    ) %>%
      layout(title = "Mead Water Allocation", paper_bgcolor = "#E0E0E0")
  })

  #Leaflet map of basin
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data = basin, fillOpacity = 0.25)
  })
  
  #AEP curve
output$aepPlot <- renderPlot({
 ggplot(AEPmead, aes(x = Statistic, y = Value)) +
    geom_line(color = "#0f4d92") +
    scale_y_continuous(labels = scales::comma) + 
    labs(
      title = "Annual Exceedance Probability Curve", 
      x = "Percent AEP Flood", 
      y = "Flow (cf/s)"
    ) +
    theme(
      plot.margin = margin(10, 10, 10, 10, "pt"),  # Add a margin around the plot
      plot.background = element_rect(fill = NA, color = "#0f4d92", size = 1)
    )
  })
# Filter data based on selected statistic
selected_parameter_data <- reactive({
  filter(meadstats, Parameter == input$parameter_select)
})

# Display table with value and unit
observeEvent(input$parameter_select, {
  output$parameter_table <- renderTable({
    filter(meadstats, Parameter == input$parameter_select)
  })
})

# Display value and unit as text
output$selected_value_unit <- renderText({
  selected_parameter <- selected_parameter_data()
  
})
}
# Run the app
shinyApp(ui, server)
