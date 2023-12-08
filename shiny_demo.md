---
editor_options: 
  markdown: 
    wrap: 72
---

# Shiny Demo

Caitlin Mothes 2023-03-01

## Shiny Demo

[Shiny](https://shiny.rstudio.com/) is an R package that takes
interactivity to another level through interactive web applications,
allowing users to interact with any aspect of your data and analysis.
You can host them as standalone web apps or embed them within R Markdown
documents or build dashboards. And the best part is...**you can do it
all within R, no web development skills required!**

So, lets walk through the steps and build a quick shiny app! For this
demo we are going to build off some of the data and interactive maps you
made in the [Week 4 Geospatial Lesson
3](https://github.com/Data-Sci-Intro-2023/Week-4-Geospatial/blob/master/spatial-viz.md).
For example, we have a lot of species occurrence data with numerous
variables. Based on various attributes, we could allow users to choose
what they want to see on the map, such as which species, what type of
observation they were, and the range of elevation species were found at.

*But first, a little background on how Shiny works.*

Shiny apps are contained in a single script called `app.R` . `app.R` has
three components:

-   a user interface (ui) object, which controls the layout and
    appearance of your app

-   a server function, which contains the instructions needed to build
    your app

-   a call to `shinyApp()` which creates your web application based on
    your ui/server objects.

Let's create a new shiny app by going to File -\> New File -\> Shiny Web
App. Call it something like 'shinyDemo', and save it in the project
directory (which should be the default). Keep all other default
settings.

This creates a new folder in your project directory with the same name
you just gave the application, and within that folder is the `app.R`
script which automatically gives an outline of our shiny app, with the
`ui` and `server` objects and a call to `shinyApp()` at the end. **Since
shiny apps are self contained within the `app.R` file**, at the top of
the file define which libraries you need and read in your data.
**`app.R` files assume the working directory is the directory the
`app.R` file lives in**, so for this example the folder you just created
is the working directory for `app.R`.

## Create your first shiny app

RStudio actually gives you a template app to work with by filling in the
`ui` and `server` elements with some demo widgets and data. We can
actually run this script and make our first shiny app!

To run this app, from the `app.R` script click the 'Run App' button in
the upper right with the green arrow.

![](www/oldfaithful.png)

This is a very basic shiny app, but inspect the `app.R` script code and
compare it to the application it creates. In the `ui` we create the
layout (a sidebar panel and a main panel), specify the widgets and its
settings (`sliderInput()`) and a placeholder for an output
(`plotOutput()`). One of the most important things to notice is that
each widget or output in the `ui` must be given an ID, which is the
first argument in each function (e.g., the ID for `sliderInput()` is
"bins", the ID for the `plotOutput()` is "distPlot").

These IDs are how you define what happens on the server side. When we
look at this server code, we create the plot output by calling
`output${insert ID name here}` and assign it some code to create the
plot. Within this code chunk, we also see `input$bins`. This will return
the value that the user has selected from the slider input and create
the plot (in this case, changing the bin size of the plot).

## Create a shiny app for Colorado species occurrence data

Now that you've seen some of the fundamental structure of a shiny app,
lets make our own using data from Week 4 Geospatial lesson.

First, load the shiny demo data (located in the data/ folder) and
inspect the `occ` object. You should also read in the following
libraries:

``` r
library(tmap)
library(sf)
library(dplyr)

load("data/shinyDemoData.RData")

occ
```

`occ` is an sf point object, and for each species occurrence point we
have attributes for the species, year and month of observation, type of
observation, and the elevation at that occurrence. We also loaded in the
park boundary polygon for Rocky Mountain National Park (ROMO) that we
will add to the map as well. Let's use `tmap` to make an interactive map
of all these layers. *Note: this is pulling from some of the code in the
Week 4 Geospatial spatial-viz.Rmd lesson.*

``` r
tmap_mode("view")

tm_shape(occ) +
  tm_dots(
    col = "Species",
    size = 0.1,
    palette = "Dark2",
    title = "Species Occurrences",
    popup.vars = c("Record Type" = "basisOfRecord",
                   "Elevation (m)" = "elevation")
  ) +
  tm_shape(ROMO) +
  tm_polygons(alpha = 0.7, title = "Rocky Mountain National Park")
```

The first thing we may notice is there are a lot of species occurrence
points on this map. This is a good scenario to turn this into a shiny
application to allow users to filter out these points on the map based
on different attributes.

### Getting Started

Remember that the `app.R` runs assuming that the directory it is in is
the working directory. To get the data for this demo shiny app, **find
`shinyDemoData.RData` in the Week-6-Project-Explore repo (your current
project directory) and copy it to the new shiny app folder you just
created (so the same folder `app.R` is in).**

Now lets set up our app script by loading necessary libraries and data
sets. Put this chunk of code at the top of `app.R` (under
`library(shiny)`).

``` r
#set up for the shiny app
library(tmap)
library(sf)
library(dplyr)

# read in data
load("shinyDemoData.RData")

# set tmap mode to interactive
tmap_mode("view")
```

### Define the `ui`

Lets keep the same layout as the template `app.R` script, with a fluid
page with a title panel, followed by a sidebar layout with a main panel
(our map) and a side panel (user inputs). You can learn more about
different layout options
[here](https://shiny.rstudio.com/articles/layout-guide.html).

However, we do want to change the widgets and output. Delete the current
`ui` and replace it with the following code (reading the comments to see
what each widget is doing):

``` r
ui <- fluidPage(
  #App title
  titlePanel("Species of Colorado"),
  
  # Add some informational text using and HTML tag (i.e., a level 5 heading)
  h5(
    "In this app you can filter occurrences by species, type of observation, and elevation. You can also click on individual occurrences to view metadata."
  ),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for widgets that users can interact with
    sidebarPanel(
      # Input: select species shown on map
      checkboxGroupInput(
        inputId = "species",
        label = "Species",
        # these names should match that in the dataset, if they didn't you would use 'choiceNames' and 'choiceValues' like we do for the next widget
        choices = list("Elk", "Yellow-bellied Marmot", "Western Tiger Salamander"),
        # selected = sets which are selected by default
        selected = c("Elk", "Yellow-bellied Marmot", "Western Tiger Salamander")
      ),
      
      # Input: Filter points by observation type
      checkboxGroupInput(
        inputId = "obs",
        label = "Observation Type",
         choiceNames = list(
          "Human Observation",
          "Preserved Specimen",
          "Machine Observation"
        ),
        choiceValues = list(
          "HUMAN_OBSERVATION",
          "PRESERVED_SPECIMEN",
          "MACHINE_OBSERVATION"
        ),
        selected = c("HUMAN_OBSERVATION",
                     "PRESERVED_SPECIMEN",
                     "MACHINE_OBSERVATION"
        )
      ),
      
      
      # Input: Filter by elevation
      sliderInput(
        inputId = "elevation",
        label = "Elevation",
        min = 1000,
        max = 4500,
        value = c(1000, 4500)
      )
      
    ),
    
    # Main panel for displaying output (our map)
    mainPanel(# Output: interactive tmap object
      tmapOutput("map"))
    
  )
  
)
```

With this updated `ui` in the `app.R` script, you can actually **run the
app** and see what the interface looks like, there just won't be any
outputs or reactivity yet.

### Define the `server`

Now we need to define the server logic (i.e., what R code to run when a
user interacts with the application) that draws a map based on the user
inputs.

Essentially we can take the tmap code above to create the interactive
map, but what's changing now is our `occ` data set, which will be
filtered based on the user inputs in the `ui`. When an environmental
object changes based on user inputs, we call this a *reactive* object
and define it within the `reactive()` function. When you later use that
reactive object (for example adding it to a map), you must add `()` to
the end of the object name. Finally, to create the map we put the code
within `renderTmap()`. Here is the final server code, which you should
replace with the template `server <-` code in `app.R`. *Note: be
cautious of proper bracket placement in this code, rainbow parentheses
come in handy with Shiny apps!*

``` r
server <- function(input, output){
  
  # Make a reactive object for the occ data by calling inputIDs to extract the values the user chose
  occ_react <- reactive(
    occ %>%
      filter(Species %in% input$species) %>%
      filter(basisOfRecord %in% input$obs) %>%
      filter(elevation >= input$elevation[1] &
               elevation <= input$elevation[2])
  )
  
  # Render the map based on our reactive occurrence dataset
  output$map <- renderTmap({
    tm_shape(occ_react()) +
      tm_dots(
        col = "Species",
        size = 0.1,
        palette = "Dark2",
        title = "Species Occurrences",
        popup.vars = c(
          "Species" = "Species",
          "Record Type" = "basisOfRecord",
          "Elevation (m)" = "elevation"
        )
      ) +
      tm_shape(ROMO) +
      tm_polygons(alpha = 0.7, title = "Rocky Mountain National Park")
    
    
  })
  
  
  
}
```

![](www/shinyPreview.png)

### Sharing your application

To share Shiny applications you can either share the shiny app folder
you created (which has the bundled `app.R` file and any associated data
files) and the user can run it in their own R session, or you can
publicly host your application for free through accounts like
[shinyapps.io](https://www.shinyapps.io/).

Follow [this
tutorial](https://shiny.rstudio.com/articles/shinyapps.html) for
creating a free shinyapps.io account and deploying your shiny
application.

### Further exploration

This was a very basic example of a shiny application. There are a huge
range of layouts, widgets, and outputs to explore in Shiny. Some places
to get started are the Shiny [cheat
sheet](https://shiny.rstudio.com/images/shiny-cheatsheet.pdf) and the
Shiny [gallery](https://shiny.rstudio.com/gallery/) (which includes
links to the raw code that built the application).
