library(shiny)
if (!exists("all_cities")) all_cities = readRDS("data/cities.rds")
if (!exists("usa_cities")) usa_cities = readRDS("data/usa_cities.rds")

shinyUI(fluidPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom_styles.css")
  ),
  
  title = "Traveling Salesman with Tabu Search Algorithm, Shiny, and R",
  
  tags$h2(tags$a(href="/traveling-salesman", "Traveling Salesman", target="_blank")),
  
  plotOutput("map", height="550px"),
  
  fluidRow(
    column(5,
      tags$ol(
        tags$li("Customize the list of cities, based on the world or US map"),
        tags$li("Adjust tabu search parameters to taste"),
        tags$li("Click the 'solve' button!")
      )
    ),
    column(3,
      tags$button("SOLVE", id="go_button", class="btn btn-info btn-large action-button shiny-bound-input")
    ),
    column(3,
      HTML("<button id='set_random_cities_2' class='btn btn-large action-button shiny-bound-input'>
              <i class='fa fa-refresh'></i>
              Set Cities Randomly
            </button>")
    ), class="aaa"
  ),
  
  hr(),
  
  fluidRow(
    column(5,
      h4("Choose a map and which cities to tour"),
      selectInput("map_name", NA, c("World", "USA"), "World", width="100px"),
      p("Type below to select individual cities, or", actionButton("set_random_cities", "set randomly", icon=icon("refresh"))),
      selectizeInput("cities", NA, all_cities$full.name, multiple=TRUE, width="100%",
                     options = list(maxItems=30, maxOptions=100, placeholder="Start typing to select some cities...",
                                    selectOnTab=TRUE, openOnFocus=FALSE, hideSelected=TRUE)),
      checkboxInput("label_cities", "Label cities on map?", FALSE)
    ),
    
    column(2,
      h4("Tabu Searching Parameters"),
      inputPanel(
        numericInput("total_iterations", "Number of Iterations to Run", 50, min=0, max=1000),
        numericInput("plot_every_iterations", "Draw Map Every N Iterations", 5, min=1, max=1000),
        numericInput("tabu_expiration", "Choose the Tabu expiration (in numbers of iterations)", 5, min=1, max=50)
      ),
      class="numeric-inputs"
    ),
    
    column(5,
      plotOutput("distance_results", height="260px")
    )
  )
))
