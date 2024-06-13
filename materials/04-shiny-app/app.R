library(shiny)
library(bslib)
library(glue)

# Define UI ----
ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar(
                    dateRangeInput("dates","Select date range"),
                    selectInput(
                      "route",
                      "Select route",
                        choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                        selected = 1
                      ),
                    selectInput(
                      "vessel",
                      "Select vessel",
                      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                      selected = 1
                    )
                      
                    ),
  navset_underline(
    nav_panel(title = "Overview",
              card("Map"),
              card("Details",
                   textOutput("table_headline")
              )),
    nav_panel(title = "Delay Predictions")
  )

)

# Define server logic ----
server <- function(input, output) {
  
  output$table_headline <- renderText({
    glue("Summary for {input$route} and {input$vessel}")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)