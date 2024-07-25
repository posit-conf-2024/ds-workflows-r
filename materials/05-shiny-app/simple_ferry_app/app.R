library(shiny)
library(bslib)
library(jsonlite)
library(tibble)

## Define UI ------------------------
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "minty"),
  title = "Seattle Ferries",
  sidebar = sidebar(
    
    # Date
    dateInput("date","Select Date"),
    
    # Input hour
    selectInput(
      "hour",
      "Select Closest Hour",
      choices = 0:23 |> as.character(),
      selected = "0"
    ),
    
    # Route
    selectInput(
      "departing",
      "Select Departing Station",
      choices = c("Southworth", "Vashon Island", "Anacortes"),
      selected = "Anacortes"
    ),
    
    selectInput(
      "arriving",
      "Select Arriving Station",
      choices = c("Bainbridge Island", "Seattle", "Orcas Island"),
      selected = "Bainbridge Island"
    ),
    
    # Weather Code
    selectInput(
      "weather_code",
      "Select Weather Conditions",
      choices = c("0", "1", "2", "3"),
      selected = "0"
    ),
    
    # Wind Status
    radioButtons(
      "windy",
      "Wind Status",
      choices = c("windy", "calm"),
      selected = "calm"
    ),
    
    # Gust Status
    radioButtons(
      "gusty",
      "Gust Status",
      choices = c("gusty", "calm"),
      selected = "calm"
    )
  ),
  card(
    verbatimTextOutput("new_ferry")
  )
)

## Define Server -------------------------
server <- function(input, output, session) {
  
  # Output Text
  output$new_ferry <- renderText({
    new_ferry_data <- tibble(
      departing = input$departing,
      date = input$date,
      weather_code = input$weather_code,
      wind_status = input$windy,
      gust_status = input$gusty,
      hour = input$hour,
      delay = 0
    )
  
    toJSON(new_ferry_data, pretty = TRUE)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
