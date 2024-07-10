library(shiny)
library(bslib)
library(glue)
library(ferryfairy)
library(tidyverse)
library(pins)
library(vetiver)
library(leaflet)
library(leaflet.extras2)
library(thematic)
library(shinycssloaders)

# Read Ferry Data Pin
board <- board_connect(auth = "envvar")
ferry_data <- pin_read(board, "katie.masiello/vesselhistory_w_weather") |> 
  as_tibble() |> 
  mutate(arriving = str_to_title(str_replace_all(arriving, "_", " ")),
         departing = str_to_title(str_replace_all(departing, "_", " ")))

# Get station lat/long
station_latlong <- ferryfairy::get_terminalinfo()

# Weather Code Info
weather_codes <- list(
"Clear" = "0",
"Mostly Clear" = "1",
"Partly Cloudy" = "2",
"Cloudy" = "3",
"Fog" = "45",
"Freezing Fog" = "48",
"Light Drizzle" = "51",
"Drizzle" = "53",
"Heavy Drizzle" = "55",
"Light Freezing Drizzle" = "56",
"Freezing Drizzle" = "57",
"Light Rain" = "61",
"Rain" = "63",
"Heavy Rain" = "65",
"Light Freezing Rain" = "66",
"Freezing Rain" = "67",
"Light Snow" = "71",
"Snow" = "73",
"Heavy Snow" = "75",
"Snow Grains" = "77",
"Light Rain Shower" = "80",
"Rain Shower" = "81",
"Heavy Rain Shower" = "82",
"Snow Shower" = "85",
"Heavy Snow Shower" = "86",
"Thunderstorm" = "95",
"Hailstorm" = "96",
"Heavy Hailstorm" = "99"
)

# Only include weather codes found in data
weather_codes_ferry_data <- weather_codes[weather_codes %in% as.character(unique(ferry_data$weather_code))]

# Define UI ----------------------------------------
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
                        choices = sort(unique(ferry_data$departing)),
                        selected = "Anacortes"
                      ),
                    
                    selectInput(
                      "arriving",
                      "Select Arriving Station",
                      choices = NULL
                    ),
                    
                    # Weather Code
                    selectInput(
                      "weather_code",
                      "Select Weather Conditions",
                      choices = weather_codes_ferry_data,
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
  
  navset_underline(
    nav_panel(title = "Overview",
              br(),
              withSpinner(
                uiOutput("delay_status_box"),
                proxy.height = "10px",
                type = 7
                ),

              # Map
              card("Map",
                   leafletOutput("map")
              )),
    nav_panel(title = "Delay History",
              br(),
              plotOutput("delay_hist"))
  )

)

# Define server logic ----
server <- function(input, output, session) {
  
  observeEvent(input$departing, {
      updateSelectInput(session, "arriving", 
                        choices = sort(unique(ferry_data |> 
                                                filter(departing == input$departing) |> 
                                                pull(arriving))))
    })
  
  # Get departing and arriving lat/long
  station_lat <- reactive({
    depart_lat <- station_latlong |> 
      filter(terminal_name == input$departing) |>
      pull(latitude)
    arrive_lat <- station_latlong |> 
      filter(terminal_name == input$arriving) |>
      pull(latitude)
    c(depart_lat, arrive_lat)
  })
  
  station_long <- reactive({
    depart_long <- station_latlong |> 
      filter(terminal_name == input$departing) |>
      pull(longitude)
    arrive_long <- station_latlong |> 
      filter(terminal_name == input$arriving) |>
      pull(longitude)
    c(depart_long, arrive_long)
  })
  
  # Predict delay
  delay_status <- reactive({
    endpoint <- vetiver_endpoint(paste0("https://connect.posit.it/content/0b0a63a0-ec5b-4ecb-bee3-4e7256249981", "/predict"))
    
    # New ferry data point
    new_ferry_data <- tibble(
      departing = input$departing,
      date = input$date,
      weather_code = input$weather_code,
      wind_status = input$windy,
      gust_status = input$gusty,
      hour = input$hour,
      delay = 0
    )
    
    predict(endpoint, 
            new_ferry_data, 
            httr::add_headers(Authorization = paste("Key", 
                                                    Sys.getenv("CONNECT_API_KEY"))))$.pred_class
  })
  
  delay_color <- reactive({
    if (delay_status() == "delayed") {
      "danger"
    } else {
      "success"
    }
  })
  
  # Create a leaflet map
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles() |>  # Add default OpenStreetMap map tiles
      addAntpath(
        lng = station_long(),  # Longitude coordinates
        lat = station_lat(),  # Latitude coordinates
        weight = 4,         # Line weight
        color = "blue"
      )
  })
  
  # Output filtered data
  output$filtered_data <- renderTable({
    filtered_data()
  })
  
  # Output value box value for delay status
  output$delay_status_box <- renderUI({
    
    Sys.sleep(1)
    
    value_box(title = "Predicted Delay Status",
              value = str_to_title(delay_status()),
              theme = delay_color())
  })
  
  # Create delay histogram
  avg_delay <- reactive({
    ferry_data |> 
      filter(departing == input$departing) |> 
      filter(arriving == input$arriving) |> 
      filter(delay > 0) |> 
      pull(delay) |> 
      mean()
  })
  
  output$delay_hist <- renderPlot({
    ferry_data |> 
      filter(departing == input$departing) |> 
      filter(arriving == input$arriving) |>
      filter(delay > 0) |> 
      ggplot(aes(x = delay)) +
      geom_histogram(bins = 50, fill = "#56cc9d", color = "black") +
      geom_vline(xintercept = avg_delay(), 
                 color = "#ff7851", 
                 linewidth = 1.5, 
                 linetype = "dashed") +
      annotate("text", x = avg_delay(), y = Inf, hjust = -0.1, vjust = 2,
               label = glue("Avg Delay: {round(avg_delay(), 2)} min"), 
               color = "#ff7851", size = 6, ) +
      labs(title = "Delay Distribution",
           subtitle = glue("{input$departing} --> {input$arriving}"),
           x = "Delay (minutes)",
           y = "Frequency") +
      xlim(0, 60) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 25),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        plot.subtitle = element_text(size = 20)
      )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)