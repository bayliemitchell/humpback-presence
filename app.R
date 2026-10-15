#shiny web app dev
library(shiny); library(leaflet); library(leaflet.extras)
library(plotly); library(dplyr) ; library(lubridate)


# Load and prepare data
occ_clean <- readRDS("data/occ_clean.rds") |>
  mutate(
    date  = as.Date(eventDate),
    year  = year(date),
    month = month(date, label = TRUE, abbr = TRUE)
  ) |>
  filter(!is.na(date))

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  titlePanel("Humpback whale occurrence explorer (2020–2026)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("years", "Year range",
                  min = 2020, max = 2026,
                  value = c(2020, 2026), sep = ""),
      
      radioButtons("view", "Map layer",
                   choices = c("Points", "Heatmap"), inline = TRUE),
      
      hr(),
      
      p(style = "font-size:12px; color:grey;",
        "Data: GBIF occurrence download (DwC-A). Presences only —
         empty ocean does not imply absence. Sampling reflects
         observer effort, not true animal density."),
      
      downloadButton("dl", "Download filtered CSV")
    ),
    
    mainPanel(
      leafletOutput("map", height = 520),
      br(),
      plotlyOutput("seasonal", height = 240)
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive filtered dataset
  filtered <- reactive({
    occ_clean |>
      filter(year >= input$years[1],
             year <= input$years[2])
  })
  
  # Base map (renders once)
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("Esri.OceanBasemap") |>
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  # Update map layer when filter or view changes
  observe({
    df <- filtered()
    
    proxy <- leafletProxy("map") |>
      clearMarkers() |>
      clearHeatmap()
    
    if (nrow(df) == 0) return()
    
    if (input$view == "Points") {
      proxy |> addCircleMarkers(
        data       = df,
        lng        = ~decimalLongitude,
        lat        = ~decimalLatitude,
        radius     = 3,
        color      = "#1a7abf",
        stroke     = FALSE,
        fillOpacity = 0.5,
        popup      = ~paste0(
          "<b>Date:</b> ", date, "<br>",
          "<b>Coords:</b> ", round(decimalLatitude, 3),
          ", ", round(decimalLongitude, 3)
        )
      )
    } else {
      proxy |> addHeatmap(
        data    = df,
        lng     = ~decimalLongitude,
        lat     = ~decimalLatitude,
        blur    = 20,
        max     = 0.05,
        radius  = 15
      )
    }
  })
  
  # Seasonal bar chart
  output$seasonal <- renderPlotly({
    month_levels <- c("Jan","Feb","Mar","Apr","May","Jun",
                      "Jul","Aug","Sep","Oct","Nov","Dec")
    
    filtered() |>
      count(month) |>
      mutate(month = factor(month, levels = month_levels)) |>
      arrange(month) |>
      plot_ly(x = ~month, y = ~n, type = "bar",
              marker = list(color = "#1a7abf")) |>
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Occurrences"),
        margin = list(t = 10, b = 40)
      )
  })
  
  # CSV download
  output$dl <- downloadHandler(
    filename = function() paste0("humpback_", input$years[1],
                                 "_", input$years[2], ".csv"),
    content  = function(f) write.csv(filtered(), f, row.names = FALSE)
  )
}

shinyApp(ui, server)
