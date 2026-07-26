library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Water Level & LNAPL Hydrographs"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Data file", accept = c(".csv")),
      selectInput("location", "Location", choices = NULL),
      dateRangeInput("date_range", "Date range", start = NA, end = NA)
    ),
    mainPanel(
      plotOutput("hydrograph")
    )
  )
)

server <- function(input, output, session) {
  hydro_data <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath, stringsAsFactors = FALSE)
  })

  observeEvent(hydro_data(), {
    updateSelectInput(session, "location", choices = unique(hydro_data()[[1]]))
  })

  output$hydrograph <- renderPlot({
    df <- hydro_data()
    ggplot(df, aes(x = .data[[names(df)[2]]], y = .data[[names(df)[3]]])) +
      geom_line() +
      geom_point() +
      labs(x = NULL, y = NULL) +
      theme_minimal()
  })
}

shinyApp(ui, server)
