# Load required packages
library(shiny)
library(shinyWidgets)
library(googlesheets4)
library(DT)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(plotly)
library(lubridate)
library(writexl)

# Configuration
options(googlesheets4_quiet = TRUE)

# Deauthorize Google Sheets for public access
tryCatch({
  gs4_deauth()
}, error = function(e) {
  message("Deauthorization failed: ", e$message)
})

# Google Sheet URL
SHEET_URL <- "https://docs.google.com/spreadsheets/d/1knHw-PqSkuGObMkHQElkUAzlWWRCQik6eIkTUC1Rypk"

# UI
ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .title-panel { text-align: center; padding: 20px; }
      .status-badge { padding: 5px 10px; border-radius: 12px; }
      .cold { background-color: #3498db; color: white; }
      .warm { background-color: #f1c40f; color: black; }
      .hot { background-color: #e74c3c; color: white; }
      .refresh-btn { margin-top: 10px; }
      .summary-box { padding: 15px; margin: 10px 0; background-color: #f8f9fa; border-radius: 8px; }
    "))
  ),
  
  titlePanel(
    div(class = "title-panel",
        h1("🔍 LinkedIn Lead Dashboard", style = "font-weight: bold;"),
        p("Real-time lead management and analytics", style = "color: #666;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      pickerInput(
        inputId = "status_filter",
        label = "Filter by Lead Status:",
        choices = c("All", "Cold", "Warm", "Hot"),
        selected = "All",
        options = list(
          `live-search` = TRUE,
          style = "btn-primary"
        )
      ),
      dateRangeInput(
        "date_range",
        label = "Filter by Date Range:",
        start = Sys.Date() - 30,
        end = Sys.Date(),
        max = Sys.Date()
      ),
      actionButton("refresh_data", "Refresh Data", 
                   icon = icon("sync"), 
                   class = "btn-block refresh-btn"),
      downloadButton("download_data", "Export to Excel",
                     class = "btn-block refresh-btn"),
      div(class = "summary-box",
          h4("Data Summary"),
          textOutput("last_sync"),
          textOutput("total_leads"),
          textOutput("status_summary")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Lead Table",
                 withSpinner(DTOutput("lead_table"), type = 6)
        ),
        tabPanel("Analytics",
                 fluidRow(
                   column(6, plotlyOutput("status_plot")),
                   column(6, plotlyOutput("time_trend"))
                 )
        )
      ),
      tags$hr(),
      tags$p("Synced hourly via GitHub Actions | Powered by RShiny & Zoho | © 2025")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values for caching
  rv <- reactiveValues(
    lead_data = NULL,
    last_refresh = Sys.time()
  )
  
  # Data loading with error handling and caching
  fetch_data <- reactive({
    req(SHEET_URL)
    tryCatch({
      df <- read_sheet(SHEET_URL) %>%
        mutate(Date = as.Date(Timestamp)) %>%
        filter(!is.na(Status) & Status %in% c("Cold", "Warm", "Hot"))
      
      validate(
        need("Status" %in% colnames(df), "Missing 'Status' column in Google Sheet."),
        need("Timestamp" %in% colnames(df), "Missing 'Timestamp' column in Google Sheet."),
        need(nrow(df) > 0, "No data available in Google Sheet."),
        need(any(!is.na(df$Date)), "No valid dates found in 'Timestamp' column.")
      )
      
      rv$lead_data <- df
      rv$last_refresh <- Sys.time()
      df
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = 10
      )
      NULL
    })
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(rv$lead_data)
    df <- rv$lead_data
    
    if (input$status_filter != "All") {
      df <- df %>% filter(Status == input$status_filter)
    }
    
    df <- df %>% 
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    
    df
  })
  
  # Refresh data on button click
  observeEvent(input$refresh_data, {
    showNotification("Refreshing data...", type = "message")
    fetch_data()
  })
  
  # Auto-refresh every 30 minutes
  observe({
    invalidateLater(30 * 60 * 1000) # 30 minutes in milliseconds
    fetch_data()
  })
  
  # Data table output
  output$lead_table <- renderDT({
    req(filtered_data())
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(
          list(
            targets = which(colnames(filtered_data()) == "Status") - 1,
            render = JS(
              "function(data, type, row, meta) {
                if (type === 'display') {
                  var className = data.toLowerCase();
                  return '<span class=\"status-badge ' + className + '\">' + data + '</span>';
                }
                return data;
              }"
            )
          )
        )
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  # Summary outputs
  output$last_sync <- renderText({
    paste("Last synced:", format(rv$last_refresh, "%Y-%m-%d %H:%M:%S"))
  })
  
  output$total_leads <- renderText({
    req(filtered_data())
    paste("Total leads:", nrow(filtered_data()))
  })
  
  output$status_summary <- renderText({
    req(filtered_data())
    summary <- filtered_data() %>%
      count(Status) %>%
      mutate(text = paste(Status, ":", n)) %>%
      pull(text) %>%
      paste(collapse = " | ")
    paste("Status breakdown:", summary)
  })
  
  # Plotly visualizations
  output$status_plot <- renderPlotly({
    req(filtered_data())
    
    plot_data <- filtered_data() %>%
      count(Status) %>%
      mutate(color = case_when(
        Status == "Cold" ~ "#3498db",
        Status == "Warm" ~ "#f1c40f",
        Status == "Hot" ~ "#e74c3c"
      ))
    
    plot_ly(
      data = plot_data,
      x = ~Status,
      y = ~n,
      type = "bar",
      marker = list(color = ~color)
    ) %>%
      layout(
        title = "Lead Distribution by Status",
        xaxis = list(title = "Status"),
        yaxis = list(title = "Count"),
        showlegend = FALSE
      )
  })
  
  output$time_trend <- renderPlotly({
    req(filtered_data())
    
    plot_data <- filtered_data() %>%
      mutate(Week = floor_date(Date, "week")) %>%
      count(Week, Status) %>%
      complete(Week, Status, fill = list(n = 0))
    
    plot_ly(
      data = plot_data,
      x = ~Week,
      y = ~n,
      color = ~Status,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = "Lead Trends Over Time",
        xaxis = list(title = "Week"),
        yaxis = list(title = "Count"),
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("leads_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(filtered_data(), file)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
