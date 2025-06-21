# Load necessary libraries
library(shiny)
library(shinydashboard)
library(prophet)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Drink Demand Forecasting"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-line")),
      menuItem("Model Performance", tabName = "performance", icon = icon("chart-bar")),
      menuItem("Export Results", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "Upload Data", status = "primary", solidHeader = TRUE, width = 12,
                  fileInput("file", "Choose Excel File",
                            accept = c(".xlsx", ".xls")),
                  numericInput("sheet", "Sheet Number:", value = 1, min = 1),
                  br(),
                  actionButton("loadData", "Load Data", class = "btn-primary"),
                  br(), br(),
                  conditionalPanel(
                    condition = "output.dataLoaded",
                    h4("Data Preview:"),
                    DT::dataTableOutput("dataPreview")
                  )
                )
              )
      ),
      
      # Forecast Tab
      tabItem(tabName = "forecast",
              fluidRow(
                box(
                  title = "Forecast Parameters", status = "primary", solidHeader = TRUE, width = 4,
                  numericInput("forecastDays", "Forecast Days:", value = 365, min = 1, max = 1095),
                  checkboxInput("includeHolidays", "Include Canadian/Quebec Holidays", value = TRUE),
                  br(),
                  actionButton("runForecast", "Generate Forecast", class = "btn-success"),
                  br(), br(),
                  conditionalPanel(
                    condition = "output.forecastGenerated",
                    h4("Model Performance:"),
                    verbatimTextOutput("modelMAE")
                  )
                ),
                box(
                  title = "Forecast Plot", status = "info", solidHeader = TRUE, width = 8,
                  plotlyOutput("forecastPlot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Forecast Components", status = "info", solidHeader = TRUE, width = 12,
                  plotOutput("componentsPlot", height = "500px")
                )
              )
      ),
      
      # Model Performance Tab
      tabItem(tabName = "performance",
              fluidRow(
                box(
                  title = "Model Diagnostics", status = "warning", solidHeader = TRUE, width = 6,
                  plotOutput("residualsPlot")
                ),
                box(
                  title = "Actual vs Predicted", status = "warning", solidHeader = TRUE, width = 6,
                  plotOutput("actualVsPredicted")
                )
              ),
              fluidRow(
                box(
                  title = "Performance Metrics", status = "info", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("performanceMetrics")
                )
              )
      ),
      
      # Export Results Tab
      tabItem(tabName = "export",
              fluidRow(
                box(
                  title = "Export Options", status = "success", solidHeader = TRUE, width = 12,
                  h4("Download Forecast Results:"),
                  br(),
                  downloadButton("downloadForecast", "Download Forecast CSV", class = "btn-success"),
                  br(), br(),
                  h4("Forecast Summary:"),
                  DT::dataTableOutput("forecastTable")
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    model = NULL,
    forecast = NULL,
    dataLoaded = FALSE,
    forecastGenerated = FALSE
  )
  
  # Create holidays data frame
  holidays <- data.frame(
    holiday = c(rep('New Year Day', 6), rep('Good Friday', 6), rep('National Patriots Day', 6), 
                rep('Saint-Jean-Baptiste Day', 6), rep('Canada Day', 6), rep('Labour Day', 6),
                rep('Thanksgiving', 6), rep('Christmas Day', 6)),
    ds = as.Date(c(
      # New Year's Day
      '2021-01-01', '2022-01-01', '2023-01-01', '2024-01-01', '2025-01-01', '2026-01-01',
      # Good Friday
      '2021-04-02', '2022-04-15', '2023-04-07', '2024-03-29', '2025-04-18', '2026-04-03',
      # National Patriots' Day (Quebec)
      '2021-05-24', '2022-05-23', '2023-05-22', '2024-05-20', '2025-05-19', '2026-05-18',
      # Saint-Jean-Baptiste Day (Quebec)
      '2021-06-24', '2022-06-24', '2023-06-24', '2024-06-24', '2025-06-24', '2026-06-24',
      # Canada Day
      '2021-07-01', '2022-07-01', '2023-07-01', '2024-07-01', '2025-07-01', '2026-07-01',
      # Labour Day
      '2021-09-06', '2022-09-05', '2023-09-04', '2024-09-02', '2025-09-01', '2026-09-07',
      # Thanksgiving
      '2021-10-11', '2022-10-10', '2023-10-09', '2024-10-14', '2025-10-13', '2026-10-12',
      # Christmas Day
      '2021-12-25', '2022-12-25', '2023-12-25', '2024-12-25', '2025-12-25', '2026-12-25'
    )),
    lower_window = 0,
    upper_window = 0
  )
  
  # Load data
  observeEvent(input$loadData, {
    req(input$file)
    
    tryCatch({
      df <- read_excel(input$file$datapath, sheet = input$sheet)
      
      # Check if we have at least 2 columns
      if(ncol(df) < 2) {
        stop("Excel file must have at least 2 columns (Date and Sales)")
      }
      
      # Assume first column is Date and second is Sales
      colnames(df)[1:2] <- c("Date", "Sales")
      
      df <- df %>%
        rename(ds = Date, y = Sales) %>%
        mutate(ds = as.Date(ds)) %>%
        filter(!is.na(ds), !is.na(y)) %>%
        arrange(ds)
      
      # Check if we have any data left after filtering
      if(nrow(df) == 0) {
        stop("No valid data found after processing. Please check your date and sales columns.")
      }
      
      values$data <- df
      values$dataLoaded <- TRUE
      
      showNotification("Data loaded successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
      values$dataLoaded <- FALSE
    })
  })
  
  # Data preview
  output$dataPreview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data, options = list(pageLength = 10))
  })
  
  output$dataLoaded <- reactive({
    values$dataLoaded
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  # Generate forecast
  observeEvent(input$runForecast, {
    req(values$data)
    
    withProgress(message = 'Generating forecast...', value = 0, {
      tryCatch({
        # Fit model
        incProgress(1/3, detail = "Fitting model...")
        
        if (input$includeHolidays) {
          m <- prophet(values$data, holidays = holidays)
        } else {
          m <- prophet(values$data)
        }
        
        # Generate forecast
        incProgress(1/3, detail = "Generating predictions...")
        future <- make_future_dataframe(m, periods = input$forecastDays)
        forecast <- predict(m, future)
        
        # Calculate MAE
        incProgress(1/3, detail = "Calculating performance...")
        comparison_df <- forecast %>%
          select(ds, yhat) %>%
          left_join(values$data, by = "ds") %>%
          filter(!is.na(y))
        
        mae <- mean(abs(comparison_df$y - comparison_df$yhat), na.rm = TRUE)
        
        values$model <- m
        values$forecast <- forecast
        values$mae <- mae
        values$forecastGenerated <- TRUE
        
        showNotification("Forecast generated successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error generating forecast:", e$message), type = "error")
      })
    })
  })
  
  # Forecast plot
  output$forecastPlot <- renderPlotly({
    req(values$model, values$forecast)
    
    p <- plot(values$model, values$forecast) +
      ggtitle("Drink Demand Forecast") +
      xlab("Date") +
      ylab("Drinks Sold") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Components plot
  output$componentsPlot <- renderPlot({
    req(values$model, values$forecast)
    prophet_plot_components(values$model, values$forecast)
  })
  
  # Model MAE
  output$modelMAE <- renderText({
    req(values$mae)
    paste0("Mean Absolute Error (MAE): ", round(values$mae, 2), " drinks\n",
           "This means the model's predictions were, on average, off by ", 
           round(values$mae, 2), " drinks.")
  })
  
  output$forecastGenerated <- reactive({
    values$forecastGenerated
  })
  outputOptions(output, "forecastGenerated", suspendWhenHidden = FALSE)
  
  # Performance plots
  output$residualsPlot <- renderPlot({
    req(values$forecast, values$data)
    
    comparison_df <- values$forecast %>%
      select(ds, yhat) %>%
      left_join(values$data, by = "ds") %>%
      filter(!is.na(y)) %>%
      mutate(residuals = y - yhat)
    
    ggplot(comparison_df, aes(x = ds, y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      ggtitle("Residuals Over Time") +
      xlab("Date") +
      ylab("Residuals") +
      theme_minimal()
  })
  
  output$actualVsPredicted <- renderPlot({
    req(values$forecast, values$data)
    
    comparison_df <- values$forecast %>%
      select(ds, yhat) %>%
      left_join(values$data, by = "ds") %>%
      filter(!is.na(y))
    
    ggplot(comparison_df, aes(x = y, y = yhat)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      ggtitle("Actual vs Predicted Values") +
      xlab("Actual Sales") +
      ylab("Predicted Sales") +
      theme_minimal()
  })
  
  # Performance metrics
  output$performanceMetrics <- renderText({
    req(values$forecast, values$data, values$mae)
    
    comparison_df <- values$forecast %>%
      select(ds, yhat) %>%
      left_join(values$data, by = "ds") %>%
      filter(!is.na(y))
    
    rmse <- sqrt(mean((comparison_df$y - comparison_df$yhat)^2, na.rm = TRUE))
    mape <- mean(abs((comparison_df$y - comparison_df$yhat) / comparison_df$y) * 100, na.rm = TRUE)
    
    paste0("Performance Metrics:\n",
           "Mean Absolute Error (MAE): ", round(values$mae, 2), "\n",
           "Root Mean Square Error (RMSE): ", round(rmse, 2), "\n",
           "Mean Absolute Percentage Error (MAPE): ", round(mape, 2), "%")
  })
  
  # Forecast table
  output$forecastTable <- DT::renderDataTable({
    req(values$forecast)
    
    forecast_summary <- values$forecast %>%
      select(ds, yhat, yhat_lower, yhat_upper) %>%
      tail(input$forecastDays) %>%
      mutate(
        yhat = round(yhat, 2),
        yhat_lower = round(yhat_lower, 2),
        yhat_upper = round(yhat_upper, 2)
      )
    
    colnames(forecast_summary) <- c("Date", "Forecast", "Lower Bound", "Upper Bound")
    
    DT::datatable(forecast_summary, options = list(pageLength = 15))
  })
  
  # Download handler
  output$downloadForecast <- downloadHandler(
    filename = function() {
      paste("drink_demand_forecast_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$forecast)
      
      forecast_to_save <- values$forecast %>%
        select(ds, yhat, yhat_lower, yhat_upper) %>%
        mutate(
          yhat = round(yhat, 2),
          yhat_lower = round(yhat_lower, 2),
          yhat_upper = round(yhat_upper, 2)
        )
      
      colnames(forecast_to_save) <- c("Date", "Forecast", "Lower_Bound", "Upper_Bound")
      
      write.csv(forecast_to_save, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
