library(shiny)
library(forecast)
library(dplyr)

# Generate SARIMA(0,1,1)(0,0,1)[12] series
#set.seed(123)
tickets_series <- ts(arima.sim(model = list(order = c(0, 1, 0), seasonal = list(order = c(0, 0, 1), period = 12)), n = 36), 
                     frequency = 12, start = c(2022, 1))
production_series <- ts(arima.sim(model = list(order = c(0, 1, 0), seasonal = list(order = c(0, 0, 1), period = 12)), n = 36), 
                        frequency = 12, start = c(2022, 1))
rates_series <- ts(arima.sim(model = list(order = c(0, 1, 0), seasonal = list(order = c(0, 0, 1), period = 12)), n = 36), 
                        frequency = 12, start = c(2022, 1))
# Combine into a data frame
data <- data.frame(
  Date = seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = 37),
  Production = as.numeric(production_series),
  Tickets = as.numeric(tickets_series),
  Rates = as.numeric(rates_series)
)

#plot(tickets_series)

# Define UI
ui <- fluidPage(
  titlePanel("Forecast with SARIMA(0,1,1)(1,0,0) Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("series", "Select Time Series:", choices = c("Production", "Tickets", "Rates")),
      numericInput("forecast_horizon", "Forecast Horizon (Months):", value = 3, min = 1),
      uiOutput("scaling_inputs_ticket"),# Dynamic scaling inputs
      uiOutput("scaling_inputs_rates"),# Dynamic scaling inputs
      actionButton("forecast_btn", "Generate Forecast")
    ),
    mainPanel(
      plotOutput("forecast_plot"),
      tableOutput("forecast_table")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive for generating scaling inputs dynamically
  output$scaling_inputs_ticket <- renderUI({
    lapply(1:input$forecast_horizon, function(i) {
      numericInput(paste0("scaling_factor_ticket", i), 
                   label = paste("Scaling Factor Ticket for Month", i, ":"), 
                   value = 1, min = 0.5, max = 1.5, step = 0.1)
    })
  })
  
  output$scaling_inputs_rates <- renderUI({
    lapply(1:input$forecast_horizon, function(i) {
      numericInput(paste0("scaling_factor_rates", i), 
                   label = paste("Scaling Factor Rates for Month", i, ":"), 
                   value = 1, min = 0.5, max = 1.5, step = 0.1)
    })
  })
  
  # Reactive for collecting scaling factors
  scaling_factors_ticket <- reactive({
    sapply(1:input$forecast_horizon, function(i) {
      input[[paste0("scaling_factor_ticket", i)]]
    })
  })
  
  scaling_factors_rates <- reactive({
    sapply(1:input$forecast_horizon, function(i) {
      input[[paste0("scaling_factor_rates", i)]]
    })
  })
  
  # Reactive for storing Tickets forecast with scaling factors
  tickets_forecast <- reactive({
    # Fit ARIMA model to the Tickets time series
    tickets_fit <- auto.arima(ts(data$Tickets, frequency = 12, start = c(2022, 1)))
    
    # Generate raw forecast
    raw_forecast <- forecast(tickets_fit, h = input$forecast_horizon)$mean
    
    # Apply dynamic scaling factors
    scaled_forecast <- raw_forecast * scaling_factors_ticket()
    
    # Log the forecast values for debugging
    print("Tickets Forecast (Scaled):")
    print(scaled_forecast)
    
    # Return the scaled forecast
    return(scaled_forecast)
  })
  
  # Reactive for storing Rates forecast with scaling factors
  rates_forecast <- reactive({
    # Fit ARIMA model to the Tickets time series
    rates_fit <- auto.arima(ts(data$Rates, frequency = 12, start = c(2022, 1)))
    
    # Generate raw forecast
    raw_forecast <- forecast(rates_fit, h = input$forecast_horizon)$mean
    
    # Apply dynamic scaling factors
    scaled_forecast_r <- raw_forecast * scaling_factors_rates()
    
    # Log the forecast values for debugging
    print("Rates Forecast (Scaled):")
    print(scaled_forecast_r)
    
    # Return the scaled forecast
    return(scaled_forecast_r)
  })
  
  
  # Reactive for Production forecast using stored and scaled Tickets forecast
  production_forecast <- reactive({
    # Fit ARIMA model to the Production time series with Tickets as external regressor
    
    tickets_forecast_scaled <- tickets_forecast()
    rates_forecast_scaled <- rates_forecast()
    x_reg_fit <- data%>%select(Tickets, Rates)
    xreg_forecast <- data.frame(Tickets = tickets_forecast_scaled, Rates = rates_forecast_scaled)

    
    production_fit <- auto.arima(ts(data$Production, frequency = 12, start = c(2022, 1)),
                                 xreg = as.matrix(x_reg_fit))
    
    # Forecast Production using Tickets forecast as external regressor
    prod_forecast <- forecast(production_fit, h = input$forecast_horizon, xreg = as.matrix(xreg_forecast))
    
    # Log the forecast values for debugging
    print("Production Forecast:")
    print(xreg_forecast)
    print(prod_forecast)
    
    return(prod_forecast)
  })  
  # Plot Forecast
  output$forecast_plot <- renderPlot({
    req(input$forecast_btn)
    if (input$series == "Tickets") {
      tick_forecast <- tickets_forecast()
      plot(tick_forecast, main = "Ticket Forecast")
    } else {
      prod_forecast <- production_forecast()
      plot(prod_forecast, main = "Production Forecast Using Adjusted Tickets")
    }
  })
  
  # Display Forecast Table
  output$forecast_table <- renderTable({
    req(input$forecast_btn)
    if (input$series == "Tickets") {
      data.frame(
        Date = seq(max(data$Date) + 1, by = "month", length.out = input$forecast_horizon),
        Forecast = tickets_forecast()
      )
    } else {
      prod_forecast <- production_forecast()
      data.frame(
        Date = seq(max(data$Date) + 1, by = "month", length.out = input$forecast_horizon),
        Forecast = prod_forecast$mean
      )
    }
  })
}

# Run the App
shinyApp(ui = ui, server = server)
