# Load libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(scales)
library(tidyverse)
library(forecast)

# Define UI
ui <- fluidPage(
  titlePanel("Data Story"),
  tabsetPanel(
    tabPanel("Predicted Growth Rates", plotlyOutput(outputId = "predictedGrowthRatePlot"))
  )
)

# Define server logic
server <- function(input, output, session) { 
  
  ##Cleaning, tidying and filtering original dataset 
  ### Mapping for country names
  country_mapping <- c(
    AT = "Austria", BE = "Belgium", BG = "Bulgaria", CY = "Cyprus",
    CZ = "Czech Republic", DE = "Germany", DK = "Denmark", EE = "Estonia",
    EL = "Greece", ES = "Spain", FI = "Finland", FR = "France",
    HR = "Croatia", HU = "Hungary", IE = "Ireland", IT = "Italy",
    LT = "Lithuania", LU = "Luxembourg", LV = "Latvia", MT = "Malta",
    NL = "Netherlands", PL = "Poland", PT = "Portugal", RO = "Romania",
    SE = "Sweden", SI = "Slovenia", SK = "Slovakia"
  )
  
  ### Reading data, selecting variables and filtering out unwanted countries
  data <- read.csv("EU27_passenger_data.csv")
  filtered_data <- data %>%
    select(TIME_PERIOD, OBS_VALUE, geo) %>%
    filter(!geo %in% c("BA", "CH", "EU27_2020", "IS", "ME", "MK", "NO", "RS", "UK"))
  
  ### Map geo to country names
  filtered_data <- filtered_data %>%
    mutate(Country = recode(geo, !!!country_mapping))
  
  ### Present data in wide format 
  final_data <- filtered_data %>%
    pivot_wider(
      names_from = TIME_PERIOD,
      values_from = OBS_VALUE,
      id_cols = Country
    )
  
  ## Prepare dataset for general growth rates 
  general_growth_rate_data <- final_data %>%
    pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.numeric(str_remove(Year, "X"))) %>%
    arrange(Year) %>%
    group_by(Year) %>%
    summarize(Value = mean(Value, na.rm = TRUE)) %>%
    mutate(GrowthRate = (Value/lag(Value) - 1) * 100) %>%
    filter(!is.na(GrowthRate)) %>%
    ungroup()
  
  ## Prepare dataset for general predicted growth rates
  ### Growth rate until 2019
  historical_data <- general_growth_rate_data %>%
    select(Year, GrowthRate) %>%
    filter(Year <= 2019)  # Ensure that we only select data up to 2019
  
  ### Create a time index for the historical data
  time_index <- seq_along(historical_data$GrowthRate)
  
  ### Fit a linear regression model
  lm_model <- lm(GrowthRate ~ time_index, data = historical_data)
  
  ### Create a time index for the future predictions
  future_time_index <- (max(time_index) + 1):(max(time_index) + 8)
  
  ### Make predictions for the next 8 values
  predicted_values <- predict(lm_model, newdata = data.frame(time_index = future_time_index))
  
  ### Combine the historical data with the predicted values
  future_years <- (max(historical_data$Year) + 1):(max(historical_data$Year) + 8)
  predicted_growth_rates <- data.frame(Year = future_years, GrowthRate = predicted_values)
  
  ### Combine historical and predicted data
  growth_data <- rbind(historical_data, predicted_growth_rates)
  
  ## Render the general predicted growth rate plot
  output$predictedGrowthRatePlot <- renderPlotly({
    req(growth_data)
    p <- ggplot(growth_data, aes(x = Year, y = GrowthRate, group = 1, text = paste("Year: ", Year, "<br>Growth Rate: ", round(GrowthRate, 2), "%"))) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Year-on-Year Growth Rate (%)", title = "Predicted Growth Rate") +
      scale_x_continuous(breaks = min(growth_data$Year):max(growth_data$Year)) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
}

# Create Shiny app
shinyApp(ui = ui, server = server)
