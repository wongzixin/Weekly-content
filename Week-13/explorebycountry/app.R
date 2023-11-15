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
    tabPanel("Explore by Country", 
             uiOutput("selectCountry"), 
             plotlyOutput(outputId = "barPlot3"),
             plotlyOutput(outputId = "growthRatePlot"),
             plotlyOutput(outputId = "predictedGrowthPlot"))
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
  
  ## Define utility functions for growth rate by country and predicted growth rate 
  ### Calculate growth rate by country
  growth_rate <- function(data) {
    data %>%
      pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value") %>%
      mutate(Year = as.numeric(str_remove(Year, "X"))) %>%
      arrange(Year) %>%
      group_by(Country) %>%
      mutate(GrowthRate = (Value/lag(Value) - 1) * 100) %>%
      filter(!is.na(GrowthRate)) %>% 
      ungroup()
  }
  
  #### Reactive expression for selected country growth rates 
  selected_country_growth_rate <- reactive({
    req(input$selectedCountry)
    growth_rate_data <- final_data %>%
      filter(Country == input$selectedCountry)
    growth_rate(growth_rate_data)
  })
  
  # Define the function to calculate the predicted growth rate with intervention
  ### Calculate predicted growth rate
  predict_growth_rate <- function(data) {
    
    #### Fit a linear regression model 
    model <- lm(Value ~ Year, data = data) 
    
    #### Create new data frame for prediction years 2023-2027
    new_data <- data.frame(Year = 2023:2027)  
    
    #### Predict the values for these years
    prediction <- predict(model, new_data)
    new_data$PredictedValue = prediction
    
    #### Calculate growth rate for these predictions
    last_known_value <- tail(data$Value, 1)
    new_data <- new_data %>% 
      mutate(GrowthRate = if_else(row_number() == 1, 
                                  (PredictedValue/last_known_value - 1) * 100, 
                                  (PredictedValue/lag(PredictedValue) - 1) * 100)) %>% 
      filter(!is.na(GrowthRate))
    
    return(new_data)
  }
  
  ### Reactive expression for selected countries' predicted growth rates 
  predicted_growth_rates <- reactive({
    req(input$selectedCountry)
    country_data <- final_data %>%
      filter(Country == input$selectedCountry) %>%
      pivot_longer(cols = `2020`:`2022`, names_to = "Year", values_to = "Value") %>%
      mutate(Year = as.numeric(Year)) %>%
      arrange(Year)
    predict_growth_rate(country_data)  
  })
  
  ## Ensure the selectInput is updated after the data is prepared
  output$selectCountry <- renderUI({
    sorted_countries <- sort(unique(final_data$Country))
    selectInput("selectedCountry", "Select a Country:", choices = sorted_countries)
  })
  
  ## Render the line graph for total number of passengers for the selected country 
  output$barPlot3 <- renderPlotly({
    req(input$selectedCountry)  
    
    ### Pivot data from wide to long
    country_data <- final_data %>%
      filter(Country == input$selectedCountry) %>%
      pivot_longer(
        cols = -Country, 
        names_to = "Year",
        values_to = "Value"
      ) %>%
      mutate(
        Year = as.numeric(Year), # Convert Year to numeric
        Value = as.numeric(Value) # Ensure Value is numeric
      ) %>%
      arrange(Year)
    p <- ggplot(country_data, aes(x = as.numeric(Year), y = Value, group = 1, text = paste("Year: ", Year, "<br>Passengers: ", scales::comma(Value)))) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Number of Passengers", title = paste("Number of Passengers over the Years -", input$selectedCountry)) +
      scale_x_continuous(breaks = 2011:2022) +
      scale_y_log10(labels = scales::comma) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  ## Render the growth rate plot for the selected country
  output$growthRatePlot <- renderPlotly({
    req(selected_country_growth_rate())
    growth_data <- selected_country_growth_rate()
    year_range <- range(growth_data$Year)
    p <- ggplot(growth_data, aes(x = Year, y = GrowthRate, group = 1, text = paste("Year: ", Year, "<br>Growth Rate: ", round(GrowthRate, 2), "%"))) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Year-on-Year Growth Rate (%)", title = paste("Year-on-Year Growth Rate for", input$selectedCountry)) +
      scale_x_continuous(breaks = 2011:2022) + 
      theme_minimal()
    ggplotly(p, tooltip = "text")
  }) 
  
  ## Render the prediction growth rate plot for the selected country
  output$predictedGrowthPlot <- renderPlotly({
    req(predicted_growth_rates())
    predicted_data <- predicted_growth_rates()
    p <- ggplot(predicted_data, aes(x = Year, y = GrowthRate, group = 1, text = paste("Year: ", Year, "<br>Predicted Growth Rate: ", round(GrowthRate, 2), "%"))) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Predicted Growth Rate (%)", title = paste("Predicted Growth Rate for", input$selectedCountry, "for 2023-2027")) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
}

# Create Shiny app
shinyApp(ui = ui, server = server)
