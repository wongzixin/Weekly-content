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
    tabPanel("Number of Passengers by Country", plotlyOutput(outputId = "barPlot1")),
    tabPanel("Number of Passengers by Year", 
             plotlyOutput(outputId = "barPlot2"),
             plotlyOutput(outputId = "generalGrowthRatePlot")),
    tabPanel("Contribution by Country Each Year", plotlyOutput(outputId = "stackedBarPlot"))
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
  
  ## Prepare dataset for barPlot1
  data_country_total <- final_data %>%
    rowwise() %>%
    mutate(Total = sum(c_across(`2011`:`2022`), na.rm = TRUE)) %>%
    ungroup() %>%
    select(Country, Total) 
  
  ## Prepare dataset for barPlot2 
  data_year_total <- final_data %>%
    summarise(across(`2011`:`2022`, ~sum(.x, na.rm = TRUE))) %>%
    pivot_longer(cols = `2011`:`2022`, names_to = "Year", values_to = "TotalPassengers")
  
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
  
  ## Prepare dataset for stackedBarPlot 
  stacked_data <- final_data %>%
    pivot_longer(cols = -Country, names_to = "Year", values_to = "Passengers") %>%
    group_by(Year, Country) %>%
    mutate(Contribution = Passengers) %>%
    ungroup()
  
  ### Arrange countries by most to least contribution for stackedBarPlot
  stacked_data <- stacked_data %>% 
    arrange(Year, desc(Contribution))
  
  ### Reorder Country factor based on arranged data for stacking
  stacked_data <- stacked_data %>%
    mutate(Country = factor(Country, levels = unique(Country)))
  
  ## Render the bar plot for total number of passengers by country across the years
  output$barPlot1 <- renderPlotly({
    p <- ggplot(data_country_total, aes(x = Country, y = Total, text = paste("Country: ", Country, "<br>Passengers: ", scales::comma(Total)))) +
      geom_bar(stat = "identity", fill = "grey", color = "white") +
      labs(x = "Country", y = "Number of Passengers", title = "Total Number of Passengers by Country From 2011 to 2022") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_log10(labels = scales::comma) 
    ggplotly(p, tooltip = "text")
  })
  
  ## Render the bar plot for total number of passengers each year 
  output$barPlot2 <- renderPlotly({
    p <- ggplot(data_year_total, aes(x = as.factor(Year), y = TotalPassengers, text = paste("Year: ", Year, "<br>Passengers: ", scales::comma(TotalPassengers)))) +
      geom_bar(stat = "identity", fill = "grey", color = "white") +
      labs(x = "Year", y = "Number of Passengers", title = "Total Number of Passengers by Year") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
    ggplotly(p, tooltip = "text")
  })
  
  ## Render the general growth rate plot 
  output$generalGrowthRatePlot <- renderPlotly({
    req(general_growth_rate_data)
    p <- ggplot(general_growth_rate_data, aes(x = Year, y = GrowthRate, group = 1, text = paste("Year: ", Year, "<br>Growth Rate: ", round(GrowthRate, 2), "%"))) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Year-on-Year Growth Rate (%)", title = "Year-on-Year Growth Rate") +
      scale_x_continuous(breaks = 2011:2022) + 
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  ## Render the stacked bar plot for contributions of each country each year 
  output$stackedBarPlot <- renderPlotly({
    p <- ggplot(stacked_data, aes(x = as.factor(Year), y = Passengers, fill = Country,
                                  text = paste("Country: ", Country, 
                                               "<br>Year: ", Year, 
                                               "<br>Passengers: ", scales::comma(Passengers)))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "Year", y = "Number of Passengers", title = "Contribution by Country Each Year") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
      guides(fill = guide_legend(reverse = TRUE))
    ggplotly(p, tooltip = "text")
  })
  
}

# Create Shiny app
shinyApp(ui = ui, server = server)
