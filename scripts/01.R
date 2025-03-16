# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(DT)
library(tidyr)


data <- read.csv('data/covid_19_clean_complete.csv')

# Clean dataset
data <- data |> 
  filter(!is.na(Confirmed), !is.na(Deaths), !is.na(Recovered), !is.na(Active))

data$Date <- as.Date(data$Date)

# UI part
ui <- fluidPage(
  titlePanel("COVID-19 Data Dashboard"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      # Filter by multiple countries for comparison
      selectInput("compare_countries", "Select Countries to Compare:", 
                  choices = unique(data$Country.Region), 
                  selected = c("Australia", "Canada"), 
                  multiple = TRUE),
      
      # Filter by date range
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(data$Date),
                     end = max(data$Date),
                     min = min(data$Date),
                     max = max(data$Date))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Notes", verbatimTextOutput("summary")),
        tabPanel("Active Cases", 
                 plotlyOutput("comparison_trends")),
        tabPanel("Deaths", 
                 plotlyOutput("comparison_death_trends")),
        tabPanel("Recovery", 
                 plotlyOutput("comparison_recovery_trends")),
        tabPanel("Aggregate Data",
                 plotlyOutput("comparison_aggregate"))
      )
    )
  )
)

# Server part
server <- function(input, output, session) {

  
  # Filtered data for comparison of multiple countries
  comparison_data <- reactive({
    data %>%
      filter(Country.Region %in% input$compare_countries,
             Date >= input$date_range[1],
             Date <= input$date_range[2])

  })
 
  output$summary <- renderText({"Data from :https://www.kaggle.com/datasets/imdevskp/corona-virus-report/code\n\nNote, some country datasets are empty (i.e. Canada Recovered stats), these will not display anything"})
  
  # Render Comparison of Countries (Positive Cases)
  output$comparison_trends <- renderPlotly({
    plot_comparison <- ggplot(comparison_data(), aes(x = Date, y = Confirmed, color = Country.Region)) +
      geom_smooth() +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "1 month") +  # Show individual dates
      labs(title = "Comparison of Positive Cases trends across Countries",
           x = "Date", y = "Positive Cases") +
      theme_minimal()
    ggplotly(plot_comparison)
  })
  output$comparison_recovery_trends <- renderPlotly({
    # Check if data has variation
    has_variation <- comparison_data() %>%
      group_by(Country.Region) %>%
      summarise(var_check = var(Recovered, na.rm = TRUE)) %>%
      mutate(has_var = !is.na(var_check) & var_check > 0)
    
    # Merge with original dataset
    plot_data <- comparison_data() %>%
      left_join(has_variation, by = "Country.Region")
    
    plot_comparison <- ggplot(plot_data, aes(x = Date, y = Recovered, color = Country.Region)) +
      geom_smooth(data = subset(plot_data, has_var), method = "loess", na.rm = TRUE) +  # Apply smoothing only if variation exists
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "1 month") +
      labs(title = "Comparison of Recovery Trends Across Countries",
           x = "Date", y = "Recovered Cases") +
      theme_minimal()
    
    ggplotly(plot_comparison)
  })
  output$comparison_Active_trends <- renderPlotly({
    plot_comparison <- ggplot(comparison_data(), aes(x = Date, y = Active, color = Country.Region)) +
      geom_smooth() +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "1 month") +  # Show individual dates
      labs(title = "Comparison of Active Cases trends across Countries",
           x = "Date", y = "Positive Cases") +
      theme_minimal()
    ggplotly(plot_comparison)
  })
  # Render Comparison of Countries (Deaths)
  output$comparison_death_trends <- renderPlotly({
    plot_comparison <- ggplot(comparison_data(), aes(x = Date, y = Deaths, color = Country.Region)) +
      geom_smooth() +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "1 month") +  # Show individual dates
      labs(title = "Comparison of Death trends across Countries",
           x = "Date", y = "Deaths") +
      theme_minimal()
    ggplotly(plot_comparison)
  })
  # Render Comparison of Countries (Deaths)
  output$comparison_aggregate <- renderPlotly({
    bardata <- comparison_data() |> 
      group_by(Country.Region) %>%
      summarise(
        Total_Active = sum(Active, na.rm = TRUE),
        Total_Recovered = sum(Recovered, na.rm = TRUE),
        Total_Death = sum(Deaths, na.rm = TRUE)
      ) |> 
      pivot_longer(cols = starts_with("Total_"), names_to = "Case_Type", values_to = "Total_Cases")
    
    plot_comparison <- ggplot(bardata, aes(x = Country.Region, y = Total_Cases, fill = Case_Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Aggregate Data Comparison by Country",
           x = "Country", y = "Total Cases") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(plot_comparison)
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server) 
