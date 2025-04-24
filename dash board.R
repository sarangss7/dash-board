# Install and load necessary packages
install.packages(c("shiny", "shinydashboard", "plotly", "ggplot2", "dplyr"))
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)

# Load the dataset (replace with your dataset URL or file)
data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv")

# Create UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Gapminder Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Data View", tabName = "dataView", icon = icon("table")),
      # Dropdown for selecting continent
      selectInput("continent", "Select Continent:", choices = unique(data$continent), selected = "Asia"),
      # Slider for selecting year
      sliderInput("year", "Select Year:",
                  min = min(data$year), max = max(data$year),
                  value = max(data$year), step = 5),
      # Dropdown for selecting trend (GDP per Capita or Life Expectancy)
      selectInput("trend", "Select Trend to View:", choices = c("GDP per Capita", "Life Expectancy"))
    )
  ),
  dashboardBody(
    # Add custom CSS for styling
    tags$head(
      tags$style(HTML("
        .box {border-radius: 8px;}
        .box-header {background-color: #2e3b4e; color: white; font-size: 18px;}
        .box-body {background-color: #f8f9fa;}
        .content-wrapper {background-color: #e9ecef;}
        .skin-blue .main-header .navbar {background-color: #005f8d;}
        .skin-blue .main-sidebar {background-color: #002c3d;}
      "))
    ),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "GDP Per Capita Over Time", status = "primary", solidHeader = TRUE, 
                    width = 6, 
                    plotlyOutput("gdp_plot")),
                box(title = "Population Distribution by Country", status = "warning", solidHeader = TRUE, 
                    width = 6, 
                    plotlyOutput("population_plot"))
              ),
              fluidRow(
                box(title = "GDP vs Life Expectancy", status = "success", solidHeader = TRUE,
                    width = 12, 
                    plotlyOutput("gdp_life_plot"))
              )
      ),
      tabItem(tabName = "dataView",
              fluidRow(
                box(title = "Gapminder Data", status = "info", solidHeader = TRUE, 
                    width = 12, 
                    dataTableOutput("data_table"))
              )
      )
    )
  )
)

# Create server logic
server <- function(input, output) {
  
  # Filter the data based on the selected year and continent
  filtered_data <- reactive({
    data %>%
      filter(year == input$year, continent == input$continent)
  })
  
  # GDP Per Capita Over Time Plot (Using ggplot2 and plotly)
  output$gdp_plot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = year, y = gdpPercap, color = continent)) +
      geom_line() +
      labs(title = paste("GDP Per Capita Over Time in", input$continent), x = "Year", y = "GDP per Capita") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, color = "blue", size = 16),
        axis.title = element_text(color = "red")
      )
    ggplotly(p)
  })
  
  # Population Distribution by Country Plot (Using plotly)
  output$population_plot <- renderPlotly({
    df <- filtered_data()
    
    plot_ly(df, x = ~country, y = ~pop, color = ~continent, type = "bar") %>%
      layout(title = paste("Population Distribution in", input$continent, "in", input$year),
             xaxis = list(title = "Country"), yaxis = list(title = "Population"))
  })
  
  # GDP vs Life Expectancy Scatter Plot (Using plotly)
  output$gdp_life_plot <- renderPlotly({
    df <- filtered_data()
    
    plot_ly(df, x = ~gdpPercap, y = ~lifeExp, color = ~continent, type = "scatter", mode = "markers") %>%
      layout(title = paste("GDP vs Life Expectancy in", input$continent, "in", input$year),
             xaxis = list(title = "GDP per Capita"), yaxis = list(title = "Life Expectancy"))
  })
  
  # Render the data table
  output$data_table <- renderDataTable({
    filtered_data()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
