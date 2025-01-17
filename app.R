# Title: Gapminder
# Author: Allan Ilyasov
# Different visualizations from the Gapminder dataset to reveal 
# global trends in life expectancy, population growth, and economic development.
#
library(shiny)
library(gapminder)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)


data <- subset(gapminder, year > 1950)
data_grouped <- data %>%
  group_by(continent, year) %>%
  summarise(
    avgLifeExp = mean(lifeExp, na.rm = TRUE),
    avgGdpPercap = mean(gdpPercap, na.rm = TRUE),
    totalPop = sum(pop, na.rm = TRUE)
  ) %>%
  ungroup()

write.csv(data_grouped, "gapminder_cleaned.csv")
ui <- fluidPage(
  titlePanel("Gapminder Data Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select continent
      selectInput(
        inputId = "continent",
        label = "Select Continent:",
        choices = c("All", levels(data$continent)),
        selected = "All"
      ),
      
      # Slider for selecting year range
      sliderInput(
        inputId = "year_range",
        label = "Select Year Range:",
        min = min(data$year),
        max = max(data$year),
        value = c(1960, 2000), # Default range
        step = 5,
        sep = ""
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Line Chart (Interactive)", plotlyOutput("line_plot")),
        tabPanel("Scatter Plot (Interactive)", plotlyOutput("scatter_plot")),
        tabPanel("Bar Chart (Interactive)", plotlyOutput("bar_plot")),
        tabPanel("Box Plot (Interactive)", plotlyOutput("box_plot")),
        tabPanel("Faceted Plot", plotOutput("facet_plot")),
        tabPanel("Density Plot", plotOutput("density_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    d <- data
    if (input$continent != "All") {
      d <- d %>% filter(continent == input$continent)
    }
    d <- d %>% filter(year >= input$year_range[1], year <= input$year_range[2])
    d
  })
  
  filtered_data_grouped <- reactive({
    d <- data_grouped
    if (input$continent != "All") {
      d <- d %>% filter(continent == input$continent)
    }
    d <- d %>% filter(year >= input$year_range[1], year <= input$year_range[2])
    d
  })
  
  # Line Chart: each continent's average life expectancy over time
  output$line_plot <- renderPlotly({
    linechart <- ggplot(filtered_data_grouped(), aes(x = year, y = avgLifeExp, color = continent)) +
      geom_line(size = 1.2) +
      labs(
        title = "Average Life Expectancy by Continent Over Time (Interactive",
        x = "Year",
        y = "Average Life Expectancy",
        color = "Continent"
      ) +
      theme_dark() +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size=14),
        legend.text = element_text(size=12)
      )
    ggplotly(linechart)
  })
  
  # Scatter Plot: GDP per capita vs Life Expectancy for specific year (Interactive)
  output$scatter_plot <- renderPlotly({
    year_chosen <- max(input$year_range)
    d <- filtered_data() %>% filter(year == year_chosen)
    if (nrow(d) == 0) return(NULL) # If no data, return nothing
    
    scatterplot <- ggplot(d, aes(
      x = gdpPercap, 
      y = lifeExp, 
      size = pop, 
      color = continent,
      # Include the country name in text aesthetic for the tooltip
      text = paste("Country:", country,
                   "<br>GDP per Capita:", round(gdpPercap, 2),
                   "<br>Life Expectancy:", round(lifeExp, 2),
                   "<br>Population:", scales::comma(pop))
    )) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(labels = comma) +
      labs(
        title = paste("GDP per Capita vs. Life Expectancy (", year_chosen, ")", sep=""),
        x = "GDP per Capita (USD)",
        y = "Life Expectancy (Years)",
        size = "Population",
        color = "Continent"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size=13),
        legend.text = element_text(size=11)
      )
    
    # Set tooltip = "text" to use the text aesthetic
    ggplotly(scatterplot, tooltip = "text")
  })
  
  
  # Bar Chart: total population by continent for a specific year.
  output$bar_plot <- renderPlotly({
    year_chosen <- max(input$year_range)
    d <- filtered_data_grouped() %>% filter(year == year_chosen)
    if (nrow(d) == 0) return(NULL)
    
    barchart <- ggplot(d, aes(x = continent, y = totalPop, fill = continent)) +
      geom_col() +
      scale_y_continuous(labels = comma) +
      labs(
        title = paste("Total Population by Continent (", year_chosen, ")", sep=""),
        x = "Continent",
        y = "Total Population",
        fill = "Continent"
      ) +
      theme_dark() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none"
      )
    ggplotly(barchart)
  })
  
  # Box Plot: each continent's life expectancy distribution.
  output$box_plot <- renderPlotly({
    box_plot <- ggplot(filtered_data(), aes(x = continent, y = lifeExp, fill = continent)) +
      geom_boxplot() +
      labs(
        title = "Life Expectancy Distribution by Continent",
        x = "Continent",
        y = "Life Expectancy"
      ) +
      theme_light() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none"
      )
    ggplotly(box_plot)
  })
  
#Faceted Plot: GDP per capita vs life expectancy 
  output$facet_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = gdpPercap, y = lifeExp, color = continent)) +
      geom_point(alpha = 0.7) +
      facet_wrap(~ year, ncol = 3) +
      labs(
        title = "GDP per Capita vs. Life Expectancy by Year",
        x = "GDP per Capita",
        y = "Life Expectancy",
        color = "Continent"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_text(size=13),
        legend.text = element_text(size=11)
      )
  })
  
# Density Plot:life expectancy distribution across continents.
  output$density_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = lifeExp, fill = continent)) +
      geom_density(alpha = 0.5) +
      labs(
        title = "Life Expectancy Distribution by Continent",
        x = "Life Expectancy",
        y = "Density",
        fill = "Continent"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "bottom"
        
      )
  })
}

shinyApp(ui = ui, server = server)
