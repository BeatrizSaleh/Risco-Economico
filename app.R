# app.R

library(shiny)
library(tidyverse)
library(WDI)  
library(scales)
library(ggplot2)
library(plotly)


# Dados
set.seed(123)
dates <- seq(as.Date("2010-01-01"), as.Date("2023-12-01"), by = "quarter")
regions <- c("G7", "EM", "BRICS")
indicators <- c("GDP", "Inflation", "Unemployment", "InterestRate")

# Dataset simulado (por região, indicador e tempo)
sim_data <- expand.grid(date = dates, region = regions, indicator = indicators) %>%
  mutate(value = rnorm(n(), mean = 0, sd = 1)) %>%
  pivot_wider(names_from = indicator, values_from = value)

# PCA por região
compute_pca <- function(df) {
  mat <- df %>% select(GDP, Inflation, Unemployment, InterestRate) %>% scale()
  pca <- prcomp(mat)
  df$Barometer <- pca$x[,1]
  return(df)
}

# Aplicar PCA por região
barometer_data <- sim_data %>%
  group_by(region) %>%
  group_modify(~ compute_pca(.x)) %>%
  ungroup() %>%
  mutate(date = as.Date(date, origin = "1970-01-01"))  # <- conversão global


# UI 
ui <- fluidPage(
  titlePanel("Monitor Global de Condições Econômicas"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Selecione a região:", choices = unique(barometer_data$region))
    ),
    mainPanel(
      plotlyOutput("barometerPlot"),
      br(),
      tableOutput("latestValues")
    )
  )
)

# Servidor
server <- function(input, output) {
  filtered_data <- reactive({
    barometer_data %>%
      filter(region == input$region) %>%
      mutate(date = as.Date(date, origin = "1970-01-01"))  # <- Aqui está o fix
  })
  
  output$barometerPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = date, y = Barometer)) +
      geom_line(size = 1) +
      labs(title = paste("Índice de Condições Econômicas -", input$region),
           x = "Data", y = "Índice (1º Componente PCA)") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$latestValues <- renderTable({
    tail(filtered_data() %>% select(date, Barometer), 5)
  })
}


# Rodar
shinyApp(ui = ui, server = server)
