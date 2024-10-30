library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(RColorBrewer)

data <- read.csv("data/data.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("ESG Score Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("env_grade", "Select Environmental Grade:", 
                  choices = c("All", unique(data$environment_grade))),
      selectInput("soc_grade", "Select Social Grade:", 
                  choices = c("All", unique(data$social_grade))),
      selectInput("gov_grade", "Select Governance Grade:",
                  choices = c("All", unique(data$governance_grade))),
      selectInput("score_type", "Select Score Type:",
                  choices = c("Environmental Score" = "environment_score",
                              "Social Score" = "social_score",
                              "Governance Score" = "governance_score")),
      sliderInput("bin_width", "Select Bin Width:", min = 5, max = 50, value = 10)
      
    ),
    
    mainPanel(
      plotOutput("esgBar"),
      dataTableOutput("filteredData")
    )
  )
)

# Define the Server
server <- function(input, output) {
  palette_colors <- brewer.pal(n = 6, "Blues")
  
  filtered_data <- reactive({
    data %>%
      filter(
        (environment_grade == input$env_grade | input$env_grade == "All") &
          (social_grade == input$soc_grade | input$soc_grade == "All") &
          (governance_grade == input$gov_grade | input$gov_grade == "All")
      )%>%
      select(c(environment_grade, social_grade, governance_grade, total_grade, environment_score, 
               social_score, governance_score, total_score))
  })
  
  output$esgBar <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$score_type)) +
      geom_histogram(
        binwidth = input$bin_width,
        fill = palette_colors[4], 
        color = palette_colors[6], 
        alpha = 0.7
      ) +
      labs(
        title = paste("Distribution of", gsub("_", " ", tools::toTitleCase(input$score_type))),
        x = gsub("_", " ", tools::toTitleCase(input$score_type)),
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$filteredData <- renderDataTable({
    filtered_data()
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)
