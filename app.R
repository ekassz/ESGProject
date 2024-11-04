library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(RColorBrewer)
library(DT)

data <- read.csv("data/data.csv")

data$esg_category <- dplyr::case_when(
  data$environment_grade %in% c("AAA", "AA", "A") ~ "High",
  data$environment_grade %in% c("BBB", "BB", "B") ~ "Medium",
  data$environment_grade %in% c("CCC", "CC", "C") ~ "Low",
  TRUE ~ "Unknown"
  
)

# Define the UI
ui <- fluidPage(
  titlePanel(tags$strong("ESG Performance Insights")),
  sidebarLayout(
    sidebarPanel(
      helpText("This tool helps investors and stakeholders explore how companies perform in three key areas:" ,tags$strong("Environmental, Social, and Governance (ESG)"),". 
              With this you can help assess companies' performance, giving insights into sustainability, ethical practices, and risk factors.
               You can filter companies by performance level:", tags$strong("High, Medium, or Low"), " and view the score distribution for any ESG category in a simple histogram."),
      helpText("Below the chart, a dashboard displays specific information about companies that match your filters, letting you explore individual results while seeing overall trends."),
      helpText(tags$strong("Filter by ESG Levels")),
      helpText("Use the filters provided below to refine the data displayed in the histogram."),
      selectInput("env_grade", "Select Environmental Grade:", 
                  choices = c("All", "High", "Medium", "Low")),
      selectInput("soc_grade", "Select Social Grade:", 
                  choices = c("All", "High", "Medium", "Low")),
      selectInput("gov_grade", "Select Governance Grade:",
                  choices = c("All", "High", "Medium", "Low")),
      selectInput("score_type", "Select Score Type:",
                  choices = c("Environmental Score" = "environment_score",
                              "Social Score" = "social_score",
                              "Governance Score" = "governance_score")),
      helpText("The red dashed line on the chart shows the average score, giving a quick reference for typical performance. 
               Scores clustered around this line indicate common results, while more spread-out scores suggest a wider range of performance. 
               Use the adjustable bin width to see finer or broader score details."),
      sliderInput("bin_width", "Select Bin Width:", min = 5, max = 50, value = 10)
    ),
    
    mainPanel(
      plotlyOutput("esgBar"),
      br(),
      h4(tags$strong("ESG Company Score Table")),
      p("Use this table to explore how companies perform on Environmental, Social, and Governance (ESG) criteria.
        Companies with strong ESG scores often manage risks better, align with ethical practices, and demonstrate a commitment to sustainable growth, making them potentially safer and more responsible investments. 
        Use the filters to explore score distributions and uncover how different companies rank in each ESG category."),
      p(tags$strong("Understanding the Grades:")),
      p(tags$strong("High (AAA, AA, A):"),"Indicates strong performance and low risk"),
      p(tags$strong("Medium (BBB, BB, B):"),"Reflects moderate performance with some risk"),
      p(tags$strong("Low (CCC, C):"),"Shows weaker performance and higher risk"),
      p("By understanding these grades, you can identify companies that align with your values or investment goals, whether you're prioritizing sustainability, social responsibility, or ethical governance practices."),
      DT::DTOutput("filteredData")
    )
  )
)

# Define the Server
server <- function(input, output) {
  palette_colors <- brewer.pal(n = 6, "Blues")
  
  filtered_data <- reactive({
    data %>%
      filter(
        (esg_category == input$env_grade | input$env_grade == "All") & 
        (esg_category == input$soc_grade | input$soc_grade == "All") & 
        (esg_category == input$gov_grade | input$gov_grade == "All")
      )%>%
      select(c(name, environment_grade, social_grade, governance_grade, total_grade, environment_score, 
               social_score, governance_score, total_score))
  })
  
  output$esgBar <- renderPlotly({
    #check if there's any rows in the filtered data
    if(nrow(filtered_data()) == 0){
      plot_ly()%>%
        layout(
          title = "No data available for the selected filters",
          xaxis = list(showticklabels = FALSE),
          yaxis = list(showticklabels = FALSE)
        )
    }else {
    
      plot_data <- filtered_data() %>%
        mutate(tooltip_text = paste("Company:", name, "<br>Score:", get(input$score_type)))
      
      
      p <- ggplot(plot_data,
                  aes_string(x = input$score_type, text = "tooltip_text")) +
        geom_histogram(
          binwidth = input$bin_width,
          fill = palette_colors[4],
          color = palette_colors[6],
          alpha = 0.7
        ) +
        geom_vline(
          aes(xintercept = mean(get(input$score_type), na.rm = TRUE)),
          color = "red",
          linetype = "dashed",
          linewidth = 0.7
        ) +
        labs(
          title = paste("Distribution of", gsub(
            "_", " ", tools::toTitleCase(input$score_type)
          )),
          x = gsub("_", " ", tools::toTitleCase(input$score_type)),
          y = "Frequency"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      
      ggplotly(p, tooltip = "text") %>%
        layout(hovermode = "closest") %>%
        config(displayModeBar = FALSE)
    }
    
  })
  
  output$filteredData <- DT::renderDT({
    filtered_data()%>%
      rename(Company = name,  `Environmental Grade` = environment_grade,
             `Social Grade` = social_grade, `Governance Grade` = governance_grade,
             `Total Grade` = total_grade,`Environmental Score` = environment_score,
             `Social Score` = social_score, `Governance Score` = governance_score,
             `Total Score` = total_score)
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)

