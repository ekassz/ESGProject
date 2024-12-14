library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)
library(RColorBrewer)
library(viridisLite)
library(tidyr)


# Load datasets
data1 <- read.csv("https://raw.githubusercontent.com/cjstone2/Stat436/refs/heads/main/ESGdata.csv")
data2 <- read.csv("https://raw.githubusercontent.com/jasonjclark/stat436deadline2/refs/heads/main/publiccompanies.csv") %>%
  mutate(industry = case_when(
    industry == "Aerospace & Defense" ~ "Aerospace and Defense",
    industry == "Metals & Mining" ~ "Metals and Mining",
    TRUE ~ industry
  ))
data3 <- read_csv("https://raw.githubusercontent.com/ManaswiKolani/Stat436ProjectMilestone2/refs/heads/main/data.csv")
data4 <- read.csv("https://raw.githubusercontent.com/ekassz/ESGProject/refs/heads/main/data/data.csv")

data4 <- data4 %>%
  mutate(
    env_category = case_when(
      environment_grade %in% c("AAA", "AA", "A") ~ "High",
      environment_grade %in% c("BBB", "BB", "B") ~ "Medium",
      environment_grade %in% c("CCC", "CC", "C") ~ "Low",
      TRUE ~ "Unknown"
    ),
    soc_category = case_when(
      social_grade %in% c("AAA", "AA", "A") ~ "High",
      social_grade %in% c("BBB", "BB", "B") ~ "Medium",
      social_grade %in% c("CCC", "CC", "C") ~ "Low",
      TRUE ~ "Unknown"
    ),
    gov_category = case_when(
      governance_grade %in% c("AAA", "AA", "A") ~ "High",
      governance_grade %in% c("BBB", "BB", "B") ~ "Medium",
      governance_grade %in% c("CCC", "CC", "C") ~ "Low",
      TRUE ~ "Unknown"
    )
  )


# UI
ui <- fluidPage(
  titlePanel("ESG Insights"),
  tabsetPanel(
    # Tab 1: Radar Chart Comparison
    tabPanel("Radar Chart Comparison",
             sidebarLayout(
               sidebarPanel(
                 helpText("Compare companies' ESG performance using a radar chart."),
                 selectInput("company1", "Select First Company:", choices = unique(data1$name)),
                 selectInput("company2", "Select Second Company:", choices = unique(data1$name)),
                 helpText("Each axis represents a different ESG dimension: Environmental, Social, and Governance.")
               ),
               mainPanel(plotlyOutput("esgRadar"))
             )),
    
    # Tab 2: Boxplots by Industry
    tabPanel("Boxplots by Industry",
             sidebarLayout(
               sidebarPanel(
                 selectInput("esg_category", "Select ESG Category:",
                             choices = c("Environmental" = "environment_score",
                                         "Social" = "social_score",
                                         "Governance" = "governance_score",
                                         "Total ESG" = "total_score")),
                 selectizeInput("industries", "Select Industries:",
                                choices = sort(unique(data2$industry)),
                                selected = sort(unique(data2$industry))[1:5], multiple = TRUE),
                 helpText("Hover over points to see company details.")
               ),
               mainPanel(plotlyOutput("boxplot", height = "700px"))
             )),
    
    # Tab 3: ESG Heatmap and Summary
    tabPanel("ESG Heatmap and Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("industry_input", "Select Industry:", choices = unique(data3$industry)),
                 helpText("Explore ESG scores and grades within a selected industry."),
                 verbatimTextOutput("statisticsText")
               ),
               mainPanel(
                 plotOutput("industrySummaryPlot"),
                 plotOutput("esgHeatmapPlot"),
                 tableOutput("comparisonTable")
               )
             )),
    
    # Tab 4: ESG Grade Distribution
    tabPanel("ESG Grade Distribution",
             sidebarLayout(
               sidebarPanel(
                 helpText("This tool helps investors and stakeholders explore how companies perform in three key areas:" ,tags$strong("Environmental, Social, and Governance (ESG)"),". 
              With this you can help assess companies' performance, giving insights into sustainability, ethical practices, and risk factors.
               You can filter companies by performance level:", tags$strong("High, Medium, or Low"), " and view the score distribution for any ESG category in a simple histogram."),
                 helpText(tags$strong("Filter by ESG Levels")),
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
                 helpText("The red dashed line on the chart shows the average score, giving a quick reference for typical performance."),
                 sliderInput("bin_width", "Select Bin Width:", min = 5, max = 50, value = 10),
               ),
               
               mainPanel(
                 plotlyOutput("esgBar"),
                 br(),
                 h4(tags$strong("ESG Company Score Table")),
                 p("Use this table to explore how companies perform on Environmental, Social, and Governance (ESG) criteria.
        Companies with strong ESG scores often manage risks better, align with ethical practices, and demonstrate a commitment to sustainable growth, making them potentially safer and more responsible investments."),
                 p(tags$strong("Understanding the Grades:")),
                 p(tags$strong("High (AAA, AA, A):"),"Indicates strong performance and low risk"),
                 p(tags$strong("Medium (BBB, BB, B):"),"Reflects moderate performance with some risk"),
                 p(tags$strong("Low (CCC, C):"),"Shows weaker performance and higher risk"),
                 DT::DTOutput("filteredData")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Tab 1: Radar Chart Comparison
  output$esgRadar <- renderPlotly({
    company1_scores <- data1 %>%
      filter(name == input$company1) %>%
      select(environment_score, social_score, governance_score) %>%
      unlist() %>%
      as.numeric()
    
    company2_scores <- data1 %>%
      filter(name == input$company2) %>%
      select(environment_score, social_score, governance_score) %>%
      unlist() %>%
      as.numeric()
    
    metrics <- c("Environmental Score", "Social Score", "Governance Score")
    
    plot_ly(
      type = 'scatterpolar',
      mode = 'lines'
    ) %>%
      add_trace(
        r = company1_scores,
        theta = metrics,
        fill = 'toself',
        name = input$company1,
        text = paste0(metrics, ": ", company1_scores),
        hoverinfo = 'text'
      ) %>%
      add_trace(
        r = company2_scores,
        theta = metrics,
        fill = 'toself',
        name = input$company2,
        text = paste0(metrics, ": ", company2_scores),
        hoverinfo = 'text'
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(data1[c("environment_score", "social_score", "governance_score")]))
          )
        ),
        title = "ESG Performance Comparison"
      )
  })
  
  # Tab 2: Boxplots by Industry
  output$boxplot <- renderPlotly({
    filtered_data <- data2 %>%
      filter(industry %in% input$industries)
    
    esg_label <- switch(input$esg_category,
                        "environment_score" = "Environmental Score",
                        "social_score" = "Social Score",
                        "governance_score" = "Governance Score",
                        "total_score" = "Total ESG Score")
    
    ggplotly(
      ggplot(filtered_data, aes(x = industry, y = .data[[input$esg_category]], fill = industry)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(aes(text = paste("Company:", name)), width = 0.2, size = 1) +
        labs(title = esg_label, x = "Industry", y = esg_label) +
        theme_minimal()
    )
  })
  
  # Tab 3: ESG Heatmap and Summary
  output$statisticsText <- renderPrint({
    filtered_data <- data3 %>%
      filter(industry == input$industry_input)
    
    highest <- filtered_data[which.max(filtered_data$total_score), ]
    lowest <- filtered_data[which.min(filtered_data$total_score), ]
    
    cat("Highest ESG Score:\n")
    cat("Company:", highest$name, "| Total Score:", highest$total_score, "\n\n")
    
    cat("Lowest ESG Score:\n")
    cat("Company:", lowest$name, "| Total Score:", lowest$total_score, "\n")
  })
  
  output$industrySummaryPlot <- renderPlot({
    filtered_data <- data3 %>%
      filter(industry == input$industry_input)
    
    ggplot(filtered_data, aes(x = reorder(name, -total_score), y = total_score, fill = total_grade)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("A" = "green", "BBB" = "yellow", "BB" = "orange", "B" = "red")) +
      labs(title = paste("ESG Scores in", input$industry_input), x = "Company", y = "Total Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$esgHeatmapPlot <- renderPlot({
    filtered_data <- data3 %>%
      filter(industry == input$industry_input) %>%
      pivot_longer(cols = c(environment_score, social_score, governance_score), 
                   names_to = "criteria", values_to = "score")
    
    ggplot(filtered_data, aes(x = name, y = criteria, fill = score)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = paste("ESG Heatmap for", input$industry_input), x = "Company", y = "Criteria") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$comparisonTable <- renderTable({
    data3 %>%
      filter(industry == input$industry_input) %>%
      select(name, total_score, total_grade)
  })
  
  # Tab 4: ESG Grade Distribution
  palette_colors <- brewer.pal(n = 6, "Blues")
  
  filtered_data <- reactive({
    data4 %>%
      filter(
        (env_category == input$env_grade | input$env_grade == "All") & 
          (soc_category == input$soc_grade | input$soc_grade == "All") & 
          (gov_category == input$gov_grade | input$gov_grade == "All")
      )%>%
      select(c(name, environment_grade, social_grade, governance_grade, total_grade, environment_score, 
               social_score, governance_score, total_score))
  })
  
  output$esgBar <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      plot_ly() %>%
        layout(
          title = "No data available for the selected filters",
          xaxis = list(showticklabels = FALSE),
          yaxis = list(showticklabels = FALSE)
        )
    } else {
      search_query <- input$filteredData_search
      is_search_empty <- is.null(search_query) || search_query == ""
      
      plot_data <- filtered_data() %>%
        mutate(
          search = case_when(
            !is_search_empty & grepl(search_query, name, ignore.case = TRUE) ~ "Searched Companies",
            TRUE ~ "All Companies"
          ),
          performance_level = case_when(
            get(input$score_type) >= 500 ~ "High (AAA, AA, A)",
            get(input$score_type) >= 200 & get(input$score_type) < 500 ~ "Medium (BBB, BB, B)",
            get(input$score_type) < 200 ~ "Low (CCC, C)"
          ),
          tooltip_text = paste("Company:", name, "<br>Score:", get(input$score_type))
        )
      
      ggplotly(
        ggplot(plot_data, aes_string(x = input$score_type, fill = "search", text = "tooltip_text")) +
          geom_histogram(
            data = plot_data %>% filter(search == "All Companies"),
            binwidth = input$bin_width,
            color = "blue",
            alpha = 0.7
          ) +
          geom_histogram(
            data = plot_data %>% filter(search == "Searched Companies"),
            binwidth = input$bin_width,
            color = "red",
            alpha = 1.0
          ) +
          scale_fill_manual(
            values = c(
              "All Companies" = palette_colors[4],
              "Searched Companies" = "orange"
            ),
            labels = c(
              "All Companies" = "All Companies",
              "Searched companies" = ifelse(!is_search_empty, search_query, "Searched Companies")
            )
          ) +
          geom_vline(
            aes(xintercept = mean(get(input$score_type), na.rm = TRUE)),
            color = "red",
            linetype = "dashed",
            linewidth = 0.7
          ) +
          labs(
            title = paste("Distribution of", gsub("_", " ", tools::toTitleCase(input$score_type))),
            x = gsub("_", " ", tools::toTitleCase(input$score_type)),
            y = "Frequency"
          ) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)) +
          annotate("text", x = 200, y = 72, label = "B Grade Min.", color = "red", size = 4, hjust = -0.1) +
          annotate("text", x = 500, y = 110, label = "A Grade Min.", color = "red", size = 4, hjust = -0.1)
      ) %>%
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

# Run App
shinyApp(ui, server)