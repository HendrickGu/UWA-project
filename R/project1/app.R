#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)


ui <- fluidPage(
  titlePanel("Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Select Plot Type:", 
                  choices = c("Bar Chart", "Dot Chart", "Pie Chart"))
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.plot_type != 'Pie Chart'",
        selectInput("y_variable", "Select y-axis variable:", 
                    choices = c("video_views", "video_views_for_last_30_days",
                                "highest_monthly_earnings", "highest_yearly_earnings"))
      ),
      plotOutput("plot", height = "400px", width = "600px")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    youtube_cleaned %>%
      drop_na()  # It is meaningless to participate in the rank if views or earnings equals 0
  })
  
  output$plot <- renderPlot({
    y_var <- input$y_variable
    
    # Calculate the occurrence count for each category
    category_counts <- filtered_data() %>%
      count(category)
    
    # Keep only categories with occurrences greater than or equal to 5(If the frequency of occurrence is too low, it is not representative)
    valid_categories <- category_counts %>%
      filter(n >= 5) %>%
      pull(category)
    
    # # Filter the data based on the selected category filter(for bar and dot chart)
    filtered_data <- filtered_data() %>%
      filter(category %in% valid_categories)
    
    if (input$plot_type == "Bar Chart" || input$plot_type == "Dot Chart") {
      avg_values <- filtered_data %>%
        group_by(category) %>%
        summarize(avg_value = mean(.data[[y_var]], na.rm = TRUE))
      
      if (input$plot_type == "Bar Chart") {
        ggplot(avg_values, aes(x = category, y = avg_value, fill = category)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = "Category", y = y_var) +
          ggtitle("Bar Chart (Average Values)") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
                axis.text.y = element_text(size = 14))
      } else if (input$plot_type == "Dot Chart") {
        ggplot(filtered_data, aes(x = category, y = .data[[y_var]], color = category)) +
          geom_point() +
          labs(x = "Category", y = y_var) +
          ggtitle("Dot Chart") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
                axis.text.y = element_text(size = 14))
      }
    } else if (input$plot_type == "Pie Chart") {
      data <- filtered_data() %>%
        group_by(category) %>%
        summarize(total = sum(.data[[y_var]], na.rm = TRUE))
      
      plot_ly(data, labels = ~category, values = ~total, type = 'pie') %>%
        layout(title = "Pie Chart")
    }
  })
}

shinyApp(ui, server)



