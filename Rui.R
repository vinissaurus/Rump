# Load required libraries
library(shiny)
library(ggplot2)

# Load iris dataset
data(iris)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Histogram of Iris Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "var", label = "Select a variable", choices = names(iris), selected = "Sepal.Length"),
      sliderInput(inputId = "bins", label = "Select number of bins", min = 5, max = 50, value = 30)
    ),
    mainPanel(
      plotOutput(outputId = "histogram")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Generate histogram based on user input
  output$histogram <- renderPlot({
    ggplot(iris, aes_string(x = input$var)) +
      geom_histogram(bins = input$bins, color = "white", fill = "skyblue") +
      labs(title = paste("Histogram of", input$var),
           x = input$var, y = "Frequency")
  })
  
}

# Run the app
shinyApp(ui, server)
