library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plotly"),
  plotOutput("plot")
)

server <- function(input, output) {

  # I need to be able to put NULL in here or anything so that
  # there is no output and also no error message.
  output$plotly <- renderPlotly({
    NULL
  })

  # This works and sends no error message
  output$plot <- renderPlot({
    NULL
  })

}

shinyApp(ui, server)
