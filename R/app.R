##############################
######### bamlss.vis #########
##############################

### --- Preliminaries --- ###

## Libraries
library(shiny)

### --- Shiny App --- ###

ui <- fluidPage(
  # Title
  titlePanel("Visualize your bamlss predictions"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Overview"),
                  tabPanel("Scenarios")

      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot"),
                  tabPanel("Properties"))
    )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
