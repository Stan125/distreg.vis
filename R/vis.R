#' bamlss.vis function
#'
#' @import shiny
#' @export

### --- Shiny App --- ###

vis <- function() {
  ## Make certain UI elements
  # Sidebars
  overviewpanel <-
    tabPanel("Overview",
             br(),
             selectInput("model", label = "Select a model",
                         choices = c("",search_ba())),
             uiOutput("family_ui"),
             uiOutput("equations_ui"))
  scenariopanel <-
    tabPanel("Scenarios",
             uiOutput("scenarios_ui"))

  # Plot
  plotpanel <- tabPanel("Plot",
                        plotOutput("dist_plot"))


  ## Assemble UI
  ui <- fluidPage(
    # Title
    titlePanel("Visualize your bamlss predictions"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(type = "pills",
                    overviewpanel,
                    scenariopanel

        )
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    plotpanel,
                    tabPanel("Properties"))
      )
    )
  )

  server <- function(input, output, session) {

    ## --- Model --- ##
    # Reactive model
    m <- reactive(
      if (input$model != "" & !is.null(input$model))
        get(input$model)
      else
        NULL
    )

    # Reactive model data
    m_data <- reactive({
      if(!is.null(m()))
        m()$model.frame
    })

    ## --- Overview tab --- ##

    # Equations Output
    output$equations_ui <- renderUI({
      if (!is.null(m())) {
        list(strong("Model Equations"),
             verbatimTextOutput("equations"))
      }
    })

    # Equations Rendering
    output$equations <- renderPrint({
      if (!is.null(m()))
        m()$formula
    })

    # Family Output
    output$family_ui <- renderUI({
      if (!is.null(m())) {
        list(strong("Model Family"),
             verbatimTextOutput("family"))
      }
    })

    # Family Rendering
    output$family <- renderPrint({
      if (!is.null(m()))
        cat(f_disp(m()))
    })

    ## --- Scenarios Tab --- ##

    output$scenarios_ui <- renderUI({
      if (!is.null(m())) {

        # Create slider UI elements
        m_indep <- m_data()[, -1]
        cnames <- colnames(m_indep)
        ui_list <- list()
        for (i in 1:ncol(m_indep)) {
          ui_list[[i]] <- sliderInput(inputId = paste0("var", i),
                                      label = cnames[i],
                                      min = round(min(m_indep[, i]), 2),
                                      max = round(max(m_indep[, i]), 2),
                                      value = round(mean(m_indep[, i]), 2),
                                      sep = "")
        }

        # Create indercept UI elements
        ui_list[[ncol(m_indep) + 1]] <-
          checkboxInput("intercept", "Include Intercept?",
                        value = TRUE)

        # Action Button
        ui_list[[ncol(m_indep) + 2]] <-
          actionButton("scen_act", "Create Scenario!")

        # Delete all Scenarios
        ui_list[[ncol(m_indep) + 3]] <-
          actionButton("scen_clear", "Clear Scenarios")

        # Return the list to uis
        ui_list
      }
    })

    ## --- Newdata --- ##

    # This function catches the current selected data
    current_data <- reactive({
      if (!is.null(m())) {
        n_indep <- m_data()[, -1]
        dat <- c()
        for (i in 1:ncol(n_indep))
          dat <- c(dat, input[[paste0("var", i)]])
        dat <- as.data.frame(t(as.matrix(dat)))
        colnames(dat) <- colnames(n_indep)
        dat
      } else {
        NULL
      }
    })

    # This function updates the prediction each time the button is clicked
    pred <- reactiveValues(data = NULL)

    observeEvent(input$scen_act, {
      if (is.null(pred$data))
        pred$data <- current_data()
      else if (!is.null(pred$data))
        pred$data <- rbind(pred$data, current_data())
    })
    observeEvent(input$scen_clear, {
      pred$data <- NULL
    })


    ## --- Plot Tab --- ##
    output$dist_plot <- renderPlot({
      if (!is.null(pred$data))
        plot_dist(m(), pred$data)
      else
        NULL
    })
    # output$testprint <- renderPrint({
    #
    #   if (!is.null(pred$data))
    #     pred$data
    #   else
    #     cat("no scenario selected")
    # })
  }

  shinyApp(ui, server)
}
