#' bamlss.vis function
#'
#' @import shiny
#' @import rhandsontable
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
    tabPanel("Scenarios", value = 5,
             uiOutput("scenarios_ui"))

  scenariodatapanel <-
    tabPanel("Scenario Data", value = 6,
             strong("Edit scenario data here"),
             br(), br(),
             uiOutput("scenariodata_ui"))

  # Plot
  plotpanel <- tabPanel("Plot",
                        #verbatimTextOutput("testprint"))
                        fluidRow(
                          column(width = 9,
                                 plotOutput("dist_plot")),
                          column(width = 3, br(),
                                 uiOutput("plotbar"))
                        )
  )

  # Properties
  proppanel <- tabPanel("Properties", uiOutput("exvxdf_ui"))


  ## Assemble UI
  ui <- fluidPage(
    # Title
    titlePanel("Visualize your bamlss predictions"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(type = "pills", id = "pillpanel",
                    overviewpanel,
                    scenariopanel,
                    scenariodatapanel

        )
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    plotpanel,
                    proppanel)
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

        # Some Space
        ui_list[[1]] <- br()

        # Action Button
        ui_list[[2]] <- actionButton("scen_act", "Create Scenario!")

        # Delete all Scenarios
        ui_list[[3]] <- actionButton("scen_clear", "Clear Scenarios")

        # More space
        ui_list[[4]] <- br()

        # Create intercept UI elements
        ui_list[[5]] <- checkboxInput("intercept", "Include Intercept?",
                                      value = TRUE)

        # Create coefficient elements
        for (i in 1:ncol(m_indep)) {
          if (any(is.numeric(m_indep[, i]))) {
            ui_list[[i + 5]] <- sliderInput(inputId = paste0("var", i),
                                            label = cnames[i],
                                            min = round(min(m_indep[, i]), 2),
                                            max = round(max(m_indep[, i]), 2),
                                            value = round(mean(m_indep[, i]), 2),
                                            sep = "")
          } else if (any(is.factor(m_indep[, i]))) {
            ui_list[[i + 5]] <- selectInput(inputId = paste0("var", i),
                                            label = cnames[i],
                                            choices = levels(m_indep[, i]),
                                            selected = levels(m_indep[, i])[1])
          }
        }

        # Return the list to uis
        ui_list
      }
    })

    ## --- Newdata --- ##

    # This function catches the current selected data
    current_data <- reactive({
      if (!is.null(m())) {
        indep <- m_data()[, -1]

        # Create empty dataframe
        dat <- indep[NULL, ]

        # Get current variable values
        for (i in 1:ncol(indep))
          dat[1, i] <- input[[paste0("var", i)]]

        # Convert categorical variables to factors with right levels
        dat <- fac_equ(indep, dat)

        # Add intercept
        dat <- cbind(dat, intercept = input$intercept)

        # Show DF
        dat
      } else {
        NULL
      }
    })

    # This function updates the prediction data each time the button is clicked
    pred <- reactiveValues(data = NULL)

    observeEvent(input$scen_act, {
      if (is.null(pred$data)) {
        pred$data <- current_data()
      }
      else if (!is.null(pred$data)) {
        pred$data <- rbind(pred$data, current_data())
        row.names(pred$data) <- paste0("P", 1:nrow(pred$data))
      }
    })

    observeEvent(input$scen_clear, {
      pred$data <- NULL
    })

    # This function clears the current pred$data when a new model is selected
    observeEvent(m(), {
      pred$data <- NULL
    })

    ## --- Scenario data Tab --- ##

    # This function displays the UI of the handsontable
    output$scenariodata_ui <- renderUI({
      rHandsontableOutput(outputId = "predtable")
    })

    # This function renders the handsontable
    output$predtable <- renderRHandsontable({
      if (!is.null(pred$data)) {
        DF <- pred$data
        rhandsontable(DF, width = 400)
      } else {
        NULL
      }
    })

    # This function updates the prediction data when hot changes
    observe({
      if (!is.null(input$predtable)) {
        DF <- hot_to_r(input$predtable)
        row.names(DF) <- paste0("P", 1:nrow(DF))
        pred$data <- DF
      }
    })

    ## --- Current predictions --- ##

    # This function always catches the current predictions
    cur_pred <- reactive({
      if (!is.null(pred$data))
        preds(m(), pred$data)
    })

    ## --- Plot Tab --- ##

    ## Plot is rendered here
    output$dist_plot <- renderPlot({
      if (!is.null(pred$data))
        plot_dist(m(), cur_pred(), palette = input$pal_choices,
                  type = input$type_choices)
      else
        NULL
    })

    ## Color Choices / pdf/cdf choice are rendered here
    output$plotbar <- renderUI({
      if (!is.null(m()) & any(input$pillpanel == 5, input$pillpanel == 6)) {
        ui_list <- list()
        # Palette Choices
        ui_list[[1]] <-
          selectInput("pal_choices", label = "Colour Palette",
                      choices = c("default", "Accent", "Dark2", "Paired",
                                  "Pastel1", "Pastel2", "Set1", "Set2",
                                  "Set3"))

        # CDF/PDF Choice
        ui_list[[2]] <-
          selectInput("type_choices", label = "PDF or CDF?",
                      choices = c("pdf", "cdf"))

        ui_list
      }
    })

    # output$testprint <- renderPrint({
    #   if (!is.null(pred$data))
    #     pred$data
    #   else
    #     cat("no scenario selected")
    # })

    ## --- Properties Tab --- ##

    # UI for expectation/variance dataframe
    output$exvxdf_ui <- renderUI({
      if (!is.null(m()))
        tableOutput("exvxdf")
    })

    # Server-Rendering of DF
    output$exvxdf <- renderTable({
      if (!is.null(m())) {
        fam <- family(m())$family
        moments <- apply(cur_pred(), MARGIN = 1,
                         FUN = moments, family = fam)
        moments <- do.call(rbind, moments)
        moments
      }
    }, rownames = TRUE)

  }
  shinyApp(ui, server)
}
