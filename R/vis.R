#' bamlss.vis function
#'
#' Function to call the bamlss.vis Shiny App which represents the core of this
#'   package.
#' @import shiny
#' @import rhandsontable
#' @import rstudioapi
#' @importFrom plotly renderPlotly plotlyOutput ggplotly plotly_empty
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
                                 uiOutput("condition_plot")),
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
      if (!is.null(m()))
        m()$model.frame
    })

    # Reactive model family
    fam <- reactive({
      if (!is.null(m()))
        family(m())
    })

    # Got Model and data?
    gmad <- reactive({
      if (!is.null(m()) & !is.null(pred$data))
        TRUE
      else
        FALSE
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
    # Since 0.4.5 it also checks whether cov combinations are in range
    observe({
      if (!is.null(input$predtable)) {
        # Convert handsontable to df and give it the original rownames
        DF <- hot_to_r(input$predtable)
        row.names(DF) <- paste0("P", 1:nrow(DF))

        # Check whether newdata is in old data's range
        combs <- range_checker(m()$model.frame, DF)
        if (!is.null(combs)) { # if not NULL then we have bad combs
          warn_message <- bad_range_warning(combs)
          showNotification(warn_message, type = "warning", duration = 10)
        }

        # Assign the new DF to pred$data
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

    ## PLotly is rendered here, condition is checked with conditionalPanel
    output$plotly <- renderPlotly({
      if (gmad()) {
        if (is.2d(fam()$family, fam()$links)) {
          p <- plot_dist(m(), cur_pred(), palette = input$pal_choices,
                         type = input$type_choices)
          p$elementId <- NULL
          p
        } else {
          # This and ...
          p <- plotly_empty(type = "scatter", mode = "markers")
          p$elementId <- NULL
          p
        }
      } else {
        # ...this are only to prevent annoying error messages from plotly
        p <- plotly_empty(type = "scatter", mode = "markers")
        p$elementId <- NULL
        p
      }
    })

    ## Plot is rendered here, condition is checked with conditionalPanel
    output$plot <- renderPlot({
      if (gmad())
        if (!is.2d(fam()$family, fam()$links))
          plot_dist(m(), cur_pred(), palette = input$pal_choices,
                    type = input$type_choices)
      else
        NULL
    })

    ## The Plot Ui element itself is rendered here
    ## It checks the conditions for plot and then decides if plotly or plot
    output$condition_plot <- renderUI({
      if (gmad()) {
        if (is.2d(fam()$family, fam()$links)) {
          plotlyOutput("plotly")
        } else {
          plotOutput("plot")
        }
      } else {
        NULL
      }
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

        # Action Button for console pasting
        ui_list[[3]] <-
          actionButton("pastecode", icon = icon("code"),
                       label = "Obtain Code!", style = "color:white;
                       background-color:red")

        ui_list
      }
    })

    ## What happens when pastecode button is pressed
    observeEvent(input$pastecode, {
          # First line of code
          c_data <- capture.output(dput(pred$data))
          c_data <- c("covariate_data <- ", c_data)
          c_data <- paste0(c_data, collapse = "")

          # Second line of code
          c_predictions <- call("preds", model = as.name(input$model),
                                newdata = quote(covariate_data))
          c_predictions <- paste0("pred_data <- ", deparse(c_predictions))

          # Third line of code
          c_plot <- call("plot_dist", model = as.name(input$model),
                         predictions = quote(pred_data),
                         type = input$type_choices)
          if (!is.null(input$palette))
            c_plot[["palette"]] <- input$palette
          c_plot <- deparse(c_plot) # Make call into character

          showModal(modalDialog(
            title = "Obtain your R code",
            tags$code(c_data), br(),
            tags$code(c_predictions), br(),
            tags$code(c_plot),
            easyClose = TRUE
          ))
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
