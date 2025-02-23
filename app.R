if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, bslib, yaml, R6, data.table, purrr, glue,
  uuid, digest, shinyjs, shinyBS, DT, ggplot2
)

library(shiny)
library(bslib)
# library(jsonlite)
library(yaml)
library(shinyBS)
library(uuid)
library(R6)
library(data.table)

source(file.path("R", "DataProcessorClass.R"))
source(file.path("R", "helper_functions.R"))
source(file.path("R", "constants.R"))
source(file.path("R", "validations.R"))

# Module sources
source(file.path("modules", "plots", "plotYearlyAssetProgressionModule.R"))
source(file.path("modules", "plots", "comparisonPlotsModule.R"))
source(file.path("modules", "sidebar", "sidebarInputModule.R"))
source(file.path("modules", "sidebar", "propertyManagementModule.R"))
source(file.path("modules", "plots", "financialMetricsModule.R"))
source(file.path("modules", "plots", "stackedAreaPlotModule.R"))
source(file.path("modules", "sensitivityAnalysisModule.R"))

ui <- bslib::page_navbar(
  title = "Financial Evolution Calculator",

  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "default"
  ),

  header = tags$head(
    tags$link(rel = "stylesheet",
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "css/styles.css"),
    tags$script(src = "https://code.jquery.com/jquery-3.6.0.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"),
    tags$script(src = "js/main.js")
  ),

  nav_panel(
    title = "Analysis",
    sidebarLayout(
      sidebarPanel(
        class = "sidebar-panel",
        width = 3,
        sidebarInputModuleUI("sidebar_inputs")
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(
            width = 6,
            sliderInput("global_year_range", "Year range:",
                        min = 2025, max = 2075,
                        value = c(2025, 2075),
                        step = 1,
                        sep = ""),
            # Add an invisible overlay to prevent interaction with the left handle
            tags$div(
              style = "position: absolute;
                 left: 0;
                 bottom: 0;
                 width: 50px;  /* Adjust based on your layout */
                 height: 35px; /* Adjust based on your layout */
                 cursor: not-allowed;
                 z-index: 1000;"
            )
          ),
          column(
            width = 6,
            div(
              style = "margin-top: 25px;",
              downloadButton("download_calculations", "Save Financial Evolution",
                             class = "btn-sm"),
              actionButton("add_to_comparison", "Add to Comparison",
                           class = "btn-sm")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            card(
              style = "height: 400px;",
              card_header("Asset Evolution"),
              plotYearlyAssetProgressionModuleUI("result_plot")
            )
          ),
          column(
            width = 6,
            card(
              style = "height: 400px;",
              card_header("Financial Metrics"),
              financialMetricsModuleUI("financial_metrics")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            card(
              style = "height: 400px;",
              card_header("Expenses Components"),
              stackedAreaPlotModuleUI("expense_components", plot_type = "expenses")
            )
          ),
          column(
            width = 6,
            card(
              style = "height: 400px;",
              card_header("Income Components"),
              stackedAreaPlotModuleUI("income_components", plot_type = "income")
            )
          )
        ),
        # Add new section for sensitivity analysis
        fluidRow(
          column(
            width = 12,
            card(
              card_header("Sensitivity Analysis"),
              sensitivityAnalysisModuleUI("sensitivity_analysis")
            )
          )
        )
      )
    )
  ),

  # Remove the separate sensitivity analysis tab
  nav_panel(
    title = "Comparison",
    comparisonPlotsModuleUI("comparison_plots")
  )
)

server <- function(input, output, session) {
  # Let the module handle the configuration
  reactive_config <- sidebarInputModuleServer("sidebar_inputs")

  # Create a reactive expression for the year range
  year_range <- reactive({
    if (input$global_year_range[1] != 2025) {
      updateSliderInput(session, "global_year_range",
                        value = c(2025, input$global_year_range[2]))
    }
    input$global_year_range
  })

  # Reactive expression for processed data
  processed_data <- eventReactive(
    c(reactive_config(), input$global_year_range), {
    req(reactive_config(), year_range())
    data_processor <- DataProcessor$new(
      scenario_name = "example",
      config = reactive_config(),
      initial_year = min(year_range()),
      final_year = max(year_range())
    )
    data_processor$calculate()
    results <- data_processor$get_results()
    return(results)
  },
  ignoreNULL = FALSE
  )

  # Plot module
  plotYearlyAssetProgressionModuleServer("result_plot", processed_data, reactive_config, year_range)
  financialMetricsModuleServer("financial_metrics", processed_data, year_range)
  stackedAreaPlotModuleServer("expense_components", processed_data, year_range, plot_type = "expenses")
  stackedAreaPlotModuleServer("income_components", processed_data, year_range, plot_type = "income")
  sensitivityAnalysisModuleServer("sensitivity_analysis", reactive_config, processed_data, year_range)

  # Download handler for financial calculations
  output$download_calculations <- downloadHandler(
    filename = function() {
      paste0("financial_evolution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      # Get current processed data
      data <- isolate(processed_data())

      # Write to CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )

  # Observer for add to comparison button (placeholder for now)
  observeEvent(input$add_to_comparison, {
    # This will be implemented later
    showNotification("Add to comparison functionality will be implemented soon",
                     type = "message")
  })

  # Call the comparison plots module
  comparisonPlotsModule("comparison_plots", comparison_list)
}

shinyApp(ui, server)
