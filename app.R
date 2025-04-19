if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, bslib, yaml, R6, data.table, purrr, glue,
  uuid, digest, shinyjs, shinyBS, DT, ggplot2, shinyTree
)

require(shiny)
require(bslib)
require(shinyTree)
require(yaml)
require(shinyBS)
require(uuid)
require(R6)
require(data.table)

source(file.path("R", "DataProcessorClass.R"))
source(file.path("R", "helper_functions.R"))
source(file.path("R", "constants.R"))
source(file.path("R", "validations.R"))

# Module sources
source(file.path("modules", "homeNavtab", "homeNavtabModule.R"))
source(file.path("modules", "calculatorNavTab", "sidebarInputModule.R"))
source(file.path("modules", "calculatorNavTab", "propertyManagementModule.R"))
source(file.path("modules", "plots", "AssetEvolutionModule.R"))
source(file.path("modules", "plots", "financialMetricsModule.R"))
source(file.path("modules", "plots", "stackedAreaPlotModule.R"))
source(file.path("modules", "plots", "sensitivityAnalysisModule.R"))

ui <- bslib::page_navbar(
  title = "Financial Evolution Calculator",

  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "default"
  ),
  selected = "Home",

  header = tags$head(
    shinyjs::useShinyjs(),

    # Script to detect mobile devices
    tags$script('
      $(document).on("shiny:connected", function() {
        var mobile = window.matchMedia("(max-width: 768px)").matches;
        Shiny.setInputValue("is_mobile", mobile);
      });
    '),

    # Add meta tags
    tags$link(rel = "stylesheet",
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "css/styles.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"),
    tags$script(src = "js/main.js")

  ),

  bslib::nav_panel(
    title = "Home",
    homeNavigationModuleUI("home_navigation")
  ),

  bslib::nav_panel(
    title = "Calculator",
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
              downloadButton("download_calculations", "Save Financial Evolution calculations (.csv)",
                             class = "btn-sm")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,  # Full width on mobile
            class = "col-lg-6",  # Half width on large screens
            card(
              id = "asset_plot_card",
              card_header("Total Asset Evolution"),
              plotYearlyAssetProgressionModuleUI("YearlyAssetProgressionPlot")
            )
          ),
          column(
            width = 12,
            class = "col-lg-6",
            card(
              id = "financial_metrics_card",
              card_header("Financial Metrics"),
              financialMetricsModuleUI("financial_metrics")
            )
          )
        ),
        fluidRow(
          # Stacked Area Plots
          column(
            width = 12,
            class = "col-lg-6",
            card(
              id = "stacked_area_card_expenses",
              card_header("Expenses Components"),
              stackedAreaPlotModuleUI("expense_components", plot_type = "expenses")
            )
          ),
          column(
            width = 12,
            class = "col-lg-6",
            card(
              id = "stacked_area_card_income",
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
              id = "sensitivity_analysis_card",
              card_header("Sensitivity Analysis"),
              sensitivityAnalysisModuleUI("sensitivity_analysis")
            )
          )
        )
      )
    )
  ),
  footer = tags$footer(
    class = "footer",
    style = "position: fixed; bottom: 0; width: 100%; background-color: #f8f9fa; padding: 8px; text-align: left;",
    HTML(
      'Developed by chvieira2. Open source code available <a href="https://github.com/chvieira2/FinancialEvolutionCalculator" target="_blank">here</a>. Feel free to open an issue, or contact me.'
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to track the active tab
  active_tab <- reactiveVal("Home")  # Default tab is "Home"

  # Observe tab changes
  observe({
    # Use JavaScript to detect tab changes and update the reactive value
    shinyjs::runjs('
      $("a[data-bs-toggle=\'tab\']").on("shown.bs.tab", function(e) {
        var activeTab = $(e.target).text().trim();
        Shiny.setInputValue("active_tab", activeTab, {priority: "event"});
      });
    ')
  })

  # Update the reactive value when the tab changes
  observeEvent(input$active_tab, {
    active_tab(input$active_tab)
    print(paste("Active tab:", active_tab()))  # Debugging: Print the active tab
  })

  # Debuggin helper for mobile
  observe({
    cat("Mobile detection:", input$is_mobile, "\n")
    output$debug_info <- renderText({
      glue::glue("Screen: {input$dimension[1]}x{input$dimension[2]}")
    })
  })

  # Call the Home Navigation Module
  homeNavigationModuleServer("home_navigation",
                             if (is.null(isolate(input$is_mobile))) FALSE else isolate(input$is_mobile))

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
  # Observe the active tab and initialize the plots when the "Calculator" tab is selected
  observeEvent(active_tab(), {
    if (active_tab() == "Calculator") {
      req(processed_data())
      req(reactive_config())
      req(year_range())
      is_mobile <- isolate(input$is_mobile)
      if (is.null(is_mobile)) is_mobile <- FALSE
      # Trigger the plot modules
      plotYearlyAssetProgressionModuleServer("YearlyAssetProgressionPlot", processed_data, reactive_config, year_range)
      financialMetricsModuleServer("financial_metrics", processed_data, year_range)
      stackedAreaPlotModuleServer("expense_components", processed_data, year_range, plot_type = "expenses")
      stackedAreaPlotModuleServer("income_components", processed_data, year_range, plot_type = "income")
      sensitivityAnalysisModuleServer("sensitivity_analysis", reactive_config, processed_data, year_range, is_mobile)
    }
  })


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
}

shinyApp(ui, server)
