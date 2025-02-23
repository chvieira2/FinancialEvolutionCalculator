library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)
library(shinyWidgets)

sensitivityAnalysisModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Control panel with more compact layout
    div(
      class = "row",
      # First column (wider) - Parameters
      div(
        class = "col-8",
        # Base parameters
        div(
          style = "margin-bottom: 10px;",
          h5("Base Parameters"),
          checkboxGroupInput(
            ns("BASE_PARAMETERS"),
            NULL,
            choices = setNames(
              sapply(BASE_PARAMETERS, `[[`, "id"),
              sapply(BASE_PARAMETERS, `[[`, "name")
            ),
            selected = sapply(BASE_PARAMETERS, `[[`, "id"),
            inline = TRUE
          )
        ),
        # Property parameters section
        div(
          style = "margin-bottom: 10px;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            h5("Property Parameters"),
          ),
          uiOutput(ns("property_parameters"))
        ),
        # Run Analysis button
        div(
          style = "text-align: left; margin-top: 10px;",
          actionButton(
            ns("run_analysis"),
            "Run/Re-run Analysis",
            class = "btn-primary"
          )
        )
      ),
      # Second column (narrower) - Analysis Settings
      div(
        class = "col-4",
        div(
          style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
          h5("Analysis Configuration"),
          div(
            style = "margin-bottom: 10px;",
            sliderInput(
              ns("variation_range"),
              "Parameter Variation Range (%)",
              min = -80, max = 80,
              value = c(-20, 20),
              step = 10,
              width = "100%"
            )
          ),
          div(
            numericInput(
              ns("steps"),
              "Number of Steps",
              value = 7,
              min = 3,
              max = 9,
              width = "100%"
            )
          )
        )
      )
    ),

    # Results section
    div(
      style = "margin-top: 20px;",
      # Faceted plots
      div(
        style = "margin-bottom: 20px;",
        uiOutput(ns("sensitivity_plot_container"))
      )
    )
  )
}

sensitivityAnalysisModuleServer <- function(id, reactive_config, processed_data, year_range) {
  moduleServer(id, function(input, output, session) {

    # Reactive value to store property parameters
    property_params <- reactiveVal(NULL)

    # Reactive value to store the selected parameters at time of analysis
    selected_parameters_snapshot <- reactiveVal(NULL)

    # Handler for loading property parameters
    observe({
      req(reactive_config())

      # Get properties from config
      properties <- reactive_config()$properties

      if (!is.null(properties)) {
        # For each property, add its parameters
        property_params_list <- lapply(properties, function(property) {
          # Select which parameters to include based on property type
          parameter_templates <- if(property$type == "investment") {
            property_parameter_template  # Include all parameters for investment properties
          } else {
            property_parameter_template[!names(property_parameter_template) %in% c("cold_lease_today")]  # Exclude investment parameters for home properties
          }

          # Create parameter list
          params <- lapply(parameter_templates, function(template) {
            list(
              id = sprintf("properties.%s.%s",
                           property$name,
                           template$id_suffix),
              name = sprintf("%s (%s - %s)",
                             template$name_suffix,
                             property$name,
                             property$type),
              color = template$color,
              type = template$type,
              property_type = property$type  # Store property type for faceting
            )
          })

          params
        })

        # Flatten the property parameters list
        property_params(unlist(property_params_list, recursive = FALSE))
      } else {
        property_params(NULL)
      }
    })

    # Render property parameters checkboxes
    output$property_parameters <- renderUI({
      params <- property_params()

      if (is.null(params)) {
        return(div(
          style = "color: #666; font-style: italic;",
          "No properties configured in the current scenario."
        ))
      }

      # Get previous selections if they exist
      previous_selections <- input$property_parameters_selection

      # If there are previous selections, keep only those that still exist
      selected <- if (!is.null(previous_selections)) {
        intersect(previous_selections, sapply(params, `[[`, "id"))
      } else {
        sapply(params, `[[`, "id")  # Select all by default
      }

      checkboxGroupInput(
        session$ns("property_parameters_selection"),
        NULL,
        choices = setNames(
          sapply(params, `[[`, "id"),
          sapply(params, `[[`, "name")
        ),
        selected = selected,
        inline = TRUE
      )
    })

    # Reactive to combine both sets of parameters
    available_parameters <- reactive({
      req(input$BASE_PARAMETERS)  # Only require base parameters

      # Get selected base parameters
      selected_base <- BASE_PARAMETERS[sapply(BASE_PARAMETERS, function(p) p$id %in% input$BASE_PARAMETERS)]

      # Get selected property parameters if any
      selected_property <- NULL
      if (!is.null(property_params()) && !is.null(input$property_parameters_selection)) {
        selected_property <- property_params()[sapply(property_params(),
                                                      function(p) p$id %in% input$property_parameters_selection)]
      }

      # Combine both sets (if property parameters exist)
      if (!is.null(selected_property)) {
        c(selected_base, selected_property)
      } else {
        selected_base
      }
    })

    # Render parameter selection checkboxes
    output$parameter_selection <- renderUI({
      params <- available_parameters()

      # Create checkboxes for each parameter
      checkboxGroupInput(
        session$ns("selected_parameters"),
        "Select Parameters for Analysis:",
        choices = setNames(
          sapply(params, `[[`, "id"),
          sapply(params, `[[`, "name")
        ),
        selected = sapply(params, `[[`, "id"),
        inline = TRUE
      )
    })

    # Reactive value to store analysis results
    analysis_results <- reactiveVal(NULL)

    # Handle Run Analysis button click
    observeEvent(input$run_analysis, {
      req(reactive_config(), available_parameters())

      # Get selected parameters
      selected_params <- available_parameters()

      if (length(selected_params) == 0) {
        showNotification(
          "Please select at least one parameter for analysis.",
          type = "warning"
        )
        return()
      }

      # Store snapshot of selected parameters
      selected_parameters_snapshot(selected_params)

      # Initialize results list
      all_parameter_results <- list()

      withProgress(
        message = 'Running sensitivity analysis. This might take a few seconds...',
        value = 0,
        {
          # For each selected parameter
          for(i in seq_along(selected_params)) {
            param <- selected_params[[i]]

            # Update progress
            incProgress(1/length(selected_params))

            # Get base value
            base_value <- get_parameter_value(reactive_config(), param$id)

            if (!is.null(base_value)) {
              # Create sequence of values based on parameter type
              if (param$type == "year") {
                # For year parameters, use integer sequence within available range
                min_year <- min(year_range())
                max_year <- max(year_range())

                # Create sequence centered around base value
                steps <- max(3, input$steps)
                variation_seq <- seq(base_value - steps, base_value + steps, by = 2)
                while (min(variation_seq) < min_year) {
                  variation_seq <- variation_seq + 1
                }
              } else {
                # For percentage parameters, use percentage variation
                variation_seq <- seq(
                  base_value * (1 + input$variation_range[1]/100),
                  base_value * (1 + input$variation_range[2]/100),
                  length.out = input$steps
                )
              }

              param_results <- list()

              for(j in seq_along(variation_seq)) {
                param_value <- variation_seq[j]
                modified_config <- modify_config(reactive_config(), param$id, param_value)

                data_processor <- DataProcessor$new(
                  scenario_name = sprintf("%s_%s", param$name, round(param_value, 2)),
                  config = modified_config,
                  initial_year = min(year_range()),
                  final_year = max(year_range())
                )

                data_processor$calculate()
                results <- data_processor$get_results()

                # Calculate variation percentage differently for years
                variation_pct <- if (param$type == "year") {
                  param_value - base_value  # Show absolute year difference
                } else {
                  (param_value/base_value - 1) * 100  # Show percentage change
                }

                results$param_value <- param_value
                results$param_variation <- variation_pct
                results$parameter <- param$name
                results$parameter_color <- param$color
                results$param_type <- param$type  # Add parameter type to results

                param_results[[j]] <- results
              }

              all_parameter_results[[param$name]] <- do.call(rbind, param_results)
            }
          }

          final_results <- do.call(rbind, all_parameter_results)
          analysis_results(final_results)
        }
      )
    })

    # Calculate plot heights based on number of parameters
    plot_heights <- reactive({
      req(selected_parameters_snapshot())

      n_params <- length(selected_parameters_snapshot())

      # Sensitivity plot: minimum 400px, then 150px per parameter row (3 columns)
      sensitivity_height <- max(400, ceiling(n_params/3) * 200)

      list(
        sensitivity = sensitivity_height
      )
    })

    # Render sensitivity plot container
    output$sensitivity_plot_container <- renderUI({
      req(plot_heights())
      plotOutput(session$ns("sensitivity_plots"),
                 height = sprintf("%dpx", plot_heights()$sensitivity))
    })

    # Render faceted sensitivity plots
    output$sensitivity_plots <- renderPlot({
      req(analysis_results(), selected_parameters_snapshot())

      # Get initial year from the data
      initial_year <- min(analysis_results()$Year)

      # Get the order of parameters from the snapshot instead of current selections
      ordered_params <- sapply(selected_parameters_snapshot(), `[[`, "name")

      # Find global y-axis limits
      y_limits <- range(analysis_results()$total_asset/1000)
      y_limits[1] <- -100
      y_range <- diff(y_limits)

      # Calculate text spacing
      text_height <- y_range * 0.1  # Height of each text entry

      # Calculate fixed y positions for all texts
      max_y <- y_limits[2] + y_range * 0.15  # Position for parameter headers
      text_positions <- seq(
        from = max_y - text_height,  # Start just below header
        by = -text_height,           # Stack downwards
        length.out = input$steps     # One position per variation
      )

      # Prepare text annotations with property type information
      text_data <- analysis_results() %>%
        mutate(
          parameter = factor(parameter, levels = ordered_params),
          # Extract property type from parameter name if it exists
          property_type = case_when(
            grepl("\\(investment\\)", parameter) ~ "Investment Property",
            grepl("\\(home\\)", parameter) ~ "Home Property",
            TRUE ~ "General Parameter"
          )
        ) %>%
        group_by(parameter, param_variation) %>%
        slice_tail(n = 1) %>%
        group_by(parameter) %>%
        arrange(desc(param_variation)) %>%
        mutate(
          value_text = case_when(
            param_type == "year" ~ sprintf("%.0f", param_value),
            param_type == "percentage" ~ sprintf("%.1f%%", param_value),
            param_type == "currency" ~ sprintf("%.0f €", param_value),
            TRUE ~ sprintf("%.1f", param_value)
          ),
          text_x = initial_year,
          text_y = text_positions[row_number()]
        ) %>%
        ungroup()

      # Create parameter headers
      param_headers <- text_data %>%
        group_by(parameter) %>%
        slice_head(n = 1) %>%
        mutate(
          value_text = sprintf("%s:", parameter),
          text_y = max_y,  # Fixed position for all headers
          param_variation = 0  # Neutral color
        )

      # Combine regular text and headers
      text_data_combined <- bind_rows(param_headers, text_data)

      # Prepare main plot data with property type
      plot_data <- analysis_results() %>%
        mutate(
          parameter = factor(parameter, levels = ordered_params),
          property_type = case_when(
            grepl("\\(investment\\)", parameter) ~ "Investment Property",
            grepl("\\(home\\)", parameter) ~ "Home Property",
            TRUE ~ "General Parameter"
          )
        )

      # Adjust y limits to accommodate text including headers
      y_limits_adjusted <- c(y_limits[1],
                             max_y + y_range * 0.1)  # Add small padding above headers

      ggplot() +
        # Add lines
        geom_line(
          data = plot_data,
          aes(x = Year,
              y = total_asset/1000,
              color = param_variation,
              group = param_variation)
        ) +
        # Add text annotations
        geom_text(
          data = text_data_combined,
          aes(x = text_x,
              y = text_y,
              label = value_text,
              color = param_variation),
          hjust = 0,
          vjust = 0,
          size = 4,
          show.legend = FALSE
        ) +
        # Modified faceting to include property type
        facet_wrap(~parameter, ncol = 3,
                   labeller = label_wrap_gen(width = 30)) +
        scale_color_gradient2(
          low = "blue",
          mid = "#2C3E50",
          high = "red",
          midpoint = 0
        ) +
        coord_cartesian(ylim = y_limits_adjusted, xlim = year_range()) +
        scale_x_continuous(
          limits = c(initial_year, NA)
        ) +
        theme_minimal() +
        labs(
          title = "Single Parameter Impact Range on Asset Value Evolution",
          subtitle = "All values are inflation-adjusted to today's euros",
          x = NULL,
          y = "Total Assets (thousands, €)",
          color = "Parameter\nVariation (% or years)"
        ) +
        theme(
          strip.text = element_text(size = 12, face = "bold"),
          strip.background = element_rect(
            fill = "lightgray",
            color = NA
          ),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "gray", fill = NA)
        )
    })
  })
}
