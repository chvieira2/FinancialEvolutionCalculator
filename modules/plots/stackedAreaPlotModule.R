library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define preset configurations for different plot types
STACKED_AREA_PLOT_CONFIGS <- list(
  income = list(
    title = "Income Components Over The Years, inflation-corrected (thousands, €)",
    colors = c(
      "Net Salary" = "#1A8754",                   # Rich Green - primary income
      "Passive Investment Returns" = "#4361EE",    # Royal Blue - investment theme
      "Rental Income" = "#F4A261",                # Sandy Orange - property theme
      "Income Taxes Deduction" = "#9D4EDD",       # Purple - tax related
      "Lump sums" = "#E63946"                     # Red - one-time/special income
    ),
    components = list(
      list(id = "lump_sums", label = "Lump sums"),
      list(id = "properties_income_taxes_adjustment", label = "Income Taxes Deduction"),
      list(id = "properties_warm_lease_income", label = "Rental Income"),
      list(id = "passive_investment_return_from_previous_year", label = "Passive Investment Returns"),
      list(id = "net_annual_salary", label = "Net Salary")
    )
  ),
  expenses = list(
    title = "Expense Components Over The Years, inflation-corrected (thousands, €)",
    colors = c(
      # Living Expenses (warm orange/red family)
      "Living Costs" = "#FF7F50",         # Coral
      "Kids-related Costs" = "#E34234",     # Vermilion

      # Renting Costs (blue family)
      "Rent" = "#4169E1",                          # Royal Blue
      "Housing Costs (Nebenkosten)" = "#1E90FF",   # Dodger Blue

      # Passive investment taxes (purple family)
      "Advance Lump-Sum Taxation (Vorabpauschale)" = "#9370DB", # Medium Purple

      # Investment Property Costs (green and brown family)
      "Properties Mortgage/Loan Principal Share" = "#2E8B57",  # Sea Green
      "Properties Mortgage/Loan Interest Share" = "#3CB371",   # Medium Sea Green
      "Properties Property Taxes" = "#8FBC8F",                 # Dark Sea Green
      "Properties Maintenance" = "#6B8E23",                    # Olive Drab
      "Properties Hausgeld" = "#BDB76B",                       # Dark Khaki
      "Investment Properties Management Fees" = "#A0522D",     # Sienna
      "Investment Properties Vacancy Costs" = "#CD853F"        # Peru
    ),
    components = list(
      # Investment Property Costs
      list(id = "properties_vacancy_months_cost", label = "Investment Properties Vacancy Costs"),
      list(id = "properties_property_management_fee", label = "Investment Properties Management Fees"),
      list(id = "properties_hausgeld_fees_total", label = "Properties Hausgeld"),
      list(id = "properties_maintenance_cost", label = "Properties Maintenance"),
      list(id = "properties_property_taxes", label = "Properties Property Taxes"),
      list(id = "properties_interest_share", label = "Properties Mortgage/Loan Interest Share"),
      list(id = "properties_principal_share", label = "Properties Mortgage/Loan Principal Share"),

      # Passive investment taxes
      list(id = "vorabpauschale_tax_from_previous_year", label = "Advance Lump-Sum Taxation (Vorabpauschale)"),

      # Rent
      list(id = "rental_housing_cost", label = "Housing Costs (Nebenkosten)"),
      list(id = "rental_cost", label = "Rent"),

      # Home Expenses
      list(id = "living_costs_family", label = "Kids-related Costs"),
      list(id = "living_standard_costs", label = "Living Costs")
    )
  )
)

stackedAreaPlotModuleUI <- function(id, plot_type) {
  ns <- NS(id)

  # Get configuration for the specified plot type
  config <- STACKED_AREA_PLOT_CONFIGS[[plot_type]]

  # Create mapping between display names and column names
  component_mapping <- setNames(
    sapply(config$components, `[[`, "id"),
    sapply(config$components, `[[`, "label")
  )

  tagList(
    div(
      class = "plot_container stacked-area-container",
      # Plot container
      div(
        class = "plot_container",
        plotOutput(ns("stacked_area_plot"))
      ),

      # Custom checkbox group
      div(
        style = "flex: 0 0 auto;",
        tags$label(paste("Select", tolower(plot_type), "components to display:"),
                   style = "font-size: 12px; font-weight: bold; margin-bottom: 0px;"),
        div(
          style = "margin-top: 0px;",
          lapply(names(config$colors), function(component_label) {
            component_id <- component_mapping[component_label]
            component_color <- config$colors[[component_label]]

            span(
              class = "checkbox-inline",
              style = sprintf("margin-right: 8px; font-size: 11px; font-weight: bold;"),
              tags$input(
                type = "checkbox",
                id = ns(paste0("component_", component_id)),
                name = ns("components"),
                value = component_id,
                checked = "checked",
                style = "transform: scale(0.8); margin-right: 2px;"
              ),
              tags$label(
                `for` = ns(paste0("component_", component_id)),
                style = sprintf("color: %s !important; font-weight: normal;", component_color),
                component_label
              )
            )
          })
        )
      )
    )
  )
}

stackedAreaPlotModuleServer <- function(id, data, year_range, plot_type) {
  moduleServer(id, function(input, output, session) {

    # Get configuration for the specified plot type
    config <- STACKED_AREA_PLOT_CONFIGS[[plot_type]]

    # Reactive expression for selected components
    selected_components <- reactive({
      # Get all possible components
      components <- sapply(config$components, `[[`, "id")

      # Check which ones are selected
      selected <- sapply(components, function(component_id) {
        input[[paste0("component_", component_id)]]
      })

      # Return only the selected ones
      components[selected]
    })

    filtered_data <- reactive({
      req(data(), year_range())
      data() %>%
        filter(Year >= year_range()[1] & Year <= year_range()[2])
    })

    output$stacked_area_plot <- renderPlot({
      req(selected_components(), filtered_data())
      generateStackedAreaPlot(filtered_data(), selected_components(), plot_type)
    })
  })
}


generateStackedAreaPlot <- function(plot_data, selected_ids = NULL, plot_type, legend_bool = FALSE) {
  # Get configuration for the specified plot type
  config <- STACKED_AREA_PLOT_CONFIGS[[plot_type]]

  # If no components provided, use all available components
  if (is.null(selected_ids)) {
    selected_ids <- sapply(config$components, `[[`, "id")
  }

  # Get the corresponding labels for the selected components
  selected_labels <- sapply(config$components[sapply(config$components,function(x) x$id %in% selected_ids)], `[[`, "label")

  # Create long format data
  plot_data_long <- plot_data %>%
    select(Year, all_of(selected_ids)) %>%
    pivot_longer(
      cols = -Year,
      names_to = "Component",
      values_to = "Value"
    )

  # Filter out components that have all zero values
  non_zero_components <- plot_data_long %>%
    group_by(Component) %>%
    summarize(has_non_zero = sum(Value, na.rm = TRUE) != 0) %>%
    filter(has_non_zero) %>%
    pull(Component)

  # Filter data to include only non-zero components
  plot_data_long <- plot_data_long %>%
    filter(Component %in% non_zero_components)

  # Update selected labels to include only non-zero components
  selected_labels <- selected_labels[selected_ids %in% non_zero_components]

  # Create a named vector for the label mapping
  label_mapping <- setNames(selected_labels, selected_ids[selected_ids %in% non_zero_components])

  # Add labels to the data
  plot_data_long$ComponentLabel <- factor(
    label_mapping[plot_data_long$Component],
    levels = selected_labels
  )

  # Create the stacked area plot
  ggplot(plot_data_long,
         aes(x = Year, y = Value/1000, fill = ComponentLabel)) +
    geom_area(alpha = 0.8) +
    theme_minimal() +
    labs(title = config$title,
         x = NULL,
         y = NULL) +
    scale_fill_manual(values = config$colors[selected_labels]) +
    scale_x_continuous(breaks = seq(min(plot_data$Year),
                                    max(plot_data$Year),
                                    by = if(max(plot_data$Year) - min(plot_data$Year) <= 15) 1
                                    else if(max(plot_data$Year) - min(plot_data$Year) <= 25) 2
                                    else 4)) +
    scale_y_continuous(
      labels = scales::label_number(
        big.mark = ",",
        accuracy = 1,
        scale = 1,
        suffix = ""
      )
    ) +
    guides(fill = guide_legend(ncol = 2)) +
    theme(
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = if(legend_bool) "bottom" else "none",
      legend.title = element_blank(),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text = element_text(size = 9)
    )
}

if (sys.nframe() == 0) {
  source(file.path("R", "helper_functions.R"))
  source(file.path("R", "constants.R"))

  for (scenario in TEMPLATE_SCENARIOS) {
    scenario <- sub(".yaml*$", "", scenario)
    scenario <- sub("inputs_", "", scenario)

    # Define test parameters
    config <- safelyLoadConfig(file.path("config", "templates",
                                         paste0("inputs_", scenario, ".yaml")))
    plot_data <- read.csv(file.path("article", "calculations",
                                    paste0("calculations_", scenario, ".csv")))

    # Generate and save income plot
    message("Generating income stacked area plot...")
    income_plot <- generateStackedAreaPlot(
      plot_data = plot_data,
      plot_type = "income",
      legend_bool = TRUE  # Enable legend for standalone plots
    )

    income_output_path <- file.path("article", "figures", paste0("IncomeComp_", scenario, ".png"))
    ggsave(income_output_path, plot = income_plot, width = 10, height = 6, dpi = 300)
    message(paste("Income plot saved to", income_output_path))

    # Generate and save expenses plot
    message("Generating expenses stacked area plot...")
    expenses_plot <- generateStackedAreaPlot(
      plot_data = plot_data,
      plot_type = "expenses",
      legend_bool = TRUE  # Enable legend for standalone plots
    )

    expenses_output_path <- file.path("article", "figures", paste0("ExpensesComp_", scenario, ".png"))
    ggsave(expenses_output_path, plot = expenses_plot, width = 10, height = 6, dpi = 300)
    message(paste("Expenses plot saved to", expenses_output_path))
  }
}
