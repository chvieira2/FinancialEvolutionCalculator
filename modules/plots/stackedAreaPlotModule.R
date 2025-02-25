library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define preset configurations for different plot types
PLOT_CONFIGS <- list(
  income = list(
    title = "Income Components Over The Years, inflation-corrected (thousands, €)",
    colors = c(
      "Net Salary" = "#66C2A5",
      "Passive Investment Returns" = "#E78AC3",
      "Rental Income" = "#FC8D62",
      "Income Taxes Deduction" = "#8DA0CB",
      "Lump sums" = "#A6D854"
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
      # Home Expenses (Blues)
      "Living Standard Costs" = "#A6CEE3",
      "Kids-related living Costs" = "#1F78B4",
      "Rent" = "#B2DF8A",
      "Housing Costs (Nebenkosten)" = "#33A02C",

      # Investment Property Costs
      "Properties Mortgage/Loan Principal Share" = "#B15928",
      "Properties Mortgage/Loan Interest Share" = "#D6604D",
      "Properties Property Taxes" = "#2166AC",
      "Properties Maintenance" = "#B2182B",
      "Properties Hausgeld" = "#D1E5F0",
      "Investment Properties Management Fees" = "#F4A582",
      "Investment Properties Vacancy Costs" = "#053061"
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

      # Rent
      list(id = "rental_housing_cost", label = "Housing Costs (Nebenkosten)"),
      list(id = "rental_cost", label = "Rent"),

      # Home Expenses
      list(id = "living_costs_family", label = "Kids-related living Costs"),
      list(id = "living_standard_costs", label = "Living Standard Costs")
    )
  )
)

stackedAreaPlotModuleUI <- function(id, plot_type) {
  ns <- NS(id)

  # Get configuration for the specified plot type
  config <- PLOT_CONFIGS[[plot_type]]

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
    config <- PLOT_CONFIGS[[plot_type]]

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

      # Get the data ready
      plot_data <- filtered_data()
      selected_ids <- selected_components()

      # Get the corresponding labels for the selected components
      selected_labels <- sapply(config$components[sapply(config$components, function(x) x$id %in% selected_ids)],
                                `[[`, "label")

      # Create long format data
      plot_data_long <- plot_data %>%
        select(Year, all_of(selected_ids)) %>%
        pivot_longer(
          cols = -Year,
          names_to = "Component",
          values_to = "Value"
        )

      # Create a named vector for the label mapping
      label_mapping <- setNames(selected_labels, selected_ids)

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
        theme(
          strip.text = element_text(size = 14, face = "bold"),
          legend.position = "none",
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text = element_text(size = 9)
        )
    })
  })
}
