library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)


financialMetricsModuleUI <- function(id) {
  ns <- NS(id)

  # Create mapping between display names and column names
  metric_mapping <- setNames(
    sapply(FINANCIAL_METRIC_DEFINITIONS, `[[`, "id"),
    sapply(FINANCIAL_METRIC_DEFINITIONS, `[[`, "label")
  )

  tagList(
    div(
      class = "plot_container metrics-container",
      # Plot container
      div(
        class = "plot_container",
        plotOutput(ns("metrics_plot"))
      ),

      # Custom checkbox group
      div(
        style = "flex: 0 0 auto;",
        tags$label("Select metrics to display:",
                   style = "font-size: 12px; font-weight: bold; margin-bottom: 0px;"),
        div(
          style = "margin-top: 0px;",
          lapply(names(FINANCIAL_METRIC_COLORS), function(metric_label) {
            metric_id <- metric_mapping[metric_label]
            metric_color <- FINANCIAL_METRIC_COLORS[[metric_label]]

            span(
              class = "checkbox-inline",
              style = sprintf("margin-right: 8px; font-size: 11px; font-weight: bold;"),
              tags$input(
                type = "checkbox",
                id = ns(paste0("metric_", metric_id)),
                name = ns("metrics"),
                value = metric_id,
                checked = "checked",
                style = "transform: scale(0.8); margin-right: 2px;"
              ),
              tags$label(
                `for` = ns(paste0("metric_", metric_id)),
                style = sprintf("color: %s !important; font-weight: normal;", metric_color),
                metric_label
              )
            )
          })
        )
      )
    )
  )
}

financialMetricsModuleServer <- function(id, data, year_range) {
  moduleServer(id, function(input, output, session) {

    # Reactive expression for selected metrics
    selected_metrics <- reactive({
      # Get all possible metrics
      metrics <- sapply(FINANCIAL_METRIC_DEFINITIONS, `[[`, "id")

      # Check which ones are selected
      selected <- sapply(metrics, function(metric_id) {
        input[[paste0("metric_", metric_id)]]
      })

      # Return only the selected ones
      metrics[selected]
    })

    filtered_data <- reactive({
      req(data(), year_range())
      data() %>%
        mutate(
          passive_investment_money_needed =
            passive_investment_money_withdrawn_from_capital_gains +
            passive_investment_money_withdrawn_from_contributions
        ) %>%
        filter(Year >= year_range()[1] & Year <= year_range()[2])
    })

    output$metrics_plot <- renderPlot({
      req(selected_metrics(), filtered_data())

      # Get the data ready
      plot_data <- filtered_data()
      selected_ids <- selected_metrics()

      # Get the corresponding labels for the selected metrics
      selected_labels <- sapply(FINANCIAL_METRIC_DEFINITIONS[sapply(FINANCIAL_METRIC_DEFINITIONS, function(x) x$id %in% selected_ids)],
                                `[[`, "label")

      # Create long format data
      plot_data_long <- plot_data %>%
        select(Year, all_of(selected_ids)) %>%
        pivot_longer(
          cols = -Year,
          names_to = "Metric",
          values_to = "Value"
        )

      # Create a named vector for the label mapping
      label_mapping <- setNames(selected_labels, selected_ids)

      # Add labels to the data
      plot_data_long$MetricLabel <- factor(
        label_mapping[plot_data_long$Metric],
        levels = selected_labels
      )

      # Create the plot
      ggplot(plot_data_long, aes(x = Year, y = Value/1000, color = MetricLabel, group = MetricLabel)) +
        geom_line(linewidth = 1) +
        geom_point() +
        theme_minimal() +
        labs(title = "Finances evolution, inflation-corrected (thousands, €)",
             subtitle = "Summary of key financial metrics over the years",
             x = NULL,
             y = NULL) +
        scale_color_manual(values = FINANCIAL_METRIC_COLORS[selected_labels]) +
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
          legend.position = "none",
          strip.text = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "gray40"),
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text = element_text(size = 9)
        )
    })
  })
}
