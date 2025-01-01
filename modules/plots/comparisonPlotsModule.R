library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

comparisonPlotsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, plotOutput(ns("comparison_plot_1"))),
      column(6, plotOutput(ns("comparison_plot_2")))
    ),
    fluidRow(
      column(6, plotOutput(ns("comparison_plot_3"))),
      column(6, plotOutput(ns("comparison_plot_4")))
    )
  )
}

comparisonPlotsModule <- function(id, comparison_list) {
  moduleServer(id, function(input, output, session) {

    # Example comparison plot 1: Line plot of all strategies for each analysis
    output$comparison_plot_1 <- renderPlot({
      req(length(comparison_list()) > 0)

      plot_data <- bind_rows(lapply(seq_along(comparison_list()), function(i) {
        comparison_list()[[i]]$data %>%
          pivot_longer(cols = -Year, names_to = "Strategy", values_to = "Value") %>%
          mutate(Analysis = paste("Analysis", i))
      }))

      ggplot(plot_data, aes(x = Year, y = Value, color = Strategy, group = interaction(Strategy, Analysis))) +
        geom_line() +
        facet_wrap(~Analysis) +
        theme_minimal() +
        labs(title = "Comparison of Strategies Across Analyses")
    })

    # Example comparison plot 2: Boxplot of values for each strategy
    output$comparison_plot_2 <- renderPlot({
      req(length(comparison_list()) > 0)

      plot_data <- bind_rows(lapply(seq_along(comparison_list()), function(i) {
        comparison_list()[[i]]$data %>%
          pivot_longer(cols = -Year, names_to = "Strategy", values_to = "Value") %>%
          mutate(Analysis = paste("Analysis", i))
      }))

      ggplot(plot_data, aes(x = Strategy, y = Value, fill = Strategy)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Distribution of Values for Each Strategy")
    })

    # Example comparison plot 3: Heatmap of average values
    output$comparison_plot_3 <- renderPlot({
      req(length(comparison_list()) > 0)

      plot_data <- bind_rows(lapply(seq_along(comparison_list()), function(i) {
        comparison_list()[[i]]$data %>%
          pivot_longer(cols = -Year, names_to = "Strategy", values_to = "Value") %>%
          group_by(Strategy) %>%
          summarize(AvgValue = mean(Value)) %>%
          mutate(Analysis = paste("Analysis", i))
      }))

      ggplot(plot_data, aes(x = Analysis, y = Strategy, fill = AvgValue)) +
        geom_tile() +
        scale_fill_viridis_c() +
        theme_minimal() +
        labs(title = "Average Values Heatmap")
    })

    # Example comparison plot 4: Parameter comparison
    output$comparison_plot_4 <- renderPlot({
      req(length(comparison_list()) > 0)

      plot_data <- bind_rows(lapply(seq_along(comparison_list()), function(i) {
        data.frame(
          Analysis = paste("Analysis", i),
          Parameter = names(comparison_list()[[i]]$params),
          Value = unlist(comparison_list()[[i]]$params)
        )
      }))

      ggplot(plot_data, aes(x = Analysis, y = Value, fill = Parameter)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~Parameter, scales = "free_y") +
        theme_minimal() +
        labs(title = "Comparison of Input Parameters")
    })
  })
}
