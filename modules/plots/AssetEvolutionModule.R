library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

plotYearlyAssetProgressionModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "plot_container",
        plotOutput(ns("YearlyAssetProgressionPlot")))
  )
}

plotYearlyAssetProgressionModuleServer <- function(id, data, reactive_config, year_range) {
  moduleServer(id, function(input, output, session) {

    filtered_data <- reactive({
      req(data(), year_range())
      data() %>%
        filter(Year >= year_range()[1] & Year <= year_range()[2])
    })

    output$YearlyAssetProgressionPlot <- renderPlot({
      # Validate config parameters before initialization
      ConfigDefaultSetting <- ConfigDefaultSetting$new()
      config <- ConfigDefaultSetting$check_parameters_within_bounds(isolate(reactive_config()))

      generateYearlyAssetProgressionPlot(filtered_data(), config)
    })
  })
}


# Function to extract landmarks from config
get_landmarks <- function(config, year_range) {
  landmarks <- list()

  # Children
  # Get base children parameters
  first_child_year <- get_parameter_value(config, "family.year_first_child_is_born")
  second_child_year <- get_parameter_value(config, "family.year_second_child_is_born")
  num_extra_children <- get_parameter_value(config, "family.number_extra_children")
  first_extra_child_year <- get_parameter_value(config, "family.year_first_extra_child")
  extra_child_spacing <- get_parameter_value(config, "family.extra_child_born_after")

  # Process all children
  if (!is.null(first_child_year) && first_child_year < year_range[[2]]) {
    landmarks$child_1 <- list(
      year = first_child_year,
      year_end = Inf,
      label = "Child 1 birth",
      is_retirement = FALSE,
      color = "#2CA02C"  # Bright green
    )

    if (!is.null(second_child_year) && second_child_year < year_range[[2]]) {
      landmarks$child_2 <- list(
        year = second_child_year,
        year_end = Inf,
        label = "Child 2 birth",
        is_retirement = FALSE,
        color = "#2CA02C"  # Same green for consistency
      )

      # Handle extra children if any
      if (!is.null(num_extra_children) && num_extra_children > 0 && first_extra_child_year < year_range[[2]]) {
        for (extra_child_number in 3:(num_extra_children+2)) {

          # Calculate year for this child
          extra_child_year <- if (extra_child_number == 3) {
            first_extra_child_year
          } else {
            first_extra_child_year + ((extra_child_number-1) * extra_child_spacing)
          }

          landmarks[[paste0("child_", extra_child_number)]] <- list(
            year = extra_child_year,
            year_end = Inf,
            label = paste("Child", extra_child_number, "birth"),
            is_retirement = FALSE,
            color = "#2CA02C"  # Same green for consistency
          )
        }
      }
    }
  }

  # Lump sums
  lump_sum_1_year <- get_parameter_value(config, "lump_sum.lump_sum_1_year")
  lump_sum_2_year <- get_parameter_value(config, "lump_sum.lump_sum_2_year")
  if (!is.null(lump_sum_1_year) && lump_sum_1_year < year_range[[2]]) {
    landmarks$lump_sum_1 <- list(
      year = lump_sum_1_year,
      year_end = Inf,
      label = "Lump Sum 1",
      is_retirement = FALSE,
      color = "#9467BD"  # Distinct purple
    )
  }
  if (!is.null(lump_sum_2_year) && lump_sum_2_year < year_range[[2]]) {
    landmarks$lump_sum_2 <- list(
      year = lump_sum_2_year,
      year_end = Inf,
      label = "Lump Sum 2",
      is_retirement = FALSE,
      color = "#9467BD"  # Same purple for consistency
    )
  }

  # Properties
  property_colors <- c("#FF7F0E",  # Orange
                       "#E377C2",   # Pink
                       "#17BECF")   # Cyan

  if (!is.null(config$properties)) {
    for (i in seq_along(config$properties)) {
      property <- config$properties[[i]]
      purchase_year <- get_parameter_value(config,
                                           sprintf("properties.%s.purchase_year", property$name))
      sale_year <- get_parameter_value(config,
                                       sprintf("properties.%s.sale_year", property$name))

      if (!is.null(purchase_year) && purchase_year < year_range[[2]]) {
        # Use modulo to cycle through colors if more properties than colors
        color_index <- ((i-1) %% length(property_colors)) + 1

        landmarks[[paste0(property$name, "_purchase")]] <- list(
          year = purchase_year,
          year_end = ifelse(sale_year > year_range[[2]], Inf, sale_year),
          label = sprintf("%s (%s)",
                          property$name,
                          if(property$type == "home") "Home" else "Investment"),
          is_retirement = FALSE,
          color = property_colors[color_index]
        )
      }
    }
  }

  # Retirement
  retirement_year <- get_parameter_value(config, "general_life.expected_year_retirement")
  if (!is.null(retirement_year) && retirement_year < year_range[[2]]) {
    landmarks$retirement <- list(
      year = retirement_year,
      year_end = Inf,
      label = "Retirement",
      color = "#2C3E50",
      is_retirement = TRUE
    )
  }

  return(landmarks)
}

generateYearlyAssetProgressionPlot <- function(plot_data, config) {
  year_range <- list(min(plot_data$Year), max(plot_data$Year))

  # Get year range for x-axis breaks
  year_range_len <- year_range[[2]] - year_range[[1]]
  year_step <- if (year_range_len <= 15) 1 else if (year_range_len <= 25) 2 else 4
  years_to_display <- seq(min(plot_data$Year),
                          max(plot_data$Year),
                          by = year_step)

  # Calculate y-axis limits
  min_y <- max(-100, min(plot_data$total_asset/1000))
  max_y <- max(50, max(plot_data$total_asset/1000))

  # Create base plot
  p <- ggplot() +
    theme_minimal()

  # Add landmarks
  landmarks <- get_landmarks(config, year_range)

  # Create data frame for legend with order preservation
  legend_data <- data.frame(
    x = numeric(),
    y = numeric(),
    year_end = numeric(),
    label = character(),
    color = character(),
    order = numeric(),
    stringsAsFactors = FALSE
  )

  # Counter for preserving order
  order_counter <- 1

  # Create data frames for ranges and points
  range_data <- data.frame(
    xmin = numeric(),
    xmax = numeric(),
    ymin = numeric(),
    ymax = numeric(),
    fill = character(),
    is_retirement = logical(),
    stringsAsFactors = FALSE
  )

  point_data <- data.frame(
    x = numeric(),
    color = character(),
    stringsAsFactors = FALSE
  )

  for (landmark in landmarks) {
    if (grepl("Retirement|property", landmark$label)) {

      if (landmark$year_end > min(plot_data$Year)) {
        # Add to range_data
        if(landmark$is_retirement) {
          range_data <- rbind(range_data,
                              data.frame(
                                xmin = landmark$year,
                                xmax = Inf,
                                ymin = -Inf,
                                ymax = Inf,
                                fill = landmark$color,
                                is_retirement = TRUE
                              ))
        } else {
          range_data <- rbind(range_data,
                              data.frame(
                                xmin = landmark$year,
                                xmax = landmark$year_end,
                                ymin = -Inf,
                                ymax = max_y*0.2,
                                fill = landmark$color,
                                is_retirement = FALSE
                              ))
        }
      }
    } else {
      # Add to point_data
      if (landmark$year >= min(plot_data$Year)) {
        point_data <- rbind(point_data,
                            data.frame(
                              x = landmark$year,
                              color = landmark$color
                            ))
      }
    }

    # Add to legend data
    if (landmark$year <= max(plot_data$Year)) {
      legend_data <- rbind(legend_data,
                           data.frame(
                             x = min(plot_data$Year),
                             y = max_y,
                             year_end = landmark$year_end,
                             label = landmark$label,
                             color = landmark$color,
                             order = order_counter,
                             is_retirement = landmark$is_retirement,
                             stringsAsFactors = FALSE
                           ))
      order_counter <- order_counter + 1
    }
  }

  # Add ranges and points to plot
  if (nrow(range_data) > 0) {
    # Add retirement ranges
    retirement_ranges <- range_data[range_data$is_retirement, ]
    if (nrow(retirement_ranges) > 0) {
      p <- p + geom_rect(data = retirement_ranges,
                         aes(xmin = xmin, xmax = xmax,
                             ymin = ymin, ymax = ymax),
                         fill = retirement_ranges$fill,
                         alpha = 0.2)
    }

    # Add other ranges
    other_ranges <- range_data[!range_data$is_retirement, ]
    if (nrow(other_ranges) > 0) {
      p <- p + geom_rect(data = other_ranges,
                         aes(xmin = xmin, xmax = xmax,
                             ymin = ymin, ymax = ymax),
                         fill = other_ranges$fill,
                         alpha = 0.1)
    }
  }

  # Add point landmarks
  if (nrow(point_data) > 0) {
    p <- p + geom_vline(data = point_data,
                        aes(xintercept = x),
                        color = point_data$color,
                        linetype = "dashed",
                        alpha = 0.8)
  }

  # Modify the legend creation section:
  if (nrow(legend_data) > 0) {
    # Create separate data frames for retirement and other landmarks
    range_legend <- legend_data %>%
      filter(grepl("Retirement|property", label)) %>%
      group_by(label, color) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(order)

    other_legend <- legend_data %>%
      filter(!grepl("Retirement|property", label)) %>%
      group_by(label, color) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(order)

    # Add dummy segments for legend only if we have data
    if (nrow(range_legend) > 0) {
      p <- p +
        # For retirement: solid rectangle
        geom_rect(data = range_legend,
                  aes(xmin = x,
                      xmax = x + 1,
                      ymin = y - 0.1,
                      ymax = y + 0.1,
                      fill = label),
                  alpha = 0.2) +
        scale_fill_manual(values = setNames(range_legend$color,
                                            range_legend$label))
    }

    if (nrow(other_legend) > 0) {
      p <- p +
        # For other landmarks: dashed line
        geom_segment(data = other_legend,
                     aes(x = x,
                         xend = x + 1,
                         y = y,
                         yend = y,
                         color = label),
                     linetype = "dashed") +
        scale_color_manual(values = setNames(other_legend$color,
                                             other_legend$label))
    }

    # Add guides only if we have both types of landmarks
    p <- p + guides(
      color = guide_legend(override.aes = list(
        linetype = "dashed",
        alpha = 0.8
      )),
      fill = guide_legend(override.aes = list(
        alpha = 0.2
      ))
    )
  }

  # Combine the legends
  p <- p +
    # Main asset line
    geom_line(data = plot_data,
              aes(x = Year, y = total_asset/1000),
              color = "#2C3E50",
              linewidth = 1) +

    coord_cartesian(ylim = c(min_y, max_y))

  # In the final plot additions, combine the legends:
  p + labs(title = "Total Asset Value Evolution, inflation-corrected (thousands, €)",
           subtitle = "Total asset includes all properties, savings, debts, and investments",
           x = NULL,
           y = NULL) +  # Remove duplicate color and fill labels
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "gray40"),
          legend.box = "vertical",    # Change from "horizontal" to "vertical"
          legend.margin = margin(),   # Reduce margin between legends
          legend.spacing = unit(0.1, "cm"),  # Reduce spacing between legends
          legend.title = element_blank()) +  # Remove legend titles
    scale_x_continuous(breaks = years_to_display) +
    scale_y_continuous(
      labels = scales::label_number(
        big.mark = ",",
        accuracy = 1,
        scale = 1
      )
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
    output_path <- file.path("article", "figures",
                             paste0("AssetEvolution_", scenario,".png"))

    # Generate the plot
    message("Generating test plot...")
    test_plot <- generateYearlyAssetProgressionPlot(plot_data, config)

    # Save the plot
    ggsave(output_path, plot = test_plot, width = 10, height = 6, dpi = 300)
    message(paste("Plot saved to", output_path))

    }
}
