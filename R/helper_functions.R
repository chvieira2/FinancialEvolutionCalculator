config_cache <- new.env()

#' Safely load YAML configuration
#' @param file_path Path to the YAML file
#' @return Configuration list or NULL if error
safelyLoadConfig <- function(file_path) {
  tryCatch({
    if (is.null(config_cache[[file_path]])) {
      config_cache[[file_path]] <- yaml::read_yaml(file_path)
    }
    config_cache[[file_path]]
  }, error = function(e) {
    warning(sprintf("Error loading config file %s: %s", file_path, e$message))
    NULL
  })
}

#' Create an input element based on type and configuration
#' @param input_type The type of input ("numericInput", "sliderInput", etc.)
#' @param input_id The ID for the input element
#' @param input_config List of configuration parameters
#' @return A Shiny input element
createInputElement <- function(input_type, input_id, input_config) {
  switch(input_type,
         "numericInput" = numericInput(
           inputId = input_id,
           label = NULL,
           value = input_config$value
         ),
         "sliderInput" = sliderInput(
           inputId = input_id,
           label = NULL,
           min = input_config$min,
           max = input_config$max,
           value = input_config$value,
           step = input_config$step
         ),
         "checkboxInput" = checkboxInput(
           inputId = input_id,
           label = NULL,
           value = input_config$value
         ),
         "selectInput" = selectInput(
           inputId = input_id,
           label = NULL,
           choices = unlist(lapply(input_config$choices, function(x) x[[1]])),
           selected = unlist(lapply(input_config$choices, function(x) x[[1]]))[1],
           multiple = FALSE,
           selectize = FALSE
         ),
         # Default to numericInput
         numericInput(
           input_id,
           label = NULL,
           value = input_config$value
         )
  )
}

#' Create an info icon with tooltip
#' @param description The tooltip text
#' @return A tags element containing the icon
createInfoIcon <- function(description) {
  tags$i(
    class = "fas fa-info-circle info-icon",
    title = description,
    `data-toggle` = "tooltip"
  )
}

#' Create a container div for sidebar inputs
#' @param label The input label
#' @param input_element The Shiny input element
#' @param info_icon The info icon element
#' @return A div containing the formatted input
createInputContainer <- function(label, input_element, info_icon) {
  div(
    class = "sidebar-input-container",
    style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 5px;",
    div(
      style = "flex: 3.5; margin-right: 15px; display: flex; align-items: center;",
      tags$label(label, `for` = input_element$attribs$id,
                 style = "margin-bottom: 0px; margin-top: 0px; line-height: 1;"),
      div(style = "margin-left: 5px;", info_icon)
    ),
    div(style = "flex: 1.5;", input_element)
  )
}

#' Create a property input
#' @param input_item Input configuration
#' @param property_name Name of the property
#' @param ns Namespace function
#' @return A div containing the property input
createPropertyInput <- function(input_item, property_name, ns) {
  input_id <- ns(paste0(property_name, "_", input_item$id))

  input_element <- createInputElement(
    input_item$type,
    input_id,
    input_item
  )

  info_icon <- createInfoIcon(input_item$description)

  createInputContainer(input_item$label, input_element, info_icon)
}

#' Create a delete button for a property
#' @param ns Namespace function
#' @param property_id Name of the property
#' @return An actionButton
createDeleteButton <- function(ns, property_id) {
  actionButton(
    inputId = ns(paste0("delete_property_", property_id)),
    label = "Delete",
    class = "btn-sm",
    style = "padding: 1px 5px; font-size: 0.8rem;"
  ) %>%
    tagAppendAttributes(
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
        ns("delete_property_trigger"),
        property_id
      )
    )
}

#' Get parameter value from config
#' @param config Configuration list
#' @param parameter_id Parameter identifier in format "section.parameter" or "properties.property_name.parameter"
#' @return Parameter value or NULL if not found
#' @examples
#' get_parameter_value(config, "taxes_description.inflation")
#' get_parameter_value(config, "properties.property1.value_growth")
get_parameter_value <- function(config, parameter_id) {
  # Split parameter_id into parts
  parts <- strsplit(parameter_id, "\\.")[[1]]

  # Handle properties separately
  if (parts[1] == "properties") {
    if (length(parts) != 3) {
      warning("Invalid property parameter id format")
      return(NULL)
    }

    property_name <- parts[2]
    param_id <- parts[3]

    # Find the property using both name and id fields
    property <- NULL
    for (p in config$properties) {
      if ((!is.null(p$name) && p$name == property_name) ||
          (!is.null(p$id) && p$id == property_name)) {
        property <- p
        break
      }
    }

    if (is.null(property)) {
      return(NULL)
    }

    # Find the parameter in property inputs
    for (input in property$inputs) {
      if (input$id == param_id) {
        return(input$value)
      }
    }

  } else {
    if (length(parts) != 2) return(NULL)

    # Handle regular parameters
    section <- parts[1]
    param_id <- parts[2]

    # Check if section exists
    if (!section %in% names(config)) return(NULL)

    # Find parameter in section inputs
    # First check in inputs list
    if ("inputs" %in% names(config[[section]])) {
      for (input in config[[section]]$inputs) {
        if (input$id == param_id) {
          return(input$value)
        }
      }
    }

    # Then check direct section parameters
    if (param_id %in% names(config[[section]])) {
      return(config[[section]][[param_id]])
    }
  }

  return(NULL)
}

#' Modify parameter value in config
#' @param config Configuration list
#' @param parameter_id Parameter identifier in format "section.parameter" or "properties.property_name.parameter"
#' @param new_value New value for the parameter
#' @return Modified configuration list
#' @examples
#' modify_config(config, "taxes_description.inflation", 2.5)
#' modify_config(config, "properties.property1.value_growth", 5.0)
modify_config <- function(config, parameter_id, new_value) {
  # Create a copy of the config
  modified_config <- config

  # Split parameter_id into parts
  parts <- strsplit(parameter_id, "\\.")[[1]]

  # Handle properties separately
  if (parts[1] == "properties") {
    if (length(parts) != 3) {
      warning("Invalid property parameter id format")
      return(config)
    }

    property_name <- parts[2]
    param_id <- parts[3]

    # Find and modify the property
    for (i in seq_along(modified_config$properties)) {
      if (modified_config$properties[[i]]$name == property_name) {
        for (j in seq_along(modified_config$properties[[i]]$inputs)) {
          if (modified_config$properties[[i]]$inputs[[j]]$id == param_id) {
            modified_config$properties[[i]]$inputs[[j]]$value <- new_value
            return(modified_config)
          }
        }
      }
    }

  } else {
    # Handle regular parameters
    section <- parts[1]
    param_id <- parts[2]

    # Check if section exists
    if (!section %in% names(modified_config)) {
      return(config)
    }

    # Find and modify parameter in section inputs
    for (i in seq_along(modified_config[[section]]$inputs)) {
      if (modified_config[[section]]$inputs[[i]]$id == param_id) {
        modified_config[[section]]$inputs[[i]]$value <- new_value
        return(modified_config)
      }
    }
  }

  return(modified_config)
}
