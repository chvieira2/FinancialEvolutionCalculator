library(shiny)
library(bslib)
library(purrr)

#' Sidebar Input Module UI
#'
#' @param id The module identifier
#' @export
sidebarInputModuleUI <- function(id) {
  ns <- NS(id)

  # Load default template configuration at UI creation
  default_template_file <- TEMPLATE_SCENARIOS[[DEFAULT_TEMPLATE]]
  initial_config <- safelyLoadConfig(file.path("config", "templates", default_template_file))

  # Helper function to create static inputs
  createStaticInput <- function(input_item, section) {
    input_id <- ns(paste0(section, "_", input_item$id))

    input_element <- createInputElement(
      input_item$type,
      input_id,
      input_item
    )

    info_icon <- createInfoIcon(input_item$description)

    createInputContainer(input_item$label, input_element, info_icon)
  }

  # Create static UI elements
  static_inputs <- if (!is.null(initial_config)) {
    lapply(names(initial_config), function(section) {
      section_inputs <- initial_config[[section]]$inputs
      essential_params <- ESSENTIAL_PARAMS[[section]]

      essential_inputs <- section_inputs[sapply(section_inputs,
                                                function(x) x$id %in% essential_params)]
      non_essential_inputs <- section_inputs[sapply(section_inputs,
                                                    function(x) !x$id %in% essential_params)]

      div(
        h3(style = "font-size: 1.5rem; margin-top: 15px; margin-bottom: 10px;",
           initial_config[[section]]$title),
        div(id = ns(paste0(section, "_essential")),
            lapply(essential_inputs, createStaticInput, section = section)),
        if (length(non_essential_inputs) > 0) {
          tagList(
            actionButton(
              inputId = ns(paste0(section, "_toggle")),
              label = "Show more parameters",
              class = "collapse-toggle",
              style = "border: none; background: none; color: #007bff; padding: 0; font-size: 0.8rem;"
            ),
            div(
              id = ns(paste0(section, "_non_essential")),
              class = "collapse",
              lapply(non_essential_inputs, createStaticInput, section = section)
            )
          )
        }      )
    })
  }

  tagList(
    div(class = "sidebar-title", "Financial Evolution Scenario"),

    # Template section
    div(
      style = "margin-bottom: 5px;",
      div(
        style = "margin-bottom: 5px; font-size: 0.9rem; color: #666;",
        "Start from a template scenario:"
      ),
      selectInput(
        ns("template_selector"),
        label = NULL,
        choices = names(TEMPLATE_SCENARIOS),
        selected = DEFAULT_TEMPLATE,
        width = "100%"
      )
    ),

    # Custom scenario section
    div(
      style = "margin-bottom: 5px;",
      div(
        style = "margin-bottom: 5px; font-size: 0.9rem; color: #666;",
        "Or load your own scenario:"
      ),
      div(
        class = "scenario-buttons-container",
        style = "display: flex; flex-direction: column; gap: 5px;",
        # File input for loading
        div(
          style = "width: auto; margin: 0; padding: 0;",
          fileInput(
            ns("scenario_file"),
            label = NULL,
            accept = c(".yml", ".yaml"),
            buttonLabel = "Load Scenario",
            placeholder = NULL
          )
        ),
        # Save button
        downloadButton(
          ns("save_scenario"),
          "Save Current Scenario",
          class = "btn-default save-scenario-btn"
        )
      )
    ),

    # Thinner divider
    tags$hr(style = "margin: 5px 0;"),

    # Rest of the inputs
    div(
      id = ns("static_inputs_container"),
      static_inputs,
      propertyManagementUI(ns("property_management"), initial_config = initial_config)
    )
  )
}

#' Sidebar Input Module Server
#'
#' @param id The module identifier
#' @export
sidebarInputModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize configuration with default template
    config <- reactiveVal({
      template_file <- TEMPLATE_SCENARIOS[[DEFAULT_TEMPLATE]]
      safelyLoadConfig(file.path("config", "templates", template_file))
    })

    # Handle template selection
    observeEvent(input$template_selector, {
      template_file <- TEMPLATE_SCENARIOS[[input$template_selector]]

      tryCatch({
        template_config <- safelyLoadConfig(file.path("config", "templates", template_file))

        if (!is.null(template_config)) {
          validation_result <- validateConfig(template_config)

          if (validation_result$is_valid) {
            # Clear existing properties and regenerate IDs for the new template
            if (!is.null(template_config$properties)) {
              for (i in seq_along(template_config$properties)) {
                template_config$properties[[i]]$id <- uuid::UUIDgenerate()  # Generate new unique IDs
              }
            }

            # Replace the entire configuration
            config(template_config)

            # Update UI inputs
            updateInputs(template_config)

            showNotification(
              paste("Template scenario", input$template_selector, "successfully loaded"),
              type = "message",
              duration = 3
            )
          } else {
            showNotification(
              paste("Invalid template configuration:",
                    paste(validation_result$messages, collapse = "; ")),
              type = "error",
              duration = 5
            )
          }
        }
      }, error = function(e) {
        showNotification(
          paste("Error loading template scenario:", e$message),
          type = "error",
          duration = 5
        )
      })
    }, ignoreInit = TRUE)

    # Handle input changes
    observe({
      # Get all input values that have changed
      current_inputs <- reactiveValuesToList(input)

      # Isolate the update to prevent circular reactions
      isolate({
        updated_config <- config()
        any_changes <- FALSE

        # Handle regular inputs
        for (section in names(updated_config)) {
          if (section != "properties") {
            for (i in seq_along(updated_config[[section]]$inputs)) {
              input_item <- updated_config[[section]]$inputs[[i]]
              input_id <- paste0(section, "_", input_item$id)
              input_value <- current_inputs[[input_id]]

              if (!is.null(input_value) && !identical(input_value, input_item$value)) {
                updated_config[[section]]$inputs[[i]]$value <- input_value
                any_changes <- TRUE
              }
            }
          }
        }

        # Handle property inputs
        if (!is.null(updated_config$properties)) {
          for (i in seq_along(updated_config$properties)) {
            property <- updated_config$properties[[i]]
            # Ensure property has an ID
            if (is.null(property$id)) {
              property$id <- uuid::UUIDgenerate()
              updated_config$properties[[i]]$id <- property$id
              any_changes <- TRUE
            }

            for (j in seq_along(property$inputs)) {
              input_item <- property$inputs[[j]]
              input_id <- paste0(property$id, "_", input_item$id)
              input_value <- current_inputs[[input_id]]

              if (!is.null(input_value) && !identical(input_value, input_item$value)) {
                updated_config$properties[[i]]$inputs[[j]]$value <- input_value
                any_changes <- TRUE
              }
            }
          }
        }

        if (any_changes) {
          config(updated_config)
        }
      })
    })

    # Helper function to update UI inputs
    updateInputs <- function(new_config) {
      for (section in names(new_config)) {
        if (section != "properties") {
          for (input_item in new_config[[section]]$inputs) {
            input_id <- paste0(section, "_", input_item$id)
            updateInput(session, input_id, input_item)
          }
        }
      }
    }

    # Handle file upload
    observeEvent(input$scenario_file, {
      req(input$scenario_file)

      tryCatch({
        uploaded_config <- safelyLoadConfig(input$scenario_file$datapath)

        if (!is.null(uploaded_config)) {
          validation_result <- validateConfig(uploaded_config)

          if (validation_result$is_valid) {
            # Completely replace the configuration in one go
            config(uploaded_config)

            # Update regular inputs
            for (section in names(uploaded_config)) {
              if (section != "properties") {
                for (input_item in uploaded_config[[section]]$inputs) {
                  input_id <- paste0(section, "_", input_item$id)
                  updateInput(session, input_id, input_item)
                }
              }
            }

            showNotification("Scenario configuration successfully loaded",
                             type = "message", duration = 3)
          } else {
            showNotification(
              paste("Invalid configuration:",
                    paste(validation_result$messages, collapse = "; ")),
              type = "error",
              duration = 5
            )
          }
        }
      }, error = function(e) {
        showNotification(
          paste("Error loading scenario:", e$message),
          type = "error",
          duration = 5
        )
      })
    }, ignoreInit = TRUE)

    # Add download handler for scenario configuration
    output$save_scenario <- downloadHandler(
      filename = function() {
        paste0("financial_scenario_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yaml")
      },
      content = function(file) {
        # Get current configuration
        current_config <- isolate(config())

        # Write to YAML file
        yaml::write_yaml(current_config, file)
      }
    )

    # Call the property management module server
    propertyManagementServer("property_management", config)

    # Return the config reactive value
    return(config)
  })
}

# Helper function to update inputs
updateInput <- function(session, input_id, input_item) {
  if (input_item$type == "sliderInput") {
    updateSliderInput(session, input_id, value = input_item$value)
  } else {
    updateNumericInput(session, input_id, value = input_item$value)
  }
}
