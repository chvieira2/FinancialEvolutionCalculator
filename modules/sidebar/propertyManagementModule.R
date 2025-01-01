library(shiny)
library(bslib)

source(file.path("R", "constants.R"))
source(file.path("R", "utils", "helper_functions.R"))

propertyManagementUI <- function(id, initial_config) {
  ns <- NS(id)

  # Create a local reactive value to store property state
  local_properties <- reactiveVal(NULL)

  # Create property input fields
  createPropertyInputs <- function(property) {
    property_name <- make.names(property$name)

    div(
      id = ns(paste0("property_", property_name)),
      div(
        style = "border-bottom: 1px solid #dee2e6; margin-bottom: 10px; padding-bottom: 5px;",
        # Header
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h4(
            style = "margin: 0; font-size: 0.9rem;",
            paste0(property$name, " (", property$type, ")")
          ),
          createDeleteButton(ns, property$name)
        ),
        # All inputs in one section
        div(
          style = "margin-top: 5px;",
          lapply(property$inputs, function(input_item) {
            input_id <- paste0(property$name, "_", input_item$id)

            div(
              class = "sidebar-input-container",
              style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 5px;",
              div(
                style = "flex: 3.5; margin-right: 15px;",
                tags$label(input_item$label)
              ),
              div(
                style = "flex: 1.5;",
                numericInput(
                  ns(input_id),
                  label = NULL,
                  value = input_item$value
                )
              )
            )
          })
        )
      )
    )
  }

  tagList(
    h3("Properties owned:"),
    uiOutput(ns("property_list")),
    h3("Add new property:"),
    div(
      style = "margin-top: 10px;",
      textInput(ns("new_property_name"), "Property name (optional)"),
      radioButtons(
        inputId = ns("new_property_type"),
        label = "Property type",
        choices = PROPERTY_TYPES,
        inline = TRUE
      ),
      actionButton(ns("add_property"), "Add property")
    )
  )
}

propertyManagementServer <- function(id, reactive_config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create property with unique ID
    createPropertyWithId <- function(property) {
      if (is.null(property$id)) {
        property$id <- uuid::UUIDgenerate()  # Add uuid package to dependencies
      }
      return(property)
    }

    # # Create a local reactive value to store property state
    # local_properties <- reactiveVal(NULL)
    #
    # # Update local properties when reactive_config changes
    # observeEvent(reactive_config(), {
    #   # print("Updating local properties from reactive_config")
    #   local_properties(reactive_config()$properties)
    # }, ignoreInit = FALSE)

    # Render property list
    output$property_list <- renderUI({
      req(reactive_config())
      properties <- reactive_config()$properties

      # For Debugging:
      # print("\nRendering property list:")
      # if (!is.null(properties)) {
      #   for (prop in properties) {
      #     print(sprintf("Property: %s (Type: %s)", prop$name, prop$type))
      #     key_params <- c("value_today", "purchase_year", "sale_year", "loan_family_friends")
      #     for (param_id in key_params) {
      #       param <- Find(function(x) x$id == param_id, prop$inputs)
      #       if (!is.null(param)) {
      #         print(sprintf("  %s: %s", param_id, param$value))
      #       }
      #     }
      #   }
      # }

      if (is.null(properties) || length(properties) == 0) {
        return(div(
          style = "font-style: italic; color: #666; font-size: 0.8rem; margin: 10px 0;",
          "No properties added yet"
        ))
      }

      # Create property cards with debugging
      property_cards <- lapply(properties, function(property) {
        # Ensure property has an ID
        if (is.null(property$id)) {
          property$id <- uuid::UUIDgenerate()
        }
        # For Debugging
        # print(paste("Creating card for property:", property$name))
        # # Print only key input values
        # key_params <- c("value_today", "purchase_year", "sale_year", "loan_family_friends")
        # for (param_id in key_params) {
        #   param <- Find(function(x) x$id == param_id, property$inputs)
        #   if (!is.null(param)) {
        #     print(sprintf("  %s: %s", param_id, param$value))
        #   }
        # }

        div(
          id = ns(paste0("property_", property$id)),
          div(
            style = "border-bottom: 1px solid #dee2e6; margin-bottom: 10px; padding-bottom: 5px;",
            # Header
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4(
                style = "margin: 0; font-size: 0.9rem;",
                paste0(property$name, " (", property$type, ")")
              ),
              createDeleteButton(ns, property$id)
            ),
            # All inputs in one section
            div(
              style = "margin-top: 5px;",
              lapply(property$inputs, function(input_item) {
                input_id <- paste0(property$id, "_", input_item$id)

                div(
                  class = "sidebar-input-container",
                  style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 5px;",
                  div(
                    style = "flex: 3.5; margin-right: 15px;",
                    tags$label(input_item$label)
                  ),
                  div(
                    style = "flex: 1.5;",
                    numericInput(
                      ns(input_id),
                      label = NULL,
                      value = input_item$value
                    )
                  )
                )
              })
            )
          )
        )
      })

      do.call(tagList, property_cards)
    })

    # Handle adding new property
    observeEvent(input$add_property, {
      # Load property defaults
      property_defaults <- safelyLoadConfig(file.path("config", "defaults", "PropertyDefaultValues.yaml"))

      new_property_name <- if (input$new_property_name != "") {
        input$new_property_name
      } else {
        existing_names <- sapply(isolate(reactive_config()$properties), function(p) p$name)
        i <- 1
        while (paste0("property", i) %in% existing_names) {
          i <- i + 1
        }
        paste0("property", i)
      }

      # Get default values for the selected property type
      property_type <- input$new_property_type
      default_values <- property_defaults$properties[[
        which(sapply(property_defaults$properties, function(x) x$type == property_type))
      ]]$inputs

      new_property <- list(
        name = new_property_name,
        type = property_type,
        inputs = default_values
      )

      # Update configuration with new property
      updated_config <- isolate(reactive_config())
      updated_config$properties <- c(updated_config$properties, list(new_property))
      reactive_config(updated_config)

      # Reset property name input
      updateTextInput(session, "new_property_name", value = "")
    })

    # Handle property deletion
    observeEvent(input$delete_property_trigger, {
      property_id <- input$delete_property_trigger

      updated_config <- isolate(reactive_config())
      property_index <- which(sapply(updated_config$properties, function(p) p$id == property_id))

      if (length(property_index) > 0) {
        updated_config$properties <- updated_config$properties[-property_index]
        reactive_config(updated_config)
      }
    })

    # Handle property input changes
    observe({
      req(reactive_config())
      updated_config <- isolate(reactive_config())

      if (!is.null(updated_config$properties)) {
        any_changes <- FALSE

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
            input_id <- paste0(property$id, "_", input_item$id)  # Use ID instead of name
            current_value <- input[[input_id]]

            if (!is.null(current_value) && !identical(current_value, input_item$value)) {
              updated_config$properties[[i]]$inputs[[j]]$value <- current_value
              any_changes <- TRUE
            }
          }
        }

        if (any_changes) {
          reactive_config(updated_config)
        }
      }
    })
  })
}
