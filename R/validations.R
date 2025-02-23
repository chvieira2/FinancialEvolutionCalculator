#' Validate numeric input within bounds
#' @param value The input value
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param name Name of the input for error messages
#' @return List with is_valid and message
validateNumericBounds <- function(value, min = NULL, max = NULL, name = "Value") {
  if (!is.numeric(value)) {
    return(list(
      is_valid = FALSE,
      message = sprintf("%s must be a numeric value", name)
    ))
  }

  if (!is.null(min) && value < min) {
    return(list(
      is_valid = FALSE,
      message = sprintf("%s must be greater than or equal to %s", name, min)
    ))
  }

  if (!is.null(max) && value > max) {
    return(list(
      is_valid = FALSE,
      message = sprintf("%s must be less than or equal to %s", name, max)
    ))
  }

  list(is_valid = TRUE, message = NULL)
}

#' Validate year input
#' @param year Year value to validate
#' @param min_year Minimum allowed year
#' @param max_year Maximum allowed year
#' @param name Name of the year input
#' @return List with is_valid and message
validateYear <- function(year, min_year = 2025, max_year = 2100, name = "Year") {
  validateNumericBounds(year, min_year, max_year, name)
}

#' Validate percentage input
#' @param value Percentage value to validate
#' @param name Name of the percentage input
#' @return List with is_valid and message
validatePercentage <- function(value, name = "Percentage") {
  validateNumericBounds(value, 0, 100, name)
}

#' Validate property configuration
#' @param property Property configuration to validate
#' @return List with is_valid and messages
validateProperty <- function(property) {
  messages <- character(0)

  # Required fields
  required_fields <- c("name", "type", "inputs")
  missing_fields <- required_fields[!required_fields %in% names(property)]

  if (length(missing_fields) > 0) {
    messages <- c(messages,
                  sprintf("Missing required fields: %s", paste(missing_fields, collapse = ", ")))
  }

  # Validate property type
  if (!property$type %in% c("home", "investment")) {
    messages <- c(messages,
                  sprintf("Invalid property type: %s. Must be 'home' or 'investment'", property$type))
  }

  # Validate inputs
  if (!is.null(property$inputs)) {
    for (input in property$inputs) {
      if (input$id == "value_today") {
        result <- validateNumericBounds(input$value, 0, NULL, "Property value")
        if (!result$is_valid) messages <- c(messages, result$message)
      }
      if (input$id == "purchase_year") {
        result <- validateYear(input$value)
        if (!result$is_valid) messages <- c(messages, result$message)
      }
      if (input$id == "sale_year") {
        result <- validateYear(input$value)
        if (!result$is_valid) messages <- c(messages, result$message)
      }
      if (input$id == "loan_family_friends") {
        result <- validateCurrency(input$value, "Family loan amount")
      }
    }
  }

  list(
    is_valid = length(messages) == 0,
    messages = messages
  )
}

#' Validate entire configuration
#' @param config Configuration to validate
#' @return List with is_valid and messages
validateConfig <- function(config) {
  messages <- character(0)

  # Validate general life section
  if (!is.null(config$general_life)) {
    for (input in config$general_life$inputs) {
      if (input$id == "net_annual_income") {
        result <- validateNumericBounds(input$value, 0, NULL, "Net annual income")
        if (!result$is_valid) messages <- c(messages, result$message)
      }
      if (input$id == "expected_year_retirement") {
        result <- validateYear(input$value)
        if (!result$is_valid) messages <- c(messages, result$message)
      }
    }
  }

  # Validate properties
  if (!is.null(config$properties)) {
    for (property in config$properties) {
      result <- validateProperty(property)
      if (!result$is_valid) {
        messages <- c(messages,
                      sprintf("Property '%s': %s", property$name, paste(result$messages, collapse = "; ")))
      }
    }
  }

  list(
    is_valid = length(messages) == 0,
    messages = messages
  )
}

validateCurrency <- function(value, name = "Value") {
  if (!is.numeric(value) || value < 0) {
    return(list(is_valid = FALSE,
                message = paste(name, "must be a positive number")))
  }
  list(is_valid = TRUE)
}
