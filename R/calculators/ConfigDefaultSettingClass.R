library(R6)

#' Parameter validation class that provides validation functionality
#' Can be inherited by other classes needing parameter validation
ConfigDefaultSetting <-
  R6Class("ConfigDefaultSetting",
          public = list(
            #' Validate and set default values for configuration parameters
            #' @param config Configuration settings read from a YAML file
            #' @return The validated configuration with invalid values replaced by defaults
            check_parameters_within_bounds = function(config) {
              # Define validation rules with default values
              validation_rules <- list(
                # General Life
                net_annual_income = list(
                  min = 0, max = 100000, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100000 }
                ),
                living_style_costs = list(
                  min = 0, max = 100000, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 50000 }
                ),
                percent_income_investing = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                savings = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                savings_emergency_reserve = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                salaries_growth_start_career = list(
                  min = -5, max = 50, default = 0,
                  validator = function(val) { is.numeric(val) && val >= -5 && val <= 50 }
                ),
                salaries_growth_end_career = list(
                  min = -5, max = 50, default = 0,
                  validator = function(val) { is.numeric(val) && val >= -5 && val <= 50 }
                ),
                expected_year_retirement = list(
                  min = 2000, max = 2100, default = 2100,
                  validator = function(val) { is.numeric(val) && val >= 2000 && val <= 2100 }
                ),
                expected_salary_reduction_retirement = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),

                # Family
                annual_cost_per_child_0_3 = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                annual_cost_per_child_4_10 = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                annual_cost_per_child_11_18 = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                annual_cost_per_child_19_25 = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                elterngeld = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                living_costs_change_elternzeit = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                salary_reduction_elternzeit = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                year_first_child_is_born = list(
                  min = 2000, max = 2100, default = 2100,
                  validator = function(val) { is.numeric(val) && val >= 2000 && val <= 2100 }
                ),
                year_second_child_is_born = list(
                  min = 2000, max = 2100, default = 2100,
                  validator = function(val) { is.numeric(val) && val >= 2000 && val <= 2100 }
                ),
                number_extra_children = list(
                  min = 0, max = 10, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 10 }
                ),
                year_first_extra_child = list(
                  min = 2000, max = 2100, default = 2100,
                  validator = function(val) { is.numeric(val) && val >= 2000 && val <= 2100 }
                ),
                extra_child_born_after = list(
                  min = 1, max = 30, default = 1,
                  validator = function(val) { is.numeric(val) && val >= 1 && val <= 30 }
                ),

                # Rental
                rent_month = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= Inf }
                ),
                rental_prices_growth = list(
                  min = -5, max = 30, default = 0,
                  validator = function(val) { is.numeric(val) && val >= -5 && val <= 30 }
                ),
                fixed_housing_costs = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= Inf }
                ),

                # Passive Investing
                expected_return_on_investment = list(
                  min = -10, max = 30, default = 0,
                  validator = function(val) { is.numeric(val) && val >= -10 && val <= 30 }
                ),
                expected_conservative_return_on_investment = list(
                  min = -10, max = 20, default = 0,
                  validator = function(val) { is.numeric(val) && val >= -10 && val <= 20 }
                ),
                apply_vorabpauschale = list(
                  min = FALSE, max = TRUE, default = TRUE,
                  validator = function(val) { is.logical(val) }
                ),
                base_interest_rate_vorabpauschale = list(
                  min = -5, max = 50, default = 2.85,
                  validator = function(val) { is.numeric(val) && val >= -5 && val <= 50 }
                ),
                investment_type = list(
                  valid_values = c("Equity Fund", "Mixed Fund", "Domestic Real Estate Fund", "Foreign Real Estate Fund"),
                  default = "Equity Fund",
                  validator = function(val) { is.character(val) && val %in% c("Equity Fund", "Mixed Fund", "Domestic Real Estate Fund", "Foreign Real Estate Fund") }
                ),
                tax_free_allowance_type = list(
                  valid_values = c("Single Person (1000€)", "Married Couple (2000€)"),
                  default = "Married Couple (2000€)",
                  validator = function(val) { is.character(val) && val %in% c("Single Person (1000€)", "Married Couple (2000€)") }
                ),

                # Taxes and Inflation
                inflation = list(
                  min = -5, max = 50, default = 0,
                  validator = function(val) { is.numeric(val) && val >= -5 && val <= 50 }
                ),
                interest_rate_cash_flow_debt = list(
                  min = 0, max = 50, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 50 }
                ),
                income_tax = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                solidarity_surcharge_tax = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                church_tax = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                capital_gains_tax_rate = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),

                # Lump Sums
                lump_sum_1 = list(
                  min = -Inf, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) }
                ),
                lump_sum_1_year = list(
                  min = 2000, max = 2100, default = 2100,
                  validator = function(val) { is.numeric(val) && val >= 2000 && val <= 2100 }
                ),
                lump_sum_2 = list(
                  min = -Inf, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) }
                ),
                lump_sum_2_year = list(
                  min = 2000, max = 2100, default = 2100,
                  validator = function(val) { is.numeric(val) && val >= 2000 && val <= 2100 }
                ),

                # Property Tax
                estate_agent_fees = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                notary_fees_purchase = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                purchase_tax = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                real_estate_agent_fees_sale = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                notary_fees_sale = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                property_transfer_taxes_sale = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                miscelanous_fees_costs_sale = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),

                # Property specific parameters
                value_growth = list(
                  min = -10, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= -10 && val <= 100 }
                ),
                value_today = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                purchase_year = list(
                  min = 2000, max = 2100, default = 2025,
                  validator = function(val) { is.numeric(val) && val >= 2000 && val <= 2100 }
                ),
                sale_year = list(
                  min = 2000, max = 2100, default = 2100,
                  validator = function(val) { is.numeric(val) && val >= 2000 && val <= 2100 }
                ),
                initial_interest_rate = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                principal_repayment_rate = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                max_mortgage_amortization_rate = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                loan_family_friends = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                interest_rate_family_friends = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                duration_payback_family_friends = list(
                  min = 1, max = 30, default = 1,
                  validator = function(val) { is.numeric(val) && val >= 1 && val <= 30 }
                ),
                property_taxes_rate = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                maintenance_cost_rate = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                ),
                hausgeld_fees_total = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                hausgeld_maintenance_costs = list(
                  min = 0, max = Inf, default = 00,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                hausgeld_utilities = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                hausgeld_reserve_fund = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                hausgeld_house_management_fee = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                hausgeld_building_insurance = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                property_value = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                mortgage_amount = list(
                  min = 0, max = Inf, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 }
                ),
                mortgage_interest_rate = list(
                  min = 0, max = 100, default = 0,
                  validator = function(val) { is.numeric(val) && val >= 0 && val <= 100 }
                )
              )

              # Process all parameter groups
              for (group in names(config)) {
                if (group != "properties") {  # Handle properties separately
                  for (i in seq_along(config[[group]]$inputs)) {
                    input_id <- config[[group]]$inputs[[i]]$id
                    input_value <- config[[group]]$inputs[[i]]$value

                    # Check if this parameter has validation rules
                    if (input_id %in% names(validation_rules)) {
                      rule <- validation_rules[[input_id]]

                      # Apply validation
                      if (is.null(input_value)) {
                        # Log the replacement for NULL values
                        warning(sprintf("Invalid value 'NULL' for parameter '%s'. Using default: %s",
                                        input_id,
                                        as.character(rule$default)))
                        config[[group]]$inputs[[i]]$value <- rule$default
                      } else if ("valid_values" %in% names(rule)) {
                        # Handle categorical/string parameters
                        if (!rule$validator(input_value)) {
                          warning(sprintf("Invalid value '%s' for parameter '%s'. Using default: %s",
                                          as.character(input_value),
                                          input_id,
                                          as.character(rule$default)))
                          config[[group]]$inputs[[i]]$value <- rule$default
                        }
                      } else if ("validator" %in% names(rule) && !rule$validator(input_value)) {
                        # Handle numeric parameters
                        warning(sprintf("Invalid value '%s' for parameter '%s'. Using default: %s",
                                        as.character(input_value),
                                        input_id,
                                        as.character(rule$default)))
                        config[[group]]$inputs[[i]]$value <- rule$default
                      }
                    }
                  }
                }
              }

              # Validate properties
              if ("properties" %in% names(config)) {
                for (p in seq_along(config$properties)) {
                  property <- config$properties[[p]]
                  for (i in seq_along(property$inputs)) {
                    input_id <- property$inputs[[i]]$id
                    input_value <- property$inputs[[i]]$value

                    # Check if this parameter has validation rules
                    if (input_id %in% names(validation_rules)) {
                      rule <- validation_rules[[input_id]]

                      # Apply validation
                      if (is.null(input_value)) {
                        # Log the replacement for NULL values
                        warning(sprintf("Invalid value 'NULL' for property parameter '%s' in '%s'. Using default: %s",
                                        input_id,
                                        property$name,
                                        as.character(rule$default)))
                        config$properties[[p]]$inputs[[i]]$value <- rule$default
                      } else if ("valid_values" %in% names(rule)) {
                        # Handle categorical/string parameters
                        if (!rule$validator(input_value)) {
                          warning(sprintf("Invalid value '%s' for property parameter '%s' in '%s'. Using default: %s",
                                          as.character(input_value),
                                          input_id,
                                          property$name,
                                          as.character(rule$default)))
                          config$properties[[p]]$inputs[[i]]$value <- rule$default
                        }
                      } else if ("validator" %in% names(rule) && !rule$validator(input_value)) {
                        # Handle numeric parameters
                        warning(sprintf("Invalid value '%s' for property parameter '%s' in '%s'. Using default: %s",
                                        as.character(input_value),
                                        input_id,
                                        property$name,
                                        as.character(rule$default)))
                        config$properties[[p]]$inputs[[i]]$value <- rule$default
                      }
                    }
                  }
                }
              }

              return(config)
            }
          )
  )
