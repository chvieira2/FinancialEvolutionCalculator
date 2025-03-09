library(R6)

#### BaseCalculator Class ####
BaseCalculator <-
  R6Class("BaseCalculator",
          public = list(
            initialize = function() {
            },

            round_to_2 = function(x) {
              return(round(x, 2))
            },

            correct_by_inflation = function(value, inflation) {
              return(((1 + value/100) / (1 + inflation/100)) - 1)
            },

            calculate_funds_from_property_sale = function(year, results, properties) {
              sale_funds <- 0
              for (temp_property_name in names(properties)) {
                temp_property_params <- properties[[temp_property_name]]
                if (year == temp_property_params[["sale_year"]]) {
                  previous_year <- year - 1
                  sale_funds <- sale_funds +
                    results[results$Year == previous_year,
                            paste0(temp_property_name, "_value_liquid")]
                }
              }

              return(self$round_to_2(sale_funds))
            },

            check_if_loan_taken = function(year, results) {
              loan_taken <- sum(results[results$Year == year, grep("mortgage_principal", names(results), value = TRUE)])

              return(loan_taken)
            }
          )
  )

#### GeneralCalculator Class ####
GeneralCalculator <-
  R6Class("GeneralCalculator",
          inherit = BaseCalculator,
          public = list(
            calculate_general = function(year, params, results) {
              self$params <- params
              self$results <- results
              # Inflation remains constant throughout the years
              self$results$inflation <- self$round_to_2(self$params$inflation)

              # General calculations (always performed)
              private$calculate_salaries_growth(year)
              private$calculate_salary(year)
              private$calculate_living_costs_change_elternzeit(year)
              private$calculate_living_costs_family(year)
              private$calculate_living_costs(year)
              private$calculate_expected_money_for_investment_savings(year)
              private$calculate_net_annual_salary(year)
              private$calculate_lump_sums(year)
              private$calculate_expected_yearly_return_on_passive_investment_inflation_corrected(year)
              private$calculate_total_taxes_on_income(year)
              private$calculate_capital_gains_tax_rate(year)


              private$calculate_passive_investment_return_from_previous_year(year)

              # Calculate Vorabpauschale tax if enabled
              if (self$params$apply_vorabpauschale && year != params$initial_year) {
                private$calculate_passive_investment_vorabpauschale(year)
                private$calculate_passive_investment_vorabpauschale_tax_from_previous_year(year)
              } else {
                private$calculate_passive_investment_vorabpauschale(year)
                self$results[self$results$Year == year, "vorabpauschale_tax_from_previous_year"] <- 0
                self$results[self$results$Year == year, "accumulated_vorabpauschale_tax"] <- 0
              }

              private$calculate_housing_cost_growth_inflation_corrected(year)
              private$calculate_housing_cost(year)

              return(self$results)
            }
          ),

          private = list(

            calculate_salaries_growth = function(year) {
              # Salary growth decreases linearly from start of career to retirement
              if (year == self$params$initial_year) {
                growth <- self$params$salaries_growth_start_career
              } else if (year > self$params$expected_year_retirement) {
                growth <- 0
              } else {
                years_until_retirement <- self$params$expected_year_retirement - self$params$initial_year
                total_decrease <- self$params$salaries_growth_start_career - self$params$salaries_growth_end_career
                yearly_decrease <- total_decrease / years_until_retirement
                growth <- self$params$salaries_growth_start_career - yearly_decrease * (year - self$params$initial_year)
              }

              # Adjust growth rate for inflation
              inflation <- self$results[self$results$Year == year, "inflation"]
              inflation_corrected_growth <- self$correct_by_inflation(growth, inflation)

              # Reduce salary upon retirement
              if (year == self$params$expected_year_retirement) {
                inflation_corrected_growth <- -self$params$expected_salary_reduction_retirement/100
              }

              self$results[self$results$Year == year, "salaries_growth_inflation_corrected"] <-
                self$round_to_2(inflation_corrected_growth * 100)  # Convert back to percentage
            },

            calculate_salary = function(year) {
              # Apply inflation-corrected growth rate to previous year's salary
              if (year == self$params$initial_year) {
                self$results[self$results$Year == year, "salary"] <- self$params$net_annual_income * 12
              } else {
                previous_year <- year - 1
                previous_salary <- self$results[self$results$Year == previous_year, "salary"]
                growth_rate <- self$results[self$results$Year == previous_year, "salaries_growth_inflation_corrected"]
                new_salary <- previous_salary * (1 + growth_rate/100)
                self$results[self$results$Year == year, "salary"] <- self$round_to_2(new_salary)
              }
            },

            calculate_living_costs_change_elternzeit = function(year) {
              change <- 0
              if ((!is.null(self$params$year_first_child_is_born) && year == self$params$year_first_child_is_born) ||
                  (!is.null(self$params$year_second_child_is_born) && year == self$params$year_second_child_is_born)) {
                change <- self$params$living_costs_change_elternzeit
              }
              # Add similar checks for extra children if needed

              self$results[self$results$Year == year, "living_costs_change_elternzeit"] <- self$round_to_2(change)
            },

            calculate_child_cost = function(year, child_birth_year) {
              # Function to calculate cost for a single child
              if (is.null(child_birth_year)) return(0)
              child_age <- year - child_birth_year
              if (child_age < 0) return(0)
              if (child_age <= 3) return(-self$params$annual_cost_per_child_0_3 + self$params$elterngeld)
              if (child_age <= 10) return(-self$params$annual_cost_per_child_4_10 + self$params$elterngeld)
              if (child_age <= 18) return(-self$params$annual_cost_per_child_11_18 + self$params$elterngeld)
              if (child_age <= 25) return(-self$params$annual_cost_per_child_19_25 + self$params$elterngeld)
              return(0)
            },

            calculate_living_costs_family = function(year) {
              total_cost <- 0

              # Calculate costs for each child
              if (!is.null(self$params$year_first_child_is_born) && year >= self$params$year_first_child_is_born) {
                total_cost <- total_cost + private$calculate_child_cost(year, self$params$year_first_child_is_born)
              }
              if (!is.null(self$params$year_second_child_is_born) && year >= self$params$year_second_child_is_born) {
                total_cost <- total_cost + private$calculate_child_cost(year, self$params$year_second_child_is_born)
              }
              # Add similar calculations for extra children if needed

              self$results[self$results$Year == year, "living_costs_family"] <- self$round_to_2(total_cost)
            },

            calculate_living_costs = function(year) {
              base_living_costs <- -self$params$living_style_costs * 12
              change_elternzeit <- self$results[self$results$Year == year, "living_costs_change_elternzeit"] / 100
              family_costs <- self$results[self$results$Year == year, "living_costs_family"]

              living_standard_costs <- base_living_costs * (1 - change_elternzeit)
              self$results[self$results$Year == year, "living_standard_costs"] <- self$round_to_2(living_standard_costs)
              total_living_costs <- living_standard_costs + family_costs
              self$results[self$results$Year == year, "living_costs"] <- self$round_to_2(total_living_costs)
            },

            calculate_expected_money_for_investment_savings = function(year) {
              if (year == self$params$initial_year) {
                value <- self$params$percent_income_investing/100*self$params$net_annual_income*12
              } else {
                value <- self$params$percent_income_investing/100*self$results[self$results$Year == year, "salary"]
              }

              self$results[self$results$Year == year, "expected_money_for_investment_savings"] <- self$round_to_2(value)
            },

            calculate_net_annual_salary = function(year) {
              if (year == self$params$initial_year) {
                base_salary <- self$params$net_annual_income * 12
              } else {
                base_salary <- self$results[self$results$Year == year, "salary"]
              }

              # Apply salary reduction for Elternzeit
              reduction <- 1
              if (!is.null(self$params$year_first_child_is_born) && year == self$params$year_first_child_is_born) {
                reduction <- 1 - (self$params$salary_reduction_elternzeit / 100)
              } else if (!is.null(self$params$year_second_child_is_born) && year == self$params$year_second_child_is_born) {
                reduction <- 1 - (self$params$salary_reduction_elternzeit / 100)
              }
              reduced_salary <- base_salary * reduction

              # Remove the expected money for passive investing
              net_salary <- reduced_salary - self$results[self$results$Year == year, "expected_money_for_investment_savings"]

              # Add similar checks for extra children if needed
              self$results[self$results$Year == year, "net_annual_salary"] <- self$round_to_2(net_salary)
            },

            calculate_lump_sums = function(year) {
              total_lump_sum <- 0

              if (!is.null(self$params$lump_sum_1_year) && year == self$params$lump_sum_1_year && !is.null(self$params$lump_sum_1) && self$params$lump_sum_1 != 0) {
                total_lump_sum <- total_lump_sum + self$params$lump_sum_1
              }

              if (!is.null(self$params$lump_sum_2_year) && year == self$params$lump_sum_2_year && !is.null(self$params$lump_sum_2) && self$params$lump_sum_2 != 0) {
                total_lump_sum <- total_lump_sum + self$params$lump_sum_2
              }

              # Add more lump sums here if needed

              self$results[self$results$Year == year, "lump_sums"] <- self$round_to_2(total_lump_sum)
            },

            calculate_expected_yearly_return_on_passive_investment_inflation_corrected = function(year) {
              inflation <- self$results[self$results$Year == year, "inflation"]
              expected_return_on_investment_inflation_corrected <- self$correct_by_inflation(self$params$expected_return_on_investment, inflation)
              expected_conservative_return_on_investment_inflation_corrected <- self$correct_by_inflation(self$params$expected_conservative_return_on_investment, inflation)


              years_until_retirement <- self$params$expected_year_retirement - self$params$initial_year
              yearly_decrease <- (expected_return_on_investment_inflation_corrected - expected_conservative_return_on_investment_inflation_corrected) / years_until_retirement

              if (year == self$params$initial_year) {
                value <- expected_return_on_investment_inflation_corrected
              } else if (year >= self$params$expected_year_retirement) {
                value <- expected_conservative_return_on_investment_inflation_corrected
              } else {
                previous_year <- year - 1
                previous_value <- self$results[self$results$Year == previous_year, "expected_yearly_return_on_passive_investment_inflation_corrected"]/100
                value <- previous_value - yearly_decrease
              }

              self$results[self$results$Year == year, "expected_yearly_return_on_passive_investment_inflation_corrected"] <- self$round_to_2(value * 100)  # Convert to percentage
            },

            calculate_total_taxes_on_income = function(year) {
              total_tax <- (self$params$income_tax / 100*(1+self$params$solidarity_surcharge_tax / 100+self$params$church_tax / 100))*100
              self$results[self$results$Year == year, "total_taxes_on_income"] <- self$round_to_2(total_tax)
            },

            calculate_capital_gains_tax_rate = function(year) {
              total_tax <- (self$params$capital_gains_tax_rate / 100*(1+self$params$solidarity_surcharge_tax / 100+self$params$church_tax / 100))*100

              self$results[self$results$Year == year, "capital_gains_tax_rate"] <- self$round_to_2(total_tax)
            },

            calculate_passive_investment_return_from_previous_year = function(year) {
              if (year == self$params$initial_year) {
                if (self$params$savings > self$params$savings_emergency_reserve) {
                  return_value <- (self$params$savings - self$params$savings_emergency_reserve) *
                    (self$params$expected_return_on_investment / 100) *
                    (1 - self$results[self$results$Year == year, "inflation"] / 100)
                } else {
                  return_value <- 0
                }
              } else {
                previous_year <- year - 1
                return_value <- max(0,
                                    self$results[self$results$Year == previous_year, "expected_yearly_return_on_passive_investment_inflation_corrected"] / 100 *
                                      self$results[self$results$Year == previous_year, "passive_investment_total_invested"]
                )
              }
              self$results[self$results$Year == year, "passive_investment_return_from_previous_year"] <- self$round_to_2(return_value)
            },

            calculate_passive_investment_vorabpauschale = function(year) {
              # Get the tax-free allowance
              tax_free_allowance <- switch(self$params$tax_free_allowance_type,
                                           "single" = 1000,
                                           "married" = 2000,
                                           1000) # Default to single


              if (year == self$params$initial_year) {
                self$results[self$results$Year == year, "passive_investment_vorabpauschale"] <-
                  self$round_to_2(tax_free_allowance)

              } else {
                previous_year <- year - 1

                inflation_previous_year <-
                  self$results[self$results$Year == previous_year, "inflation"]

                passive_investment_vorabpauschale_previous_year <-
                  self$results[self$results$Year == previous_year, "passive_investment_vorabpauschale"]

                self$results[self$results$Year == year, "passive_investment_vorabpauschale"] <-
                  self$round_to_2(passive_investment_vorabpauschale_previous_year * (1 + inflation_previous_year / 100))
              }
            },

            calculate_passive_investment_vorabpauschale_tax_from_previous_year = function(year) {
              # Get the investment factor based on investment type
              # In a real scenario, we would need to track all investment types separately
              # For simplicity, we're assuming all investments are of the same type
              investment_factor <- switch(self$params$investment_type,
                                          "equity_fund" = 0.7,
                                          "mixed_fund" = 0.85,
                                          "domestic_real_estate_fund" = 0.4,
                                          "foreign_real_estate_fund" = 0.2,
                                          0.7) # Default to equity fund

              # Get the tax-free allowance
              tax_free_allowance <- self$results[self$results$Year == year, "passive_investment_vorabpauschale"]


              # Get base interest rate
              base_interest_rate <- self$params$base_interest_rate_vorabpauschale / 100

              # Subsequent years
              previous_year <- year - 1
              previous_total_invested <- self$results[self$results$Year == previous_year, "passive_investment_total_invested"]
              return_on_investment <- self$results[self$results$Year == year, "passive_investment_return_from_previous_year"]

              # Step 1: Calculate Vorabpauschale
              vorabpauschale <- previous_total_invested * 0.7 * base_interest_rate * investment_factor

              # Step 2: Compare with actual return
              taxable_amount <- min(vorabpauschale, return_on_investment)

              # Step 3: Apply tax-free allowance
              taxable_amount_after_allowance <- max(0, taxable_amount - tax_free_allowance)

              # Step 4: Calculate tax
              tax_rate <- self$results[self$results$Year == year, "capital_gains_tax_rate"] / 100
              vorabpauschale_tax <- -taxable_amount_after_allowance * tax_rate


              # Store the values
              self$results[self$results$Year == year, "vorabpauschale_tax_from_previous_year"] <- self$round_to_2(vorabpauschale_tax)

              # Track accumulated Vorabpauschale tax paid
              accumulated_tax <- self$results[self$results$Year == previous_year, "accumulated_vorabpauschale_tax"] + vorabpauschale_tax
              self$results[self$results$Year == year, "accumulated_vorabpauschale_tax"] <- self$round_to_2(accumulated_tax)

            },

            calculate_housing_cost_growth_inflation_corrected = function(year) {
              inflation <- self$results[self$results$Year == year, "inflation"]
              value <- self$correct_by_inflation(self$params$fixed_housing_costs_change, inflation) * 100
              self$results[self$results$Year == year, "housing_cost_growth_inflation_corrected"] <- self$round_to_2(value)
            },

            calculate_housing_cost = function(year) {
              if (year == self$params$initial_year) {
                value <- -self$params$fixed_housing_costs * 12
              } else {
                previous_year <- year - 1
                previous_cost <- self$results[self$results$Year == previous_year, "housing_cost"]
                growth_rate <- self$results[self$results$Year == previous_year, "housing_cost_growth_inflation_corrected"] / 100
                value <- previous_cost * (1 + growth_rate)
              }
              self$results[self$results$Year == year, "housing_cost"] <- self$round_to_2(value)
            }
          ),

          lock_objects = FALSE  # This allows dynamic addition of fields
  )

#### RentalCalculator Class ####
RentalCalculator <-
  R6Class("RentalCalculator",
          inherit = BaseCalculator,
          public = list(
            calculate_rental = function(year, params, results) {
              self$params <- params
              self$results <- results

              self$calculate_rental_cost_growth_inflation_corrected(year)
              self$calculate_rental_cost(year)
              self$calculate_rental_housing_cost(year)

              return(self$results)
            },

            calculate_rental_cost_growth_inflation_corrected = function(year) {
              inflation <- self$results[self$results$Year == year, "inflation"]
              value <- self$correct_by_inflation(self$params$rental_prices_growth, inflation) * 100
              self$results[self$results$Year == year, "rental_cost_growth_inflation_corrected"] <- self$round_to_2(value)
            },

            calculate_rental_cost = function(year) {
              if (year == self$params$initial_year) {
                value <- -self$params$rent_month * 12
              } else {
                growth_rate <- self$results[self$results$Year == year, "rental_cost_growth_inflation_corrected"] / 100
                value <- (-self$params$rent_month * 12) * (1 + growth_rate)^(year - self$params$initial_year)
              }
              self$results[self$results$Year == year, "rental_cost"] <- self$round_to_2(value)
            },

            calculate_rental_housing_cost = function(year) {
              self$results[self$results$Year == year, "rental_housing_cost"] <- self$results[self$results$Year == year, "housing_cost"]
            }

          ),

          lock_objects = FALSE  # This allows dynamic addition of fields
  )


#### PropertyCalculator Class ####
PropertyCalculator <-
  R6Class("PropertyCalculator",
          inherit = BaseCalculator,
          public = list(
            calculate_property = function(year, params, results, property_params, property_name) {
              self$params <- params
              self$results <- results
              self$property_params <- property_params
              self$property_name <- property_name
              self$property_type <- property_params$type
              self$prefix <- paste0(self$property_name, "_"
                                    #self$property_type, "_"
              )

              private$calculate_property_value_growth_inflation_corrected(year)
              private$calculate_property_market_evaluation(year)

              if (year >= self$property_params$purchase_year && year < self$property_params$sale_year) {

                if (year == self$property_params$purchase_year) {
                  private$calculate_property_purchase_real_cost(year)
                  private$calculate_property_available_for_down_payment(year)
                  private$calculate_property_used_from_savings(year)
                  private$calculate_property_mortgage_principal(year)
                } else {
                  self$results[self$results$Year == year, paste0(self$prefix, "purchase_real_cost")] <- 0
                  self$results[self$results$Year == year, paste0(self$prefix, "available_for_down_payment")] <- 0
                  self$results[self$results$Year == year, paste0(self$prefix, "used_from_savings")] <- 0
                  self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal")] <- 0
                }

                self$calculate_property_mortgage_details(year)
                self$results[self$results$Year == year, paste0(self$prefix, "additional_amortization")] <- 0
                self$calculate_property_loan_family_friends(year)
                self$calculate_property_costs(year)

                if (self$property_type == "investment") {
                  self$calculate_investment_property_specific(year)
                }

                self$calculate_property_sale(year)
              } else {
                self$reset_property_values(year)
              }

              # Zero out rental cost when home property is owned
              if (self$property_type == "home" &&
                  year >= self$property_params$purchase_year && year < self$property_params$sale_year) {
                self$results[self$results$Year == year, "rental_cost"] <- 0
                self$results[self$results$Year == year, "rental_housing_cost"] <- 0
              }

              return(self$results)
            },

            calculate_property_mortgage_details = function(year) {

              private$calculate_property_principal_repayment_rate(year)
              private$calculate_property_mortgage_interest_rate(year)
              private$calculate_property_mortgage_principal_share(year)
              self$results[self$results$Year == year,
                           paste0(self$prefix, "mortgage_interest_share")] <-
                private$calculate_property_mortgage_interest_share(year)

              self$results[self$results$Year == year,
                           paste0(self$prefix, "mortgage_payment")] <-
                private$calculate_property_mortgage_payment(year)
              private$calculate_property_mortgage_length_years(year)
              private$calculate_property_mortgage_principal_left(year)
              private$calculate_property_mortgage_interest_left(year)
              private$calculate_property_max_mortgage_amortization(year)
            },

            calculate_property_loan_family_friends = function(year) {



              private$calculate_property_loan_family_friends_interest_rate(year)
              private$calculate_property_loan_family_friends_principal_share(year)
              self$results[self$results$Year == year,
                           paste0(self$prefix, "loan_family_friends_interest_share")] <-
                private$calculate_property_loan_family_friends_interest_share(year)
              self$results[self$results$Year == year,
                           paste0(self$prefix, "loan_family_friends_payment")] <-
                private$calculate_property_loan_family_friends_payment(year)
              private$calculate_property_loan_family_friends_principal_left(year)
              private$calculate_property_loan_family_friends_interest_left(year)
            },

            calculate_property_costs = function(year) {



              private$calculate_property_taxes_rate(year)
              private$calculate_property_taxes(year)
              private$calculate_property_maintenance_cost_rate(year)
              private$calculate_property_maintenance_cost(year)

              private$calculate_property_hausgeld_maintenance_costs(year)
              private$calculate_property_hausgeld_utilities(year)
              private$calculate_property_hausgeld_reserve_fund(year)
              private$calculate_property_hausgeld_house_management_fee(year)
              private$calculate_property_hausgeld_building_insurance(year)
              private$calculate_property_hausgeld_fees_total(year)
            },

            calculate_investment_property_specific = function(year) {
              private$calculate_investment_property_lease_rental_growth(year)
              private$calculate_investment_property_cold_rent_from_lease(year)
              private$calculate_investment_property_warm_lease_income(year)
              private$calculate_investment_property_yearly_depreciation_rate(year)
              private$calculate_investment_property_yearly_depreciation(year)
              private$calculate_investment_property_management_fee(year)
              private$calculate_investment_property_total_deducible_expenses(year)
              private$calculate_investment_property_income_taxes_adjustment(year)
              private$calculate_investment_property_vacancy_months_cost(year)
            },

            calculate_property_sale = function(year) {
              private$calculate_property_sale_rate_total(year)
              private$calculate_property_sale_costs(year)
              private$calculate_property_sale_capital_gain_tax(year)
              private$calculate_property_value_liquid(year)
            },

            reset_property_values = function(year) {
              columns_to_reset <- paste0(self$prefix, c(
                "purchase_real_cost",
                "available_for_down_payment",
                "used_from_savings",
                "mortgage_principal",
                "principal_repayment_rate",
                "mortgage_interest_rate",

                "mortgage_principal_share",
                "mortgage_interest_share",
                "mortgage_payment",
                "mortgage_length_years",
                "mortgage_principal_left",
                "mortgage_interest_left",
                "max_mortgage_amortization",
                "additional_amortization",

                "loan_family_friends_interest_rate",
                "loan_family_friends_principal_share",
                "loan_family_friends_interest_share",
                "loan_family_friends_payment",
                "loan_family_friends_principal_left",
                "loan_family_friends_interest_left",

                "property_taxes_rate",
                "property_taxes",
                "maintenance_cost_rate",
                "maintenance_cost",

                "hausgeld_maintenance_costs",
                "hausgeld_utilities",
                "hausgeld_reserve_fund",
                "hausgeld_house_management_fee",
                "hausgeld_building_insurance",
                "hausgeld_fees_total"))

              if (self$property_type == "investment") {
                columns_to_reset <- c(columns_to_reset, paste0(self$prefix, c(
                  "lease_rental_growth",
                  "cold_rent_from_lease",
                  "warm_lease_income",
                  "yearly_depreciation_rate",
                  "yearly_depreciation",
                  "property_management_fee",
                  "total_deducible_expenses",
                  "income_taxes_adjustment",
                  "vacancy_months_cost")))
              }

              columns_to_reset <- c(columns_to_reset, paste0(self$prefix, c(
                "sale_rate_total",
                "sale_costs",
                "sale_capital_gain_tax",
                "value_liquid"
              )))

              self$results[self$results$Year == year, columns_to_reset] <- 0
            }

          ),

          private = list(
            #### Property calculations - mortgage details ####
            calculate_property_value_growth_inflation_corrected = function(year) {
              inflation <- self$results[self$results$Year == year, "inflation"]
              value <- self$correct_by_inflation(self$property_params$value_growth, inflation) * 100
              self$results[self$results$Year == year, paste0(self$prefix, "value_growth_inflation_corrected")] <- self$round_to_2(value)
            },

            calculate_property_market_evaluation = function(year) {
              if (year == self$params$initial_year) {
                value <- self$property_params$value_today
              } else {
                previous_year <- year - 1
                previous_evaluation <- self$results[self$results$Year == previous_year, paste0(self$prefix, "market_evaluation")]
                growth_rate <- self$results[self$results$Year == previous_year, paste0(self$prefix, "value_growth_inflation_corrected")] / 100
                value <- previous_evaluation * (1 + growth_rate)
              }
              self$results[self$results$Year == year, paste0(self$prefix, "market_evaluation")] <- self$round_to_2(value)
            },

            calculate_property_purchase_real_cost = function(year) {
              # Calculate total cost including purchase price and associated fees
              property_price <- self$results[self$results$Year == year, paste0(self$prefix, "market_evaluation")]
              total_cost <- property_price * (1 + self$params$estate_agent_fees/100 + self$params$notary_fees_purchase/100 + self$params$purchase_tax/100)


              self$results[self$results$Year == year, paste0(self$prefix, "purchase_real_cost")] <- self$round_to_2(total_cost)
            },

            calculate_property_available_for_down_payment = function(year) {
              if (year == self$params$initial_year) {
                available_funds <- self$params$savings - self$params$savings_emergency_reserve + self$property_params$loan_family_friends
              } else {
                previous_year <- year - 1
                # Fund from selling property that year

                sale_funds <- self$calculate_funds_from_property_sale(year, self$results, self$properties)

                # Deficit to replenish emergency reserve if needed
                emergency_reserve_deficit <- self$params$savings_emergency_reserve - self$results[self$results$Year == previous_year, "savings_emergency_reserve"]

                emergency_reserve_deficit <- ifelse(emergency_reserve_deficit > 0,
                                                    emergency_reserve_deficit,
                                                    0)

                # Total funds available
                available_funds <- sale_funds +
                  emergency_reserve_deficit +
                  self$property_params$loan_family_friends

              }

              self$results[self$results$Year == year, paste0(self$prefix, "available_for_down_payment")] <- self$round_to_2(available_funds)
            },

            calculate_property_used_from_savings = function(year) {

              total_cost <- self$results[self$results$Year == year, paste0(self$prefix, "purchase_real_cost")]
              available_funds <- self$results[self$results$Year == year, paste0(self$prefix, "available_for_down_payment")]
              total_cost_left <- total_cost - available_funds

              if (total_cost_left <= 0 || year == self$params$initial_year) {
                savings_used <- 0
              } else {
                # Check for funds in the savings account from the previous year
                previous_year <- year - 1
                available_savings <- self$results[self$results$Year == previous_year, "passive_investment_total_invested_liquid"]
                savings_used <- min(total_cost_left, available_savings)
              }

              self$results[self$results$Year == year, paste0(self$prefix, "used_from_savings")] <- self$round_to_2(savings_used)
            },

            calculate_property_mortgage_principal = function(year) {
              # Mortgage principal is the difference between total cost and available for down payment and available savings

              total_cost <- self$results[self$results$Year == year, paste0(self$prefix, "purchase_real_cost")]
              available_funds <- self$results[self$results$Year == year, paste0(self$prefix, "available_for_down_payment")]
              savings_used <- self$results[self$results$Year == year, paste0(self$prefix, "used_from_savings")]

              principal_needed <- total_cost - (available_funds + savings_used)
              self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal")] <- self$round_to_2(principal_needed)
            },

            calculate_property_principal_repayment_rate = function(year) {
              if (self$results[self$results$Year == self$property_params$purchase_year, paste0(self$prefix, "mortgage_principal")] == 0) {
                value <- 0
              } else if (year == self$property_params$purchase_year) {
                value <- self$property_params$principal_repayment_rate
              } else {
                previous_year <- year - 1
                if (self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_principal_left")] < 0) {
                  value <- self$property_params$principal_repayment_rate
                } else {
                  value <- 0
                }
              }
              self$results[self$results$Year == year, paste0(self$prefix, "principal_repayment_rate")] <- self$round_to_2(value)
            },

            calculate_property_mortgage_interest_rate = function(year) {
              if (self$results[self$results$Year == self$property_params$purchase_year, paste0(self$prefix, "mortgage_principal")] == 0) {
                value <- 0
              } else if (year == self$property_params$purchase_year) {
                value <- self$property_params$initial_interest_rate
              } else {
                previous_year <- year - 1

                if (self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_principal_left")] < 0) {
                  value <- self$property_params$initial_interest_rate
                } else {
                  value <- 0
                }
              }
              self$results[self$results$Year == year, paste0(self$prefix,  "mortgage_interest_rate")] <- self$round_to_2(value)
            },

            calculate_property_mortgage_principal_share = function(year) {
              if (self$results[self$results$Year == self$property_params$purchase_year,
                               paste0(self$prefix, "mortgage_principal")] == 0) {
                value <- 0
              } else if (year == self$property_params$purchase_year) {
                value <- -self$results[self$results$Year == self$property_params$purchase_year,
                                       paste0(self$prefix, "mortgage_principal")] * (self$property_params$principal_repayment_rate / 100)
              } else {
                previous_year <- year - 1
                mortgage_payment <- private$calculate_property_mortgage_payment(year)
                interest_share <- private$calculate_property_mortgage_interest_share(year)
                additional_amortization <- self$results[self$results$Year == previous_year,
                                                        paste0(self$prefix, "additional_amortization")]
                if (mortgage_payment <= 0) {
                  value <- mortgage_payment - interest_share - additional_amortization
                } else {
                  value <- 0
                }
              }
              self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal_share")] <- self$round_to_2(value)
            },

            calculate_property_mortgage_interest_share = function(year) {
              if (self$results[self$results$Year == self$property_params$purchase_year,
                               paste0(self$prefix, "mortgage_principal")] == 0) {
                value <- 0
              } else if (year == self$property_params$purchase_year) {
                interest_rate <- self$results[self$results$Year == year, paste0(self$prefix, "mortgage_interest_rate")] / 100
                value <- -self$results[self$results$Year == self$property_params$purchase_year,                                                       paste0(self$prefix, "mortgage_principal")] * interest_rate
              } else {
                previous_year <- year - 1
                previous_principal <- self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_principal_left")]
                interest_rate <- self$results[self$results$Year == year, paste0(self$prefix, "mortgage_interest_rate")] / 100
                value <- previous_principal * interest_rate
              }
              return(self$round_to_2(value))
            },

            calculate_property_mortgage_payment = function(year) {
              if (self$results[self$results$Year == self$property_params$purchase_year,
                               paste0(self$prefix, "mortgage_principal")] == 0) {
                value <- 0
              } else if (year == self$property_params$purchase_year) {
                # Initial mortgage payment is the sum of principal and interest payments
                principal_share <- self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal_share")]
                interest_share <- self$results[self$results$Year == year, paste0(self$prefix, "mortgage_interest_share")]
                value <- principal_share + interest_share
              } else {
                # For subsequent years, calculate based on remaining principal and interest rate
                previous_year <- year - 1
                previous_principal_left <- self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_principal_left")]
                previous_interest_rate <- self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_interest_rate")] / 100
                previous_payment <- self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_payment")]
                initial_payment <- self$results[self$results$Year == self$property_params$purchase_year, paste0(self$prefix, "mortgage_payment")]

                if (previous_principal_left < 0) {
                  if (previous_principal_left > initial_payment) {
                    # If remaining principal is greater than initial payment, recalculate payment
                    value <- previous_principal_left * (1 + previous_interest_rate)
                  } else {
                    # Otherwise, maintain the same payment as the first year
                    value <- initial_payment
                  }
                } else {
                  # Mortgage is fully paid off
                  value <- 0
                }
              }
              return(self$round_to_2(value))
            },

            calculate_property_mortgage_length_years = function(year) {
              if (self$results[self$results$Year == self$property_params$purchase_year,
                               paste0(self$prefix, "mortgage_principal")] == 0) {
                value <- 0
              } else if (year == self$property_params$purchase_year) {
                rate <- self$property_params$initial_interest_rate / 100
                pmt <- abs(self$results[self$results$Year == self$property_params$purchase_year, paste0(self$prefix, "mortgage_payment")])
                pv <- -self$results[self$results$Year == self$property_params$purchase_year,                                                       paste0(self$prefix, "mortgage_principal")]

                value <- log2((pmt/rate) / (pmt/rate + pv)) / log2(1+rate)
              } else {
                value <- 0
              }

              self$results[self$results$Year == year,                                                       paste0(self$prefix, "mortgage_length_years")] <- self$round_to_2(value)
            },

            calculate_property_mortgage_principal_left = function(year) {
              if (self$results[self$results$Year == self$property_params$purchase_year,
                               paste0(self$prefix, "mortgage_principal")] == 0) {
                value <- 0
              } else if (year == self$property_params$purchase_year) {
                value <- -self$results[self$results$Year == self$property_params$purchase_year,
                                       paste0(self$prefix, "mortgage_principal")] - self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal_share")]
              } else {

                previous_year <- year - 1
                previous_principal_left <- self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_principal_left")]
                previous_principal_payment <-  self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_principal_share")]
                value <- min(0, previous_principal_left - previous_principal_payment)
              }
              self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal_left")] <- self$round_to_2(value)
            },

            calculate_property_mortgage_interest_left = function(year) {
              if (self$results[self$results$Year == self$property_params$purchase_year,
                               paste0(self$prefix, "mortgage_principal")] == 0) {
                value <- 0
              } else if (year == self$property_params$purchase_year) {
                mortgage_payment <- self$results[self$results$Year == year, paste0(self$prefix, "mortgage_payment")]
                mortgage_length <- self$results[self$results$Year == self$property_params$purchase_year,                                                       paste0(self$prefix, "mortgage_length_years")]
                principal_left <- self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal_left")]
                value <-  (mortgage_payment * mortgage_length) - principal_left - mortgage_payment
              } else {

                previous_year <- year - 1
                previous_interest_left <- self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_interest_left")]
                interest_payment <-  self$results[self$results$Year == year, paste0(self$prefix, "mortgage_interest_share")]
                value <- ifelse(self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal_left")] >= 0,
                                0,
                                min(0, previous_interest_left - interest_payment))
              }
              self$results[self$results$Year == year, paste0(self$prefix, "mortgage_interest_left")] <- self$round_to_2(value)
            },

            calculate_property_max_mortgage_amortization = function(year) {

              if (year >= self$property_params$purchase_year &&
                  year < self$property_params$sale_year) {
                previous_year <- year - 1
                if (year > self$property_params$purchase_year &&
                    self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_principal_left")] >= 0) {
                  max_amortization <- 0

                } else {
                  initial_principal <- abs(self$results[self$results$Year == self$property_params$purchase_year, paste0(self$prefix, "mortgage_principal")])

                  max_amortization <- initial_principal * (self$property_params$max_mortgage_amortization_rate / 100)
                }

              } else {

              }

              self$results[self$results$Year == year,
                           paste0(self$prefix, "max_mortgage_amortization")] <- self$round_to_2(max_amortization)
            },




            #### Property calculations - Family/friends loan ####
            calculate_property_loan_family_friends_interest_rate = function(year) {
              if (year - self$property_params$purchase_year < self$property_params$duration_payback_family_friends) {
                value <- self$property_params$interest_rate_family_friends
              } else {
                value <- 0
              }
              self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_interest_rate")] <- self$round_to_2(value)
            },

            calculate_property_loan_family_friends_principal_share = function(year) {
              if (year - self$property_params$purchase_year < self$property_params$duration_payback_family_friends) {
                payment <- private$calculate_property_loan_family_friends_payment(year)
                interest_share <- private$calculate_property_loan_family_friends_interest_share(year)
                value <- payment - interest_share
              } else {
                value <- 0
              }
              self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_principal_share")] <- self$round_to_2(value)
            },

            calculate_property_loan_family_friends_interest_share = function(year) {
              if (year == self$property_params$purchase_year) {
                value <- -self$property_params$loan_family_friends * (self$property_params$interest_rate_family_friends / 100)
              } else if (year - self$property_params$purchase_year < self$property_params$duration_payback_family_friends) {
                previous_year <- year - 1
                principal_left <- self$results[self$results$Year == previous_year, paste0(self$prefix, "loan_family_friends_principal_left")]
                interest_rate <- self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_interest_rate")] / 100
                value <- principal_left * interest_rate
              } else {
                value <- 0
              }
              return(self$round_to_2(value))
            },

            calculate_property_loan_family_friends_payment = function(year) {
              if (year - self$property_params$purchase_year < self$property_params$duration_payback_family_friends) {
                principal <- self$property_params$loan_family_friends
                rate <- self$property_params$interest_rate_family_friends / 100 / 12  # Monthly rate
                nper <- self$property_params$duration_payback_family_friends * 12  # Total number of monthly payments
                value <- -principal * rate * (1 + rate)^nper / ((1 + rate)^nper - 1) * 12  # Annual payment
              } else {

                previous_year <- year - 1
                previous_principal_left <- self$results[self$results$Year == previous_year, paste0(self$prefix, "loan_family_friends_principal_left")]
                previous_interest_rate <- self$results[self$results$Year == previous_year, paste0(self$prefix, "loan_family_friends_interest_rate")] / 100
                previous_payment <- self$results[self$results$Year == previous_year, paste0(self$prefix, "loan_family_friends_payment")]
                initial_payment <- self$results[self$results$Year == self$property_params$purchase_year, paste0(self$prefix, "loan_family_friends_payment")]

                if (previous_principal_left < 0) {
                  if (previous_principal_left > initial_payment) {
                    value <- previous_principal_left * (1 + previous_interest_rate)
                  } else {
                    value <- initial_payment
                  }
                } else {
                  value <- 0
                }
              }
              return(self$round_to_2(value))
            },

            calculate_property_loan_family_friends_principal_left = function(year) {
              if (year == self$property_params$purchase_year) {
                value <- -self$property_params$loan_family_friends - self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_principal_share")]
              } else if (year - self$property_params$purchase_year < self$property_params$duration_payback_family_friends) {
                previous_year <- year - 1
                previous_principal_left <- self$results[self$results$Year == previous_year, paste0(self$prefix, "loan_family_friends_principal_left")]
                principal_payment <- self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_principal_share")]
                value <- previous_principal_left - principal_payment
              } else {
                value <- 0
              }
              self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_principal_left")] <- self$round_to_2(value)
            },

            calculate_property_loan_family_friends_interest_left = function(year) {
              if (year == self$property_params$purchase_year) {
                loan_payment <- self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_payment")]
                loan_length <- self$property_params$duration_payback_family_friends
                principal_left <- self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_principal_left")]
                value <-  (loan_payment * (loan_length-1)) - principal_left
              } else {
                previous_year <- year - 1
                previous_interest_left <- self$results[self$results$Year == previous_year,
                                                       paste0(self$prefix, "loan_family_friends_interest_left")]
                interest_payment <-  self$results[self$results$Year == year,
                                                  paste0(self$prefix, "loan_family_friends_interest_share")]
                value <- min(0, previous_interest_left - interest_payment)
              }

              self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_interest_left")] <- self$round_to_2(value)
            },





            #### Property calculations - income and costs ####
            calculate_property_taxes_rate = function(year) {
              value <- self$property_params$property_taxes_rate
              self$results[self$results$Year == year, paste0(self$prefix, "property_taxes_rate")] <- self$round_to_2(value)
            },

            calculate_property_taxes = function(year) {
              property_value <- self$results[self$results$Year == year, paste0(self$prefix, "market_evaluation")]
              tax_rate <- self$results[self$results$Year == year, paste0(self$prefix, "property_taxes_rate")] / 100
              value <- property_value * tax_rate
              self$results[self$results$Year == year, paste0(self$prefix, "property_taxes")] <- self$round_to_2(-value)
            },

            calculate_property_maintenance_cost_rate = function(year) {
              value <- self$property_params$maintenance_cost_rate
              self$results[self$results$Year == year, paste0(self$prefix, "maintenance_cost_rate")] <- self$round_to_2(value)
            },

            calculate_property_maintenance_cost = function(year) {

              property_value <- self$results[self$results$Year == year, paste0(self$prefix, "market_evaluation")]
              maintenance_rate <- self$results[self$results$Year == year, paste0(self$prefix, "maintenance_cost_rate")] / 100
              value <- property_value * maintenance_rate
              self$results[self$results$Year == year, paste0(self$prefix, "maintenance_cost")] <- self$round_to_2(-value)
            },

            calculate_property_hausgeld_maintenance_costs = function(year) {

              value <- -self$property_params$hausgeld_maintenance_costs * 12
              self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_maintenance_costs")] <- self$round_to_2(value)
            },

            calculate_property_hausgeld_utilities = function(year) {

              value <- -self$property_params$hausgeld_utilities * 12
              self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_utilities")] <- self$round_to_2(value)
            },

            calculate_property_hausgeld_reserve_fund = function(year) {

              value <- -self$property_params$hausgeld_reserve_fund * 12
              self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_reserve_fund")] <- self$round_to_2(value)
            },

            calculate_property_hausgeld_house_management_fee = function(year) {
              value <- -self$property_params$hausgeld_house_management_fee * 12
              self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_house_management_fee")] <- self$round_to_2(value)
            },

            calculate_property_hausgeld_building_insurance = function(year) {

              value <- -self$property_params$hausgeld_building_insurance * 12
              self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_building_insurance")] <- self$round_to_2(value)
            },

            calculate_property_hausgeld_fees_total = function(year) {

              sum_hausgeld <- sum(
                self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_maintenance_costs")],
                self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_utilities")],
                self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_reserve_fund")],
                self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_house_management_fee")],
                self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_building_insurance")]
              )
              value <- min(self$property_params$hausgeld_fees_total, sum_hausgeld)
              self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_fees_total")] <- self$round_to_2(value)
            },




            #### Property calculations - Investment property specific ####
            calculate_investment_property_lease_rental_growth = function(year) {

              inflation <- self$results[self$results$Year == year, "inflation"]
              value <- self$correct_by_inflation(self$property_params$lease_rental_growth, inflation)
              self$results[self$results$Year == year, paste0(self$prefix, "lease_rental_growth")] <- self$round_to_2(value * 100)  # Convert to percentage
            },

            calculate_investment_property_cold_rent_from_lease = function(year) {
              if (year == self$property_params$purchase_year) {
                if (year == self$params$initial_year) {
                  value <- self$property_params$cold_lease_today * 12
                } else {
                  growth_rate <- self$results[self$results$Year == year, paste0(self$prefix, "lease_rental_growth")] / 100
                  value <- (self$property_params$cold_lease_today * 12) * (1 + growth_rate)^(year - self$params$initial_year)
                }
              } else {
                previous_year <- year - 1
                previous_value <- self$results[self$results$Year == previous_year, paste0(self$prefix, "cold_rent_from_lease")]
                growth_rate <- self$results[self$results$Year == year, paste0(self$prefix, "lease_rental_growth")] / 100
                value <- previous_value * (1 + growth_rate)
              }
              self$results[self$results$Year == year, paste0(self$prefix, "cold_rent_from_lease")] <- self$round_to_2(value)
            },

            calculate_investment_property_warm_lease_income = function(year) {

              value <- sum(
                self$results[self$results$Year == year, paste0(self$prefix, "cold_rent_from_lease")],
                -self$results[self$results$Year == year, paste0(self$prefix, "property_taxes")],
                -self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_fees_total")]
              )
              self$results[self$results$Year == year, paste0(self$prefix, "warm_lease_income")] <- self$round_to_2(value)
            },

            calculate_investment_property_yearly_depreciation_rate = function(year) {
              value <- self$property_params$yearly_depreciation_rate
              self$results[self$results$Year == year, paste0(self$prefix, "yearly_depreciation_rate")] <- self$round_to_2(value)
            },

            calculate_investment_property_yearly_depreciation = function(year) {

              purchase_year_value <- self$results[self$results$Year == self$property_params$purchase_year, paste0(self$prefix, "market_evaluation")]
              depreciation_rate <- self$results[self$results$Year == year, paste0(self$prefix, "yearly_depreciation_rate")] / 100
              value <- purchase_year_value * depreciation_rate
              self$results[self$results$Year == year, paste0(self$prefix, "yearly_depreciation")] <- self$round_to_2(-value)
            },

            calculate_investment_property_management_fee = function(year) {
              cold_rent <- self$results[self$results$Year == year, paste0(self$prefix, "cold_rent_from_lease")]
              value <- cold_rent * (self$property_params$property_management_rate / 100)
              self$results[self$results$Year == year, paste0(self$prefix, "property_management_fee")] <- self$round_to_2(-value)
            },

            calculate_investment_property_total_deducible_expenses = function(year) {
              value <- sum(
                self$results[self$results$Year == year, paste0(self$prefix, "property_taxes")],
                self$results[self$results$Year == year, paste0(self$prefix, "maintenance_cost")],
                self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_fees_total")],
                self$results[self$results$Year == year, paste0(self$prefix, "yearly_depreciation")],
                self$results[self$results$Year == year, paste0(self$prefix, "property_management_fee")]
              )
              self$results[self$results$Year == year, paste0(self$prefix, "total_deducible_expenses")] <- self$round_to_2(value)
            },

            calculate_investment_property_income_taxes_adjustment = function(year) {
              cold_rent <- self$results[self$results$Year == year, paste0(self$prefix, "cold_rent_from_lease")]
              investment_property_total_deducible_expenses <- self$results[self$results$Year == year, paste0(self$prefix, "total_deducible_expenses")]
              mortgage_interest <- self$results[self$results$Year == year, paste0(self$prefix, "mortgage_interest_share")]


              tax_rate <- (self$results[self$results$Year == year,
                                        "total_taxes_on_income"] / 100)
              value <-
                # Income tax deduction from depreciation and costs
                -(cold_rent + investment_property_total_deducible_expenses) * tax_rate +
                # Mortgage interest tax deduction
                (-mortgage_interest * tax_rate)

              self$results[self$results$Year == year, paste0(self$prefix, "income_taxes_adjustment")] <- self$round_to_2(value)
            },

            calculate_investment_property_vacancy_months_cost = function(year) {

              investment_property_warm_lease_income <- self$results[self$results$Year == year, paste0(self$prefix, "warm_lease_income")]
              value <- -(investment_property_warm_lease_income / 12) * self$property_params$vacancy_months
              self$results[self$results$Year == year, paste0(self$prefix, "vacancy_months_cost")] <- self$round_to_2(value)
            },



            #### Property calculations - Sale ####
            calculate_property_sale_rate_total = function(year) {
              value <- sum(
                self$params$real_estate_agent_fees_sale,
                self$params$notary_fees_sale,
                self$params$property_transfer_taxes_sale,
                self$params$miscelanous_fees_costs_sale
              )
              self$results[self$results$Year == year, paste0(self$prefix, "sale_rate_total")] <- self$round_to_2(value)
            },

            calculate_property_sale_costs = function(year) {
              property_value <- self$results[self$results$Year == year, paste0(self$prefix, "market_evaluation")]
              sale_rate <- self$results[self$results$Year == year, paste0(self$prefix, "sale_rate_total")] / 100
              value <- property_value * sale_rate
              self$results[self$results$Year == year, paste0(self$prefix, "sale_costs")] <- self$round_to_2(-value)
            },

            calculate_property_sale_capital_gain_tax = function(year) {

              if (year - self$property_params$purchase_year > 10) {
                # No capital gains tax after 10 years of ownership in Germany
                value <- 0
              } else {
                # Calculate total tax rate including solidarity surcharge and church tax
                value <- self$params$capital_gains_tax_rate +
                  (self$params$solidarity_surcharge_tax * self$params$capital_gains_tax_rate / 100) +
                  (self$params$church_tax * self$params$capital_gains_tax_rate / 100)
              }
              self$results[self$results$Year == year, paste0(self$prefix, "sale_capital_gain_tax")] <- self$round_to_2(value)
            },

            calculate_property_value_liquid = function(year) {

              property_value <- self$results[self$results$Year == year, paste0(self$prefix, "market_evaluation")]
              property_value_purchase_year <- self$results[self$results$Year == self$property_params$purchase_year, paste0(self$prefix, "market_evaluation")]
              sale_costs <- self$results[self$results$Year == year, paste0(self$prefix, "sale_costs")]
              capital_gain_tax_rate <- self$results[self$results$Year == year, paste0(self$prefix, "sale_capital_gain_tax")] / 100

              # Calculate liquid value considering sale costs and capital gains tax
              # Capital gains tax is only applied on the profit (if any)
              value <- property_value + sale_costs + min(0, (property_value_purchase_year-property_value) * capital_gain_tax_rate)


              # Pay debt on loan before liquidating
              value <- value +
                self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal_left")] +
                self$results[self$results$Year == year, paste0(self$prefix, "mortgage_interest_left")] +
                self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_principal_left")] +
                self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_interest_left")]



              self$results[self$results$Year == year, paste0(self$prefix, "value_liquid")] <- self$round_to_2(value)
            }
          ),

          lock_objects = FALSE  # This allows dynamic addition of fields
  )



#### FinancialBalanceCalculator Class ####
FinancialBalanceCalculator <-
  R6Class("FinancialBalanceCalculator",
          inherit = BaseCalculator,
          public = list(
            calculate_financial_balance = function(year, params, results, properties) {
              #' Main financial balance calculation entry point
              #' @param year Calculation year
              #' @param params Input parameters
              #' @param results Results data frame
              #' @param properties Property configurations
              #'
              self$params <- params
              self$results <- results
              self$properties <- properties

              private$calculate_total_expenses(year)
              private$calculate_total_income(year)
              private$calculate_cash_flow(year)
              private$calculate_cash_flow_distribution(year)
              private$calculate_savings_emergency_reserve(year)

              return(self$results)
            }
          ),

          private = list(

            calculate_total_expenses = function(year) {
              expenses <- sum(
                self$results[self$results$Year == year, c(
                  "living_costs",

                  "vorabpauschale_tax_from_previous_year",

                  "rental_cost",
                  "rental_housing_cost",

                  "properties_principal_share",
                  "properties_interest_share",
                  "properties_property_taxes",
                  "properties_maintenance_cost",
                  "properties_hausgeld_fees_total",
                  "properties_property_management_fee",
                  "properties_vacancy_months_cost"
                )],
                na.rm = TRUE)
              self$results[self$results$Year == year, "total_expenses"] <- expenses
            },

            calculate_total_income = function(year) {
              # Salary
              salary <- self$results[self$results$Year == year, "net_annual_salary"]

              # Passive Investment Income - Later REMOVED from cash flow calculation since it will be automatically reinvested
              returns <- self$results[self$results$Year == year, "passive_investment_return_from_previous_year"]

              # Lump sums
              lump_sums <- self$results[self$results$Year == year, "lump_sums"]

              # Property lease income
              rental_income <- sum(self$results[self$results$Year == year,
                                                grep("warm_lease_income$",
                                                     names(self$results), value = TRUE)],
                                   na.rm = TRUE)

              # Income taxes deduction
              deductions <- sum(self$results[self$results$Year == year,
                                             grep("income_taxes_adjustment$",
                                                  names(self$results), value = TRUE)],
                                na.rm = TRUE)


              income <- salary + lump_sums + rental_income + deductions + returns

              self$results[self$results$Year == year, "total_income"] <- self$round_to_2(income)
            },

            calculate_cash_flow = function(year) {
              # Income from property sale not used for down payment
              # Payment of loan debt is already accounted for in the liquid property value
              sale_funds <- self$calculate_funds_from_property_sale(year, self$results, self$properties)

              available_for_down_payment <- sum(self$results[self$results$Year == year, grep("available_for_down_payment", names(self$results), value = TRUE)])

              if (sale_funds > available_for_down_payment) {
                sale_funds <- sale_funds - available_for_down_payment
              } else {
                sale_funds <- 0
              }


              cash_flow <- self$results[self$results$Year == year, "total_income"] +
                self$results[self$results$Year == year, "total_expenses"] + sale_funds

              # Passive Investment Income is REMOVED from cash flow calculation since it will be automatically reinvested
              cash_flow <- cash_flow - self$results[self$results$Year == year, "passive_investment_return_from_previous_year"]

              self$results[self$results$Year == year, "cash_flow"] <- self$round_to_2(cash_flow)
            },

            distribute_mortgage_amortization = function(year, total_amortization) {
              # Get all properties with remaining mortgage
              property_columns <- grep("mortgage_principal_left$", names(self$results), value = TRUE)


              properties_with_mortgage <- data.frame(
                property_name = gsub("_mortgage_principal_left$", "", property_columns),
                remaining_principal = -as.numeric(self$results[self$results$Year == year, property_columns]),
                max_amortization = as.numeric(self$results[self$results$Year == year,
                                                           gsub("mortgage_principal_left$",
                                                                "max_mortgage_amortization",
                                                                property_columns)])
              )

              # Filter properties with actual remaining principal
              properties_with_mortgage <- properties_with_mortgage[properties_with_mortgage$remaining_principal > 0, ]

              if (nrow(properties_with_mortgage) == 0) {
                return(0)
              }

              # Calculate equal distribution
              equal_share <- total_amortization / nrow(properties_with_mortgage)

              # Adjust for maximum amortization limits
              for (i in 1:nrow(properties_with_mortgage)) {
                property_name <- properties_with_mortgage$property_name[i]
                max_amort <- properties_with_mortgage$max_amortization[i]

                # Limit amortization to the maximum allowed
                actual_amortization <- min(equal_share, max_amort)

                # Store the additional amortization for this property
                self$results[self$results$Year == year,
                             paste0(property_name, "_additional_amortization")] <- self$round_to_2(actual_amortization)
              }

              # Return total actual amortization
              return(sum(sapply(properties_with_mortgage$property_name, function(name) {
                self$results[self$results$Year == year, paste0(name, "_additional_amortization")]
              })))
            },

            calculate_cash_flow_distribution = function(year) {
              cash_flow <- self$results[self$results$Year == year, "cash_flow"]

              if (cash_flow <= 0) {
                self$results[self$results$Year == year, "cash_flow_to_emergency_reserve"] <- cash_flow
                self$results[self$results$Year == year, "cash_flow_to_mortgage_amortization"] <- 0
                self$results[self$results$Year == year, "cash_flow_to_investment"] <- 0
                return()
              }

              # First priority: Replenish emergency reserve
              if (year == self$params$initial_year) {
                emergency_reserve_deficit <- self$params$savings_emergency_reserve -
                  self$results[self$results$Year == year, "savings_emergency_reserve"]
              } else {
                previous_year <- year - 1
                emergency_reserve_deficit <- self$params$savings_emergency_reserve -
                  self$results[self$results$Year == previous_year, "savings_emergency_reserve"]
              }

              emergency_reserve_allocation <- min(cash_flow, max(0, emergency_reserve_deficit))
              remaining_cash_flow <- cash_flow - emergency_reserve_allocation

              # Second priority: Mortgage amortization up to maximum allowed
              actual_mortgage_allocation <- private$distribute_mortgage_amortization(year, remaining_cash_flow)
              remaining_cash_flow <- remaining_cash_flow - actual_mortgage_allocation

              # Third priority: Passive investment
              investment_allocation <- remaining_cash_flow

              # Store the allocations
              self$results[self$results$Year == year, "cash_flow_to_emergency_reserve"] <-
                self$round_to_2(emergency_reserve_allocation)
              self$results[self$results$Year == year, "cash_flow_to_mortgage_amortization"] <-
                self$round_to_2(actual_mortgage_allocation)
              self$results[self$results$Year == year, "cash_flow_to_investment"] <-
                self$round_to_2(investment_allocation)
            },

            calculate_savings_emergency_reserve = function(year) {
              if (year == self$params$initial_year) {
                cash_flow <- self$results[self$results$Year == year, "cash_flow"]
                if (cash_flow < 0) {
                  savings_emergency_reserve <- self$params$savings_emergency_reserve + cash_flow
                } else {
                  savings_emergency_reserve <- self$params$savings_emergency_reserve
                }
              } else {
                previous_year <- year - 1
                cash_flow <- self$results[self$results$Year == year, "cash_flow"]
                previous_savings_emergency_reserve <- self$results[self$results$Year == previous_year, "savings_emergency_reserve"]
                total_money_invested <- self$results[self$results$Year == previous_year, "passive_investment_total_invested_liquid"]
                capital_gains_tax <- self$results[self$results$Year == year, "capital_gains_tax_rate"] / 100

                savings_emergency_reserve <-
                  ifelse(cash_flow < 0,
                         ifelse(previous_savings_emergency_reserve + cash_flow > 0,
                                previous_savings_emergency_reserve + cash_flow,
                                ifelse(total_money_invested - abs(previous_savings_emergency_reserve + cash_flow) * (1 + capital_gains_tax) >= previous_savings_emergency_reserve + cash_flow,
                                       0,
                                       previous_savings_emergency_reserve + cash_flow
                                )
                         ),
                         min(previous_savings_emergency_reserve + cash_flow, self$params$savings_emergency_reserve)
                  ) + ifelse(previous_savings_emergency_reserve < 0, previous_savings_emergency_reserve * self$params$interest_rate_cash_flow_debt / 100, 0)
              }
              self$results[self$results$Year == year, "savings_emergency_reserve"] <- self$round_to_2(savings_emergency_reserve)
            }

          ),

          lock_objects = FALSE  # This allows dynamic addition of fields
  )



#### PassiveInvestingCalculator Class ####
PassiveInvestingCalculator <-
  R6Class("PassiveInvestingCalculator",
          inherit = BaseCalculator,
          public = list(
            calculate_passive_investment = function(year, params, results, properties) {
              #' Calculate passive investment components after financial balance
              #' @param year Calculation year
              #' @param params Input parameters
              #' @param results Results data frame
              #' @param properties Property configurations

              self$params <- params
              self$results <- results
              self$properties <- properties

              # Core passive investment calculations
              private$calculate_passive_investment_money_needed_from_liquid(year)
              private$calculate_passive_investment_money_withdrawn_from_capital_gains(year)
              private$calculate_passive_investment_money_withdrawn_from_contributions(year)
              private$calculate_passive_investment_yearly_contribution(year)
              private$calculate_passive_investment_contributions_accumulated(year)
              private$calculate_passive_investment_total_invested(year)
              private$calculate_passive_investment_total_invested_liquid(year)

              return(self$results)
            }
          ),

          private = list(

            calculate_passive_investment_money_needed_from_liquid = function(year) {
              if (year == self$params$initial_year) {
                money_taken_out <- 0
              } else {
                # Check if a property was bought this year
                needed_from_buying_property <- 0
                for (property_name in names(self$properties)) {
                  property <- self$properties[[property_name]]
                  if (year == property[["purchase_year"]]) {
                    needed_from_buying_property <- needed_from_buying_property +
                      sum(self$results[self$results$Year == year,
                                       grep("used_from_savings",
                                            names(self$results), value = TRUE)])
                  }
                }


                # Check if emergency reserve is depleted and cash flow is negative
                emergency_reserve <- self$results[self$results$Year == year,
                                                  "savings_emergency_reserve"]
                cash_flow <- self$results[self$results$Year == year, "cash_flow"]

                money_needed_cashflow <- abs(ifelse(emergency_reserve <= 0 && cash_flow < 0,
                                                    cash_flow,
                                                    0))

                # total money needed
                money_taken_out <- needed_from_buying_property +
                  money_needed_cashflow
              }

              self$results[self$results$Year == year,
                           "passive_investment_money_needed_from_liquid"] <-
                -self$round_to_2(money_taken_out)
            },

            calculate_passive_investment_money_withdrawn_from_capital_gains = function(year) {
              previous_year <- year - 1
              previous_total_invested <- self$results[self$results$Year == previous_year, "passive_investment_total_invested"]
              previous_contributions_accumulated <- self$results[self$results$Year == previous_year, "passive_investment_contributions_accumulated"]
              previous_capital_gains_tax_rate <- self$results[self$results$Year == previous_year, "capital_gains_tax_rate"] / 100
              money_needed <- self$results[self$results$Year == year,
                                           "passive_investment_money_needed_from_liquid"]

              # Calculate the tax rate
              tax_rate <- self$results[self$results$Year == previous_year, "capital_gains_tax_rate"] / 100

              if (money_needed != 0) browser()

              # Calculate capital gains from previous year
              capital_gains <- previous_total_invested - previous_contributions_accumulated

              # Apply German-specific tax rules if enabled
              if (self$params$apply_vorabpauschale) {

                # Only 70% of capital gains are taxable in advance in Germany
                taxable_percentage <- 0.7

                # Get the accumulated Vorabpauschale tax already paid
                accumulated_vorabpauschale_tax <- self$results[self$results$Year == previous_year, "accumulated_vorabpauschale_tax"]

                # Adjust the money needed based on the modified tax calculation
                if (money_needed < 0 && capital_gains > 0) {
                  # Calculate what portion of the withdrawal is from capital gains
                  # This assumes withdraw from invested money is proportional to how much is capital gains and direct contributions
                  # Only a fraction of withdraw from capital gains is taxed (taxable_percentage)
                  # Withdraw from contributions is not taxed
                  capital_gains_ratio <- abs(capital_gains) / previous_total_invested
                  capital_gains_withdrawn <- money_needed * capital_gains_ratio
                  contributions_withdrawn <- money_needed * (1 - capital_gains_ratio)

                  # Calculate tax on withdrawn capital gains with German rules
                  tax_on_capital_gains <- capital_gains_withdrawn * taxable_percentage * tax_rate

                  # Deduct already paid Vorabpauschale tax (proportionally)
                  final_tax <- min(0, tax_on_capital_gains - accumulated_vorabpauschale_tax)

                  # Adjust the money needed to account for the modified tax calculation
                  money_needed_brutto <- contributions_withdrawn + capital_gains_withdrawn - final_tax
                } else {
                  money_needed_brutto <- money_needed
                }
              } else {

                # Standard tax calculation if Vorabpauschale is not enabled 100
                # Money needed brutto will be the netto amount plus the tax on the gains
                money_needed_brutto <- money_needed * (1 + tax_rate)
              }

              # Adjust the money taken out based on accumulated contributions and capital gains tax
              if (money_needed_brutto < 0) {
                available_taxable <- previous_total_invested - previous_contributions_accumulated

                if (abs(money_needed_brutto) <= available_taxable) {
                  money_taken_out <- money_needed_brutto
                } else {
                  money_taken_out <- -available_taxable
                }
              } else {
                money_taken_out <- 0
              }

              # Update accumulated Vorabpauschale tax if money is withdrawn
              if (self$params$apply_vorabpauschale && money_taken_out < 0) {
                # Calculate what portion of the investment is being withdrawn
                withdrawal_ratio <- min(1, abs(money_taken_out) / previous_total_invested)

                # Reduce the accumulated Vorabpauschale tax proportionally
                previous_accumulated_tax <- self$results[self$results$Year == previous_year, "accumulated_vorabpauschale_tax"]
                new_accumulated_tax <- previous_accumulated_tax - tax_on_capital_gains
                self$results[self$results$Year == year, "accumulated_vorabpauschale_tax"] <- self$round_to_2(new_accumulated_tax)
              }

              self$results[self$results$Year == year, "passive_investment_money_withdrawn_from_capital_gains"] <- self$round_to_2(money_taken_out)
            },

            calculate_passive_investment_money_withdrawn_from_contributions = function(year) {

              previous_year <- year - 1
              previous_total_invested <- self$results[self$results$Year == previous_year, "passive_investment_total_invested"]
              previous_contributions_accumulated <- self$results[self$results$Year == previous_year, "passive_investment_contributions_accumulated"]
              previous_capital_gains_tax_rate <- self$results[self$results$Year == year, "capital_gains_tax_rate"] / 100
              money_needed <- self$results[self$results$Year == year,
                                           "passive_investment_money_needed_from_liquid"]

              # Correct money needed from liquid to brutto
              money_needed_brutto <- money_needed / (1 - previous_capital_gains_tax_rate)

              # Adjust the money taken out based on accumulated contributions and capital gains tax
              if (money_needed_brutto < 0) {
                available_taxable <- previous_total_invested -
                  previous_contributions_accumulated

                if (abs(money_needed_brutto) <= available_taxable) {
                  money_taken_out <- 0
                } else {
                  taxable_amount <- available_taxable
                  money_taken_out <- -(abs(money_needed_brutto) - taxable_amount) * (1 - previous_capital_gains_tax_rate)
                }
              } else {
                money_taken_out <- 0
              }

              self$results[self$results$Year == year, "passive_investment_money_withdrawn_from_contributions"] <- self$round_to_2(money_taken_out)
            },

            calculate_passive_investment_yearly_contribution = function(year) {
              loan_taken <- self$check_if_loan_taken(year, self$results)
              if (loan_taken > 0) {
                contribution <- 0
              } else {
                contribution <- self$results[self$results$Year == year, "cash_flow_to_investment"]
              }

              self$results[self$results$Year == year, "passive_investment_yearly_contribution"] <- self$round_to_2(contribution)

            },

            calculate_passive_investment_contributions_accumulated = function(year) {
              if (self$check_if_loan_taken(year, self$results) > 0) {
                accumulated <- 0
              } else {
                yearly_contribution <- self$results[self$results$Year == year, "passive_investment_yearly_contribution"]
                taken_from_contributions <- self$results[self$results$Year == year, "passive_investment_money_withdrawn_from_contributions"]

                # previous total invested
                if (year == self$params$initial_year) {
                  previous_total_accumulated <- max(0, self$params$savings -
                                                      self$params$savings_emergency_reserve)
                } else {
                  previous_year <- year - 1
                  previous_total_accumulated <- self$results[self$results$Year == previous_year, "passive_investment_contributions_accumulated"]
                }

                # Subtract the amount taken for loan
                accumulated <- max(0,previous_total_accumulated + yearly_contribution + taken_from_contributions)
              }



              self$results[self$results$Year == year, "passive_investment_contributions_accumulated"] <- self$round_to_2(accumulated)
            },

            calculate_passive_investment_total_invested = function(year) {
              # Zero out investment account if a loan was taken
              if (self$check_if_loan_taken(year, self$results) > 0) {
                total_invested <- 0
              } else {
                yearly_contribution <- self$results[self$results$Year == year, "passive_investment_yearly_contribution"]
                taken_from_investment <- self$results[self$results$Year == year,
                                                      "passive_investment_money_withdrawn_from_capital_gains"]
                taken_from_contributions <- self$results[self$results$Year == year, "passive_investment_money_withdrawn_from_contributions"]

                # Add returns to be reinvested
                return_previous_year <- self$results[self$results$Year == year, "passive_investment_return_from_previous_year"]

                # previous total invested
                if (year == self$params$initial_year) {
                  previous_total_invested <- self$results[self$results$Year == year,
                                                          "passive_investment_contributions_accumulated"]
                } else {
                  previous_year <- year - 1
                  previous_total_invested <- self$results[self$results$Year == previous_year, "passive_investment_total_invested"]
                }

                # Subtract the amount taken for loan
                total_invested <- max(0, previous_total_invested + yearly_contribution + taken_from_investment + taken_from_contributions + return_previous_year)


              }
              self$results[self$results$Year == year, "passive_investment_total_invested"] <- self$round_to_2(total_invested)
            },

            calculate_passive_investment_total_invested_liquid = function(year) {
              total_invested <- self$results[self$results$Year == year, "passive_investment_total_invested"]
              contributions_accumulated <- self$results[self$results$Year == year, "passive_investment_contributions_accumulated"]
              tax_rate <- self$results[self$results$Year == year, "capital_gains_tax_rate"] / 100

              if (total_invested - contributions_accumulated > 0) {
                liquid_value <- total_invested - tax_rate * (total_invested - contributions_accumulated)
              } else {
                liquid_value <- total_invested
              }

              self$results[self$results$Year == year, "passive_investment_total_invested_liquid"] <- self$round_to_2(liquid_value)
            }
          ),

          lock_objects = FALSE  # This allows dynamic addition of fields
  )


#### DataProcessor Class ####
DataProcessor <-
  R6Class("DataProcessor",
          inherit = BaseCalculator,
          public = list(
            params = NULL,
            properties = NULL,
            results = NULL,
            general_calculator = NULL,
            rental_calculator = NULL,
            property_calculator = NULL,
            financial_balance_calculator = NULL,
            passive_investing_calculator = NULL,
            initialize = function(scenario_name = "example",
                                  config = yaml::read_yaml("config/inputs.yaml"),
                                  initial_year = 2025,
                                  final_year = 2075) {
              self$params <- list(
                scenario_name = scenario_name,
                initial_year = initial_year,
                final_year = final_year
              )

              # Initialize property parameters
              private$init_parameters(config)
              private$init_properties(config$properties)

              # Initiate the calculators
              self$general_calculator <- GeneralCalculator$new()
              self$rental_calculator <- RentalCalculator$new()
              self$property_calculator <- PropertyCalculator$new()
              self$financial_balance_calculator <- FinancialBalanceCalculator$new()
              self$passive_investing_calculator <- PassiveInvestingCalculator$new()

              # Initialize the results dataframe
              self$results <- data.frame(Year = self$params$initial_year:self$params$final_year)
            },

            calculate = function() {
              for (year in self$params$initial_year:self$params$final_year) {
                self$calculate_year(year)
              }
            },

            calculate_year = function(year) {
              #' Orchestrate yearly calculations with proper dependency order
              #' @param year Calculation year

              # Phase 1: Core parameters and rental costs
              self$results <-
                self$general_calculator$calculate_general(year, self$params, self$results)
              self$results <-
                self$rental_calculator$calculate_rental(year, self$params, self$results)

              # Phase 2: Property calculations
              for (property_name in names(self$properties)) {
                self$results <-
                  self$property_calculator$calculate_property(year,
                                                              self$params,
                                                              self$results,
                                                              self$properties[[property_name]],
                                                              property_name)
              }
              private$calculate_property_expense_summaries(year)

              # Phase 3: Financial balance on that year
              self$results <-
                self$financial_balance_calculator$calculate_financial_balance(year,
                                                                              self$params,
                                                                              self$results,
                                                                              self$properties)

              # Phase 4: Passive investments contributions calculations
              self$results <-
                self$passive_investing_calculator$calculate_passive_investment(year,
                                                                          self$params,
                                                                          self$results,
                                                                          self$properties)

              # Final asset calculation
              private$calculate_total_asset(year)
            },

            get_results = function() {
              self$results$scenario <- self$params$scenario_name
              return(self$results)
            }
          ),


          private = list(

            init_parameters = function(config) {
              for (group in names(config)) {
                if (group != "properties") {  # Handle properties separately
                  for (input in config[[group]]$inputs) {
                    self$params[[input$id]] <- input$value
                  }
                }
              }
            },

            init_properties = function(properties_config) {
              self$properties <- list()
              for (property in properties_config) {
                property_params <- list(
                  name = property$name,
                  type = property$type
                )
                for (input in property$inputs) {
                  property_params[[input$id]] <- input$value
                }
                self$properties[[property$name]] <- property_params
              }
            },

            calculate_total_asset = function(year) {

              total_market_evaluation <- 0
              for (property_name in names(self$properties)) {
                property_params <- self$properties[[property_name]]
                if (year >= property_params[["purchase_year"]] &&
                    year < property_params[["sale_year"]]) {
                  total_market_evaluation <- total_market_evaluation +
                    self$results[self$results$Year == year,
                                 paste0(property_name, "_market_evaluation")]
                }
              }

              total_mortgage_principal_left <- sum(self$results[self$results$Year == year, grep("mortgage_principal_left", names(self$results), value = TRUE)])

              total_mortgage_interest_left <- sum(self$results[self$results$Year == year, grep("mortgage_interest_left", names(self$results), value = TRUE)])

              total_loan_family_friends_principal_left <- sum(self$results[self$results$Year == year, grep("loan_family_friends_principal_left", names(self$results), value = TRUE)])

              total_loan_family_friends_interest_left <- sum(self$results[self$results$Year == year, grep("loan_family_friends_interest_left", names(self$results), value = TRUE)])

              # Financial balance
              savings_emergency_reserve <-  self$results[self$results$Year == year, "savings_emergency_reserve"]

              # Passive investments
              passive_investment_total_invested <-  self$results[self$results$Year == year, "passive_investment_total_invested"]


              # Total asset
              self$results[self$results$Year == year,
                           "total_asset"] <-
                self$round_to_2(total_market_evaluation +
                                  total_mortgage_principal_left +
                                  total_mortgage_interest_left +
                                  total_loan_family_friends_principal_left +
                                  total_loan_family_friends_interest_left +
                                  savings_emergency_reserve +
                                  passive_investment_total_invested)
            },

            calculate_property_expense_summaries = function(year) {

              # Get all property-related columns for this year
              property_data <- self$results[self$results$Year == year, ]

              # Calculate mortgage costs (principal + interest for all properties)
              self$results[self$results$Year == year, "properties_principal_share"] <-
                sum(property_data[, grep("principal_share$",
                                         names(property_data))],
                    na.rm = TRUE)

              # Calculate family/friends loan costs
              self$results[self$results$Year == year, "properties_interest_share"] <-
                sum(property_data[, grep("interest_share$",
                                         names(property_data))],
                    na.rm = TRUE)

              # Calculate property taxes
              self$results[self$results$Year == year, "properties_property_taxes"] <-
                sum(property_data[, grep("property_taxes$",
                                         names(property_data))],
                    na.rm = TRUE)

              # Calculate maintenance costs
              self$results[self$results$Year == year, "properties_maintenance_cost"] <-
                sum(property_data[, grep("maintenance_cost$",
                                         names(property_data))],
                    na.rm = TRUE)

              # Calculate hausgeld-related costs
              self$results[self$results$Year == year, "properties_hausgeld_fees_total"] <-
                sum(property_data[, grep("hausgeld_fees_total$",
                                         names(property_data))],
                    na.rm = TRUE)

              # Calculate management fee
              self$results[self$results$Year == year, "properties_property_management_fee"] <-
                sum(property_data[, grep("property_management_fee$",
                                         names(property_data))],
                    na.rm = TRUE)

              # Calculate vacancy costs (only relevant for investment properties)
              self$results[self$results$Year == year, "properties_vacancy_months_cost"] <-
                sum(property_data[, grep("vacancy_months_cost$",
                                         names(property_data))],
                    na.rm = TRUE)

              # Calculate vacancy costs (only relevant for investment properties)
              self$results[self$results$Year == year, "properties_warm_lease_income"] <-
                sum(property_data[, grep("warm_lease_income$",
                                         names(property_data))],
                    na.rm = TRUE)

              # Calculate vacancy costs (only relevant for investment properties)
              self$results[self$results$Year == year, "properties_income_taxes_adjustment"] <-
                sum(property_data[, grep("income_taxes_adjustment$",
                                         names(property_data))],
                    na.rm = TRUE)

              # Calculate maximal amortization for all properties
              self$results[self$results$Year == year, "properties_max_mortgage_amortization"] <-
                sum(property_data[, grep("max_mortgage_amortization$", names(property_data))], na.rm = TRUE)
            }
          ),

          lock_objects = FALSE  # This allows dynamic addition of fields
  )


#### Test code ####
if (sys.nframe() == 0) {

  cat("Running test code for DataProcessor class...\n")
  input_config = yaml::read_yaml(file.path("config", "templates", "high_wage_family_rent.yaml"))


  processor <- DataProcessor$new(config = input_config, scenario_name = "example")

  processor$calculate()

  results <- processor$get_results()
  print(head(results,20))

  cat("Test code completed.\n")


  ################################
  input_config_2 <- input_config
  # input_config_2$inputs[["home_property"]]$inputs[[3]]$value <- 2025
  # input_config_2$inputs[["investment_property"]]$inputs[[3]]$value <- 2100

  processor <- DataProcessor$new(config = input_config_2, scenario_name = "home")

  processor$calculate()

  results_2 <- processor$get_results()

  # Filter only relevant columns and concatenate
  results_merged <- subset(rbind(results, results_2), select = c(Year, total_asset, scenario))

  library(ggplot2)
  years_to_display <- seq(2025,
                          2075,
                          by = 4)

  ggplot(results_merged, aes(x = Year, y = total_asset, color = scenario, group = scenario)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Total Asset value (€) - Savings, Properties, Debts and Investments",
         x = "Year",
         y = NULL) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Set2") +
    scale_x_continuous(breaks = years_to_display)

}
