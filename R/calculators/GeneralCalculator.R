library(R6)

GeneralCalculator <-
  R6Class("GeneralCalculator",
          inherit = BaseCalculator,
          public = list(
            # Performs general calculations for a given year.
            #
            # Args:
            #   year: An integer representing the year for which calculations are performed.
            #   params: A list of parameters required for the calculations.
            #   results: A data frame to store the results of the calculations.
            #
            # Returns:
            #   A data frame with updated results after performing the calculations.
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

              private$calculate_housing_cost(year)

              return(self$results)
            }
          ),

          private = list(

            # Calculates the growth rate of salaries adjusted for inflation.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the inflation-corrected salary growth rate.
            calculate_salaries_growth = function(year) {
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

              inflation <- self$results[self$results$Year == year, "inflation"]
              inflation_corrected_growth <- self$correct_by_inflation(growth, inflation)

              if (year == self$params$expected_year_retirement) {
                inflation_corrected_growth <- -self$params$expected_salary_reduction_retirement/100
              }

              self$results[self$results$Year == year, "salaries_growth_inflation_corrected"] <- self$round_to_2(inflation_corrected_growth * 100)
            },

            # Calculates the salary for a given year.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the calculated salary.
            calculate_salary = function(year) {
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

            # Calculates the change in living costs due to Elternzeit (parental leave).
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the change in living costs due to Elternzeit.
            calculate_living_costs_change_elternzeit = function(year) {
              change <- 0
              if ((!is.null(self$params$year_first_child_is_born) && year == self$params$year_first_child_is_born) ||
                  (!is.null(self$params$year_second_child_is_born) && year == self$params$year_second_child_is_born)) {
                change <- self$params$living_costs_change_elternzeit
              }

              self$results[self$results$Year == year, "living_costs_change_elternzeit"] <- self$round_to_2(change)
            },

            # Calculates the cost for a single child based on their age.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #   child_birth_year: An integer representing the birth year of the child.
            #
            # Returns:
            #   A numeric value representing the cost for the child.
            calculate_child_cost = function(year, child_birth_year) {
              if (is.null(child_birth_year)) return(0)
              child_age <- year - child_birth_year
              if (child_age < 0) return(0)
              if (child_age <= 3) return(-self$params$annual_cost_per_child_0_3 + self$params$elterngeld)
              if (child_age <= 10) return(-self$params$annual_cost_per_child_4_10 + self$params$elterngeld)
              if (child_age <= 18) return(-self$params$annual_cost_per_child_11_18 + self$params$elterngeld)
              if (child_age <= 25) return(-self$params$annual_cost_per_child_19_25 + self$params$elterngeld)
              return(0)
            },

            # Calculates the total living costs for the family.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the total living costs for the family.
            calculate_living_costs_family = function(year) {
              total_cost <- 0

              if (!is.null(self$params$year_first_child_is_born) && year >= self$params$year_first_child_is_born) {
                total_cost <- total_cost + private$calculate_child_cost(year, self$params$year_first_child_is_born)
              }
              if (!is.null(self$params$year_second_child_is_born) && year >= self$params$year_second_child_is_born) {
                total_cost <- total_cost + private$calculate_child_cost(year, self$params$year_second_child_is_born)
              }

              self$results[self$results$Year == year, "living_costs_family"] <- self$round_to_2(total_cost)
            },

            # Calculates the total living costs for a given year.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the total living costs.
            calculate_living_costs = function(year) {
              base_living_costs <- -self$params$living_style_costs * 12
              change_elternzeit <- self$results[self$results$Year == year, "living_costs_change_elternzeit"] / 100
              family_costs <- self$results[self$results$Year == year, "living_costs_family"]

              living_standard_costs <- base_living_costs * (1 - change_elternzeit)
              self$results[self$results$Year == year, "living_standard_costs"] <- self$round_to_2(living_standard_costs)
              total_living_costs <- living_standard_costs + family_costs
              self$results[self$results$Year == year, "living_costs"] <- self$round_to_2(total_living_costs)
            },

            # Calculates the expected money for investment savings for a given year.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the expected money for investment savings.
            calculate_expected_money_for_investment_savings = function(year) {
              if (year == self$params$initial_year) {
                value <- self$params$percent_income_investing/100*self$params$net_annual_income*12
              } else {
                value <- self$params$percent_income_investing/100*self$results[self$results$Year == year, "salary"]
              }

              self$results[self$results$Year == year, "expected_money_for_investment_savings"] <- self$round_to_2(value)
            },

            # Calculates the net annual salary for a given year.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the net annual salary.
            calculate_net_annual_salary = function(year) {
              if (year == self$params$initial_year) {
                base_salary <- self$params$net_annual_income * 12
              } else {
                base_salary <- self$results[self$results$Year == year, "salary"]
              }

              reduction <- 1
              if (!is.null(self$params$year_first_child_is_born) && year == self$params$year_first_child_is_born) {
                reduction <- 1 - (self$params$salary_reduction_elternzeit / 100)
              } else if (!is.null(self$params$year_second_child_is_born) && year == self$params$year_second_child_is_born) {
                reduction <- 1 - (self$params$salary_reduction_elternzeit / 100)
              }
              reduced_salary <- base_salary * reduction

              net_salary <- reduced_salary - self$results[self$results$Year == year, "expected_money_for_investment_savings"]

              self$results[self$results$Year == year, "net_annual_salary"] <- self$round_to_2(net_salary)
            },

            # Calculates the lump sums for a given year.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the lump sums.
            calculate_lump_sums = function(year) {
              total_lump_sum <- 0

              if (!is.null(self$params$lump_sum_1_year) && year == self$params$lump_sum_1_year && !is.null(self$params$lump_sum_1) && self$params$lump_sum_1 != 0) {
                total_lump_sum <- total_lump_sum + self$params$lump_sum_1
              }

              if (!is.null(self$params$lump_sum_2_year) && year == self$params$lump_sum_2_year && !is.null(self$params$lump_sum_2) && self$params$lump_sum_2 != 0) {
                total_lump_sum <- total_lump_sum + self$params$lump_sum_2
              }

              self$results[self$results$Year == year, "lump_sums"] <- self$round_to_2(total_lump_sum)
            },

            # Calculates the expected yearly return on passive investment adjusted for inflation.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the expected yearly return on passive investment adjusted for inflation.
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

              self$results[self$results$Year == year, "expected_yearly_return_on_passive_investment_inflation_corrected"] <- self$round_to_2(value * 100)
            },

            # Calculates the total taxes on income for a given year.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the total taxes on income.
            calculate_total_taxes_on_income = function(year) {
              total_tax <- (self$params$income_tax / 100*(1+self$params$solidarity_surcharge_tax / 100+self$params$church_tax / 100))*100
              self$results[self$results$Year == year, "total_taxes_on_income"] <- self$round_to_2(total_tax)
            },

            # Calculates the capital gains tax rate for a given year.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the capital gains tax rate.
            calculate_capital_gains_tax_rate = function(year) {
              total_tax <- (self$params$capital_gains_tax_rate / 100*(1+self$params$solidarity_surcharge_tax / 100+self$params$church_tax / 100))*100

              self$results[self$results$Year == year, "capital_gains_tax_rate"] <- self$round_to_2(total_tax)
            },

            # Calculates the passive investment return from the previous year.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the passive investment return from the previous year.
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

            # Calculates the Vorabpauschale for passive investments.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the Vorabpauschale.
            calculate_passive_investment_vorabpauschale = function(year) {
              tax_free_allowance <- switch(self$params$tax_free_allowance_type,
                                           "single" = 1000,
                                           "married" = 2000,
                                           1000)

              if (year == self$params$initial_year) {
                self$results[self$results$Year == year, "passive_investment_vorabpauschale"] <- self$round_to_2(tax_free_allowance)
              } else {
                previous_year <- year - 1

                inflation_previous_year <- self$results[self$results$Year == previous_year, "inflation"]
                passive_investment_vorabpauschale_previous_year <- self$results[self$results$Year == previous_year, "passive_investment_vorabpauschale"]

                self$results[self$results$Year == year, "passive_investment_vorabpauschale"] <- self$round_to_2(passive_investment_vorabpauschale_previous_year * (1 + inflation_previous_year / 100))
              }
            },

            # Calculates the Vorabpauschale tax from the previous year.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the Vorabpauschale tax from the previous year.
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

            # Calculates the housing cost for a given year.
            #
            # Args:
            #   year: An integer representing the year for which the calculation is performed.
            #
            # Returns:
            #   Updates the results data frame with the calculated housing cost.
            calculate_housing_cost = function(year) {
              if (year == self$params$initial_year) {
                value <- -self$params$fixed_housing_costs * 12
              } else {
                previous_year <- year - 1
                previous_cost <- self$results[self$results$Year == previous_year, "housing_cost"]
                growth_rate <- self$results[self$results$Year == previous_year, "inflation"] / 100
                value <- previous_cost * (1 + growth_rate)
              }
              self$results[self$results$Year == year, "housing_cost"] <- self$round_to_2(value)
            }
          ),

          lock_objects = FALSE  # This allows dynamic addition of fields
  )


#### Test code ####
if (sys.nframe() == 0) {
  source(file.path("R", "calculators", "BaseCalculator.R"))
}
