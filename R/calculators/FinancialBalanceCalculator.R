library(R6)

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
              expense_columns <- c(
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
              )
              self$results[self$results$Year == year, "total_expenses"] <- rowSums(self$results[self$results$Year == year, expense_columns], na.rm = TRUE)
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


#### Test code ####
if (sys.nframe() == 0) {
  source(file.path("R", "calculators", "BaseCalculator.R"))
}


