library(R6)

PassiveInvestingCalculator <- R6Class("PassiveInvestingCalculator",
  inherit = BaseCalculator,
  public = list(
    #' Calculate passive investment components after financial balance
    #'
    #' @param year Calculation year
    #' @param params Input parameters
    #' @param results Results data frame
    #' @param properties Property configurations
    calculate_passive_investment = function(year, params, results, properties) {
      self$params <- params
      self$results <- results
      self$properties <- properties

      # Core passive investment calculations
      private$calculate_passive_investment_money_needed_from_liquid(year)
      if (year != self$params$initial_year) {
        private$calculate_passive_investment_money_withdrawn_from_capital_gains(year)
        private$calculate_passive_investment_money_withdrawn_from_contributions(year)
      } else {
        self$results[self$results$Year == year, "passive_investment_money_withdrawn_from_capital_gains"] <- 0
        self$results[self$results$Year == year, "passive_investment_money_withdrawn_from_contributions"] <- 0
      }
      private$calculate_passive_investment_yearly_contribution(year)
      private$calculate_passive_investment_contributions_accumulated(year)
      private$calculate_passive_investment_total_invested(year)
      private$calculate_passive_investment_total_invested_liquid(year)

      return(self$results)
    }
  ),

  private = list(
    #' Calculate money needed from liquid investments
    #'
    #' @param year Calculation year
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

    #' Calculate money withdrawn from capital gains
    #'
    #' @param year Calculation year
    calculate_passive_investment_money_withdrawn_from_capital_gains = function(year) {
      previous_year <- year - 1
      previous_total_invested <- self$results[self$results$Year == previous_year, "passive_investment_total_invested"]
      previous_contributions_accumulated <- self$results[self$results$Year == previous_year, "passive_investment_contributions_accumulated"]
      previous_capital_gains_tax_rate <- self$results[self$results$Year == previous_year, "capital_gains_tax_rate"] / 100
      money_needed <- self$results[self$results$Year == year,
                                   "passive_investment_money_needed_from_liquid"]

      # Calculate the tax rate
      tax_rate <- self$results[self$results$Year == previous_year, "capital_gains_tax_rate"] / 100

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

    #' Calculate money withdrawn from contributions
    #'
    #' @param year Calculation year
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

    #' Calculate yearly contribution to passive investment
    #'
    #' @param year Calculation year
    calculate_passive_investment_yearly_contribution = function(year) {
      loan_taken <- self$check_if_loan_taken(year, self$results)
      if (loan_taken > 0) {
        contribution <- 0
      } else {
        contribution <- self$results[self$results$Year == year, "cash_flow_to_investment"]
      }

      self$results[self$results$Year == year, "passive_investment_yearly_contribution"] <- self$round_to_2(contribution)
    },

    #' Calculate accumulated contributions to passive investment
    #'
    #' @param year Calculation year
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
        accumulated <- max(0, previous_total_accumulated + yearly_contribution + taken_from_contributions)
      }

      self$results[self$results$Year == year, "passive_investment_contributions_accumulated"] <- self$round_to_2(accumulated)
    },

    #' Calculate total invested in passive investment
    #'
    #' @param year Calculation year
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

    #' Calculate total liquid invested in passive investment
    #'
    #' @param year Calculation year
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

library(testthat)

#### Test code ####
if (sys.nframe() == 0) {
  source(file.path("R", "calculators", "BaseCalculator.R"))
  
  # Arrange
  params <- list(initial_year = 2021, apply_vorabpauschale = TRUE, savings = 10000, savings_emergency_reserve = 2000)
  results <- data.frame(Year = 2021:2022, savings_emergency_reserve = c(2000, 0), cash_flow = c(1000, -500), cash_flow_to_investment = c(500, 0), capital_gains_tax_rate = c(25, 25), passive_investment_return_from_previous_year = c(0, 100))
  properties <- list(property1 = list(purchase_year = 2022))
  
  calculator <- PassiveInvestingCalculator$new()
  
  # Act
  results <- calculator$calculate_passive_investment(2021, params, results, properties)
  results <- calculator$calculate_passive_investment(2022, params, results, properties)
  
  # Assert
  test_that("PassiveInvestingCalculator calculates correctly for initial year", {
    expect_equal(results[results$Year == 2021, "passive_investment_money_withdrawn_from_capital_gains"], 0)
    expect_equal(results[results$Year == 2021, "passive_investment_money_withdrawn_from_contributions"], 0)
  })
  
  test_that("PassiveInvestingCalculator calculates correctly for subsequent year", {
    expect_true(results[results$Year == 2022, "passive_investment_money_withdrawn_from_capital_gains"] < 0)
    expect_true(results[results$Year == 2022, "passive_investment_money_withdrawn_from_contributions"] <= 0)
  })
}

