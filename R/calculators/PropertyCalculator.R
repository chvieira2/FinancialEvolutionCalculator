library(R6)

PropertyCalculator <-
R6Class("PropertyCalculator",
  inherit = BaseCalculator,
  public = list(
    #' Calculate Property
    #'
    #' This function calculates various property-related financial metrics for a given year.
    #'
    #' @param year The year for which the calculations are being made.
    #' @param params General parameters for the calculations.
    #' @param results Data frame to store the results.
    #' @param property_params Specific parameters related to the property.
    #' @param property_name The name of the property.
    #' @return Updated results data frame.
    calculate_property = function(year, params, results, property_params, property_name) {
      self$params <- params
      self$results <- results
      self$property_params <- property_params
      self$property_name <- property_name
      self$property_type <- property_params$type
      self$prefix <- paste0(self$property_name, "_")

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

    #' Calculate Property Mortgage Details
    #'
    #' This function calculates various details related to the mortgage of the property.
    #'
    #' @param year The year for which the calculations are being made.
    calculate_property_mortgage_details = function(year) {
      private$calculate_property_principal_repayment_rate(year)
      private$calculate_property_mortgage_interest_rate(year)
      private$calculate_property_mortgage_interest_share(year)
      private$calculate_property_mortgage_payment(year)
      private$calculate_property_mortgage_principal_share(year)
      private$calculate_property_mortgage_length_years(year)
      private$calculate_property_mortgage_principal_left(year)
      private$calculate_property_mortgage_interest_left(year)
      private$calculate_property_max_mortgage_amortization(year)
    },

    #' Calculate Property Loan from Family/Friends
    #'
    #' This function calculates various details related to loans taken from family or friends for the property.
    #'
    #' @param year The year for which the calculations are being made.
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

    #' Calculate Property Costs
    #'
    #' This function calculates various costs associated with the property.
    #'
    #' @param year The year for which the calculations are being made.
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

    #' Calculate Investment Property Specifics
    #'
    #' This function calculates various financial metrics specific to investment properties.
    #'
    #' @param year The year for which the calculations are being made.
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

    #' Calculate Property Sale
    #'
    #' This function calculates various details related to the sale of the property.
    #'
    #' @param year The year for which the calculations are being made.
    calculate_property_sale = function(year) {
      private$calculate_property_sale_rate_total(year)
      private$calculate_property_sale_costs(year)
      private$calculate_property_sale_capital_gain_tax(year)
      private$calculate_property_value_liquid(year)
    },

    #' Reset Property Values
    #'
    #' This function resets the property-related values for a given year.
    #'
    #' @param year The year for which the values are being reset.
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

    #' Calculate Property Value Growth Inflation Corrected
    #'
    #' This function calculates the property value growth corrected for inflation.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_value_growth_inflation_corrected = function(year) {
      inflation <- self$results[self$results$Year == year, "inflation"]
      value <- self$correct_by_inflation(self$property_params$value_growth, inflation) * 100
      self$results[self$results$Year == year, paste0(self$prefix, "value_growth_inflation_corrected")] <- self$round_to_2(value)
    },

    #' Calculate Property Market Evaluation
    #'
    #' This function calculates the market evaluation of the property.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Purchase Real Cost
    #'
    #' This function calculates the real cost of purchasing the property, including associated fees.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_purchase_real_cost = function(year) {
      # Calculate total cost including purchase price and associated fees
      property_price <- self$results[self$results$Year == year, paste0(self$prefix, "market_evaluation")]
      total_cost <- property_price * (1 + self$params$estate_agent_fees/100 + self$params$notary_fees_purchase/100 + self$params$purchase_tax/100)

      self$results[self$results$Year == year, paste0(self$prefix, "purchase_real_cost")] <- self$round_to_2(total_cost)
    },

    #' Calculate Property Available for Down Payment
    #'
    #' This function calculates the funds available for the down payment of the property.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Used from Savings
    #'
    #' This function calculates the amount of savings used for the property purchase.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Mortgage Principal
    #'
    #' This function calculates the mortgage principal needed for the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_mortgage_principal = function(year) {
      # Mortgage principal is the difference between total cost and available for down payment and available savings

      total_cost <- self$results[self$results$Year == year, paste0(self$prefix, "purchase_real_cost")]
      available_funds <- self$results[self$results$Year == year, paste0(self$prefix, "available_for_down_payment")]
      savings_used <- self$results[self$results$Year == year, paste0(self$prefix, "used_from_savings")]

      principal_needed <- total_cost - (available_funds + savings_used)
      self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal")] <- self$round_to_2(principal_needed)
    },

    #' Calculate Property Principal Repayment Rate
    #'
    #' This function calculates the principal repayment rate for the property mortgage.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Mortgage Interest Rate
    #'
    #' This function calculates the interest rate for the property mortgage.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Mortgage Interest Share
    #'
    #' This function calculates the interest share of the property mortgage.
    #'
    #' @param year The year for which the calculation is being made.
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
      self$results[self$results$Year == year,
                   paste0(self$prefix, "mortgage_interest_share")] <- self$round_to_2(value)
    },

    #' Calculate Property Mortgage Payment
    #'
    #' This function calculates the mortgage payment for the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_mortgage_payment = function(year) {
      if (self$results[self$results$Year == self$property_params$purchase_year,
                       paste0(self$prefix, "mortgage_principal")] == 0) {
        value <- 0
      } else if (year == self$property_params$purchase_year) {
        # Initial mortgage payment is the sum of principal and interest payments
        principal_share <- -self$results[self$results$Year == self$property_params$purchase_year,
                                         paste0(self$prefix, "mortgage_principal")] * (self$property_params$principal_repayment_rate / 100)

        interest_share <- self$results[self$results$Year == year, paste0(self$prefix, "mortgage_interest_share")]
        value <- principal_share + interest_share
      } else {
        # For subsequent years, calculate based on remaining principal and interest rate
        previous_year <- year - 1
        previous_principal_left <- self$results[self$results$Year == previous_year, paste0(self$prefix, "mortgage_principal_left")]
        interest_rate <- self$results[self$results$Year == year, paste0(self$prefix, "mortgage_interest_rate")] / 100
        initial_payment <- self$results[self$results$Year == self$property_params$purchase_year, paste0(self$prefix, "mortgage_payment")]

        if (previous_principal_left < 0) {
          if (previous_principal_left > initial_payment) {
            # If remaining principal is greater than initial payment, recalculate payment
            value <- previous_principal_left * (1 + interest_rate)
          } else {
            # Otherwise, maintain the same payment as the first year
            value <- initial_payment
          }
        } else {
          # Mortgage is fully paid off
          value <- 0
        }
      }

      self$results[self$results$Year == year,
                   paste0(self$prefix, "mortgage_payment")] <- self$round_to_2(value)
    },

    #' Calculate Property Mortgage Principal Share
    #'
    #' This function calculates the principal share of the property mortgage.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_mortgage_principal_share = function(year) {
      if (self$results[self$results$Year == self$property_params$purchase_year,
                       paste0(self$prefix, "mortgage_principal")] == 0) {
        value <- 0
      } else if (year == self$property_params$purchase_year) {
        value <- -self$results[self$results$Year == self$property_params$purchase_year,
                               paste0(self$prefix, "mortgage_principal")] * (self$property_params$principal_repayment_rate / 100)
      } else {
        previous_year <- year - 1
        mortgage_payment <- self$results[self$results$Year == year,
                                         paste0(self$prefix, "mortgage_payment")]
        interest_share <- self$results[self$results$Year == year,
                                       paste0(self$prefix, "mortgage_interest_share")]
        additional_amortization <- -self$results[self$results$Year == previous_year,
                                                 paste0(self$prefix, "additional_amortization")]

        if (mortgage_payment <= 0) {
          value <- mortgage_payment - interest_share + additional_amortization
        } else {
          value <- 0
        }
      }
      self$results[self$results$Year == year, paste0(self$prefix, "mortgage_principal_share")] <- self$round_to_2(value)
    },

    #' Calculate Property Mortgage Length in Years
    #'
    #' This function calculates the length of the property mortgage in years.
    #'
    #' @param year The year for which the calculation is being made.
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

      self$results[self$results$Year == year,
                   paste0(self$prefix, "mortgage_length_years")] <- self$round_to_2(value)
    },

    #' Calculate Property Mortgage Principal Left
    #'
    #' This function calculates the remaining principal of the property mortgage.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Mortgage Interest Left
    #'
    #' This function calculates the remaining interest of the property mortgage.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Max Mortgage Amortization
    #'
    #' This function calculates the maximum mortgage amortization for the property.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Loan Family/Friends Interest Rate
    #'
    #' This function calculates the interest rate for loans taken from family or friends.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_loan_family_friends_interest_rate = function(year) {
      if (year - self$property_params$purchase_year < self$property_params$duration_payback_family_friends) {
        value <- self$property_params$interest_rate_family_friends
      } else {
        value <- 0
      }
      self$results[self$results$Year == year, paste0(self$prefix, "loan_family_friends_interest_rate")] <- self$round_to_2(value)
    },

    #' Calculate Property Loan Family/Friends Principal Share
    #'
    #' This function calculates the principal share of loans taken from family or friends.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Loan Family/Friends Interest Share
    #'
    #' This function calculates the interest share of loans taken from family or friends.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Loan Family/Friends Payment
    #'
    #' This function calculates the payment for loans taken from family or friends.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Loan Family/Friends Principal Left
    #'
    #' This function calculates the remaining principal of loans taken from family or friends.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Loan Family/Friends Interest Left
    #'
    #' This function calculates the remaining interest of loans taken from family or friends.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Taxes Rate
    #'
    #' This function calculates the property taxes rate.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_taxes_rate = function(year) {
      value <- self$property_params$property_taxes_rate
      self$results[self$results$Year == year, paste0(self$prefix, "property_taxes_rate")] <- self$round_to_2(value)
    },

    #' Calculate Property Taxes
    #'
    #' This function calculates the property taxes.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_taxes = function(year) {
      property_value <- self$results[self$results$Year == year, paste0(self$prefix, "market_evaluation")]
      tax_rate <- self$results[self$results$Year == year, paste0(self$prefix, "property_taxes_rate")] / 100
      value <- property_value * tax_rate
      self$results[self$results$Year == year, paste0(self$prefix, "property_taxes")] <- self$round_to_2(-value)
    },

    #' Calculate Property Maintenance Cost Rate
    #'
    #' This function calculates the maintenance cost rate for the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_maintenance_cost_rate = function(year) {
      value <- self$property_params$maintenance_cost_rate
      self$results[self$results$Year == year, paste0(self$prefix, "maintenance_cost_rate")] <- self$round_to_2(value)
    },

    #' Calculate Property Maintenance Cost
    #'
    #' This function calculates the maintenance cost for the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_maintenance_cost = function(year) {
      property_value <- self$results[self$results$Year == year, paste0(self$prefix, "market_evaluation")]
      maintenance_rate <- self$results[self$results$Year == year, paste0(self$prefix, "maintenance_cost_rate")] / 100
      value <- property_value * maintenance_rate
      self$results[self$results$Year == year, paste0(self$prefix, "maintenance_cost")] <- self$round_to_2(-value)
    },

    #' Calculate Property Hausgeld Maintenance Costs
    #'
    #' This function calculates the Hausgeld maintenance costs for the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_hausgeld_maintenance_costs = function(year) {
      value <- -self$property_params$hausgeld_maintenance_costs * 12
      self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_maintenance_costs")] <- self$round_to_2(value)
    },

    #' Calculate Property Hausgeld Utilities
    #'
    #' This function calculates the Hausgeld utilities costs for the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_hausgeld_utilities = function(year) {
      value <- -self$property_params$hausgeld_utilities * 12
      self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_utilities")] <- self$round_to_2(value)
    },

    #' Calculate Property Hausgeld Reserve Fund
    #'
    #' This function calculates the Hausgeld reserve fund for the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_hausgeld_reserve_fund = function(year) {
      value <- -self$property_params$hausgeld_reserve_fund * 12
      self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_reserve_fund")] <- self$round_to_2(value)
    },

    #' Calculate Property Hausgeld House Management Fee
    #'
    #' This function calculates the Hausgeld house management fee for the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_hausgeld_house_management_fee = function(year) {
      value <- -self$property_params$hausgeld_house_management_fee * 12
      self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_house_management_fee")] <- self$round_to_2(value)
    },

    #' Calculate Property Hausgeld Building Insurance
    #'
    #' This function calculates the Hausgeld building insurance costs for the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_hausgeld_building_insurance = function(year) {
      value <- -self$property_params$hausgeld_building_insurance * 12
      self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_building_insurance")] <- self$round_to_2(value)
    },

    #' Calculate Property Hausgeld Fees Total
    #'
    #' This function calculates the total Hausgeld fees for the property.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Investment Property Lease Rental Growth
    #'
    #' This function calculates the lease rental growth for investment properties.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_investment_property_lease_rental_growth = function(year) {
      inflation <- self$results[self$results$Year == year, "inflation"]
      value <- self$correct_by_inflation(self$property_params$lease_rental_growth, inflation)
      self$results[self$results$Year == year, paste0(self$prefix, "lease_rental_growth")] <- self$round_to_2(value * 100)  # Convert to percentage
    },

    #' Calculate Investment Property Cold Rent from Lease
    #'
    #' This function calculates the cold rent income from lease for investment properties.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Investment Property Warm Lease Income
    #'
    #' This function calculates the warm lease income for investment properties.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_investment_property_warm_lease_income = function(year) {
      value <- sum(
        self$results[self$results$Year == year, paste0(self$prefix, "cold_rent_from_lease")],
        -self$results[self$results$Year == year, paste0(self$prefix, "property_taxes")],
        -self$results[self$results$Year == year, paste0(self$prefix, "hausgeld_fees_total")]
      )
      self$results[self$results$Year == year, paste0(self$prefix, "warm_lease_income")] <- self$round_to_2(value)
    },

    #' Calculate Investment Property Yearly Depreciation Rate
    #'
    #' This function calculates the yearly depreciation rate for investment properties.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_investment_property_yearly_depreciation_rate = function(year) {
      value <- self$property_params$yearly_depreciation_rate
      self$results[self$results$Year == year, paste0(self$prefix, "yearly_depreciation_rate")] <- self$round_to_2(value)
    },

    #' Calculate Investment Property Yearly Depreciation
    #'
    #' This function calculates the yearly depreciation for investment properties.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_investment_property_yearly_depreciation = function(year) {
      purchase_year_value <- self$results[self$results$Year == self$property_params$purchase_year, paste0(self$prefix, "market_evaluation")]
      depreciation_rate <- self$results[self$results$Year == year, paste0(self$prefix, "yearly_depreciation_rate")] / 100
      value <- purchase_year_value * depreciation_rate
      self$results[self$results$Year == year, paste0(self$prefix, "yearly_depreciation")] <- self$round_to_2(-value)
    },

    #' Calculate Investment Property Management Fee
    #'
    #' This function calculates the management fee for investment properties.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_investment_property_management_fee = function(year) {
      cold_rent <- self$results[self$results$Year == year, paste0(self$prefix, "cold_rent_from_lease")]
      value <- cold_rent * (self$property_params$property_management_rate / 100)
      self$results[self$results$Year == year, paste0(self$prefix, "property_management_fee")] <- self$round_to_2(-value)
    },

    #' Calculate Investment Property Total Deductible Expenses
    #'
    #' This function calculates the total deductible expenses for investment properties.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Investment Property Income Taxes Adjustment
    #'
    #' This function calculates the income taxes adjustment for investment properties.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Investment Property Vacancy Months Cost
    #'
    #' This function calculates the cost of vacancy months for investment properties.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_investment_property_vacancy_months_cost = function(year) {
      investment_property_warm_lease_income <- self$results[self$results$Year == year, paste0(self$prefix, "warm_lease_income")]
      value <- -(investment_property_warm_lease_income / 12) * self$property_params$vacancy_months
      self$results[self$results$Year == year, paste0(self$prefix, "vacancy_months_cost")] <- self$round_to_2(value)
    },

    #### Property calculations - Sale ####

    #' Calculate Property Sale Rate Total
    #'
    #' This function calculates the total sale rate for the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_sale_rate_total = function(year) {
      value <- sum(
        self$params$real_estate_agent_fees_sale,
        self$params$notary_fees_sale,
        self$params$property_transfer_taxes_sale,
        self$params$miscelanous_fees_costs_sale
      )
      self$results[self$results$Year == year, paste0(self$prefix, "sale_rate_total")] <- self$round_to_2(value)
    },

    #' Calculate Property Sale Costs
    #'
    #' This function calculates the costs associated with the sale of the property.
    #'
    #' @param year The year for which the calculation is being made.
    calculate_property_sale_costs = function(year) {
      property_value <- self$results[self$results$Year == year, paste0(self$prefix, "market_evaluation")]
      sale_rate <- self$results[self$results$Year == year, paste0(self$prefix, "sale_rate_total")] / 100
      value <- property_value * sale_rate
      self$results[self$results$Year == year, paste0(self$prefix, "sale_costs")] <- self$round_to_2(-value)
    },

    #' Calculate Property Sale Capital Gain Tax
    #'
    #' This function calculates the capital gain tax for the sale of the property.
    #'
    #' @param year The year for which the calculation is being made.
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

    #' Calculate Property Value Liquid
    #'
    #' This function calculates the liquid value of the property after sale.
    #'
    #' @param year The year for which the calculation is being made.
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

#### Test code ####
if (sys.nframe() == 0) {
  source(file.path("R", "calculators", "BaseCalculator.R"))
}
