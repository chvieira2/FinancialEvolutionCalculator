library(R6)

RentalCalculator <-
  R6Class("RentalCalculator",
          inherit = BaseCalculator,
          public = list(
            #' Calculate rental costs for a given year
            #' 
            #' @param year The year for which to calculate rental costs
            #' @param params A list of parameters including rental prices growth, rent per month, and initial year
            #' @param results A data frame to store the results
            #' @return Updated results data frame with calculated rental costs
            calculate_rental = function(year, params, results) {
              self$params <- params
              self$results <- results

              self$calculate_rental_cost_growth_inflation_corrected(year)
              self$calculate_rental_cost(year)
              self$calculate_rental_housing_cost(year)

              return(self$results)
            },

            #' Calculate rental cost growth corrected for inflation
            #' 
            #' @param year The year for which to calculate the inflation-corrected rental cost growth
            calculate_rental_cost_growth_inflation_corrected = function(year) {
              inflation <- self$results[self$results$Year == year, "inflation"]
              value <- self$correct_by_inflation(self$params$rental_prices_growth, inflation) * 100
              self$results[self$results$Year == year, "rental_cost_growth_inflation_corrected"] <- self$round_to_2(value)
            },

            #' Calculate rental cost for a given year
            #' 
            #' @param year The year for which to calculate the rental cost
            calculate_rental_cost = function(year) {
              if (year == self$params$initial_year) {
                value <- -self$params$rent_month * 12
              } else {
                growth_rate <- self$results[self$results$Year == year, "rental_cost_growth_inflation_corrected"] / 100
                value <- (-self$params$rent_month * 12) * (1 + growth_rate)^(year - self$params$initial_year)
              }
              self$results[self$results$Year == year, "rental_cost"] <- self$round_to_2(value)
            },

            #' Calculate rental housing cost for a given year
            #' 
            #' @param year The year for which to calculate the rental housing cost
            calculate_rental_housing_cost = function(year) {
              self$results[self$results$Year == year, "rental_housing_cost"] <- self$results[self$results$Year == year, "housing_cost"]
            }

          ),

          lock_objects = FALSE  # This allows dynamic addition of fields
  )


#### Test code ####
if (sys.nframe() == 0) {
  source(file.path("R", "calculators", "BaseCalculator.R"))
}

