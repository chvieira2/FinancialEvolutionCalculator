library(R6)

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


#### Test code ####
if (sys.nframe() == 0) {

}
