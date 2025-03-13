library(R6)

BaseCalculator <-
  R6Class("BaseCalculator",
          public = list(
            # Constructor method for the BaseCalculator class.
            # Initializes a new instance of the BaseCalculator class.
            initialize = function() {
            },

            # Rounds a given number to 2 decimal places.
            # 
            # Args:
            #   x: A numeric value to be rounded.
            #
            # Returns:
            #   A numeric value rounded to 2 decimal places.
            round_to_2 = function(x) {
              return(round(x, 2))
            },

            # Adjusts a given value by an inflation rate.
            #
            # Args:
            #   value: A numeric value representing the initial amount.
            #   inflation: A numeric value representing the inflation rate in percentage.
            #
            # Returns:
            #   A numeric value adjusted by the inflation rate.
            correct_by_inflation = function(value, inflation) {
              return(((1 + value/100) / (1 + inflation/100)) - 1)
            },

            # Calculates the funds obtained from the sale of properties in a given year.
            #
            # Args:
            #   year: An integer representing the year of the property sale.
            #   results: A data frame containing the results of previous calculations.
            #   properties: A list of properties with their respective parameters.
            #
            # Returns:
            #   A numeric value representing the total funds from property sales, rounded to 2 decimal places.
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

            # Checks if a loan was taken in a given year.
            #
            # Args:
            #   year: An integer representing the year to check for loan activity.
            #   results: A data frame containing the results of previous calculations.
            #
            # Returns:
            #   A numeric value representing the total loan amount taken in the specified year.
            check_if_loan_taken = function(year, results) {
              loan_taken <- sum(results[results$Year == year, grep("mortgage_principal", names(results), value = TRUE)])

              return(loan_taken)
            }
          )
  )


#### Test code ####
if (sys.nframe() == 0) {

}
