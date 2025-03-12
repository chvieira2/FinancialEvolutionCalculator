library(R6)

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

            #' Initialize the DataProcessor object
            #' @param scenario_name Name of the scenario
            #' @param config Configuration settings read from a YAML file
            #' @param initial_year The starting year for calculations
            #' @param final_year The ending year for calculations
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

            #' Perform calculations for all years
            calculate = function() {
              for (year in self$params$initial_year:self$params$final_year) {
                self$calculate_year(year)
              }
            },

            #' Perform calculations for a specific year
            #' @param year The year for which calculations are performed
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

            #' Get the results of the calculations
            #' @return A dataframe containing the results of the calculations
            get_results = function() {
              self$results$scenario <- self$params$scenario_name
              return(self$results)
            }
          ),

          private = list(

            #' Initialize parameters from the configuration
            #' @param config Configuration settings read from a YAML file
            init_parameters = function(config) {
              for (group in names(config)) {
                if (group != "properties") {  # Handle properties separately
                  for (input in config[[group]]$inputs) {
                    self$params[[input$id]] <- input$value
                  }
                }
              }
            },

            #' Initialize properties from the configuration
            #' @param properties_config Configuration settings for properties
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

            #' Calculate the total asset value for a specific year
            #' @param year The year for which the total asset value is calculated
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

            #' Calculate property expense summaries for a specific year
            #' @param year The year for which property expense summaries are calculated
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

              # Calculate warm lease income (only relevant for investment properties)
              self$results[self$results$Year == year, "properties_warm_lease_income"] <-
                sum(property_data[, grep("warm_lease_income$",
                                         names(property_data))],
                    na.rm = TRUE)

              # Calculate income taxes adjustment (only relevant for investment properties)
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

  source(file.path("R", "calculators", "BaseCalculator.R"))
  source(file.path("R", "calculators", "GeneralCalculator.R"))
  source(file.path("R", "calculators", "RentalCalculator.R"))
  source(file.path("R", "calculators", "PropertyCalculator.R"))
  source(file.path("R", "calculators", "FinancialBalanceCalculator.R"))
  source(file.path("R", "calculators", "PassiveInvestingCalculator.R"))

  cat("Running test code for DataProcessor class...\n")
  input_config = yaml::read_yaml(file.path("config", "templates", "mid_wage_family_housepoor.yaml"))
  # input_config$passive_investing$inputs[[3]]$value <- FALSE

  processor <- DataProcessor$new(config = input_config, scenario_name = "example")

  processor$calculate()

  results <- processor$get_results()
  print(head(results,20))

  cat("Test code completed.\n")

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
