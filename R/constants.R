ESSENTIAL_PARAMS <- list(
  general_life = c("net_annual_income", "living_style_costs", "savings", "expected_year_retirement"),
  family = c("year_first_child_is_born", "year_second_child_is_born"),
  rental = c("rent_month", "fixed_housing_costs"),
  passive_investing = c("expected_return_on_investment"),
  taxes_description = c("inflation", "income_tax"),
  lump_sum = c("lump_sum_1", "lump_sum_1_year"),
  property_tax = c()
)

# Add other constants that might be used across modules
PROPERTY_TYPES <- c(
  "Home" = "home",
  "Investment" = "investment"
)

DEFAULT_YEARS <- list(
  MIN_YEAR = 2025,
  MAX_YEAR = 2075
)
