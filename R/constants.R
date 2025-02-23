# Add this to your constants.R file
TEMPLATE_SCENARIOS <- list(
  "Low-income family renting" = "low_wage_family.yaml",
  "Mid-income family renting" = "mid_wage_family_rent.yaml",
  "Mid-income family with home property" = "mid_wage_family_housepoor.yaml",
  "Mid-income family with investment property" = "mid_wage_family_property_investment.yaml",
  "High-income family renting" = "high_wage_family_rent.yaml",
  "High-income family with home property" = "high_wage_family_home.yaml",
  "High-income family with investment property" = "high_wage_family_property_investment.yaml"
)

DEFAULT_TEMPLATE <- "Mid-income family with home property"


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


# Base parameters (non-property ones)
BASE_PARAMETERS <- list(
  # list(
  #   id = "taxes_description.inflation",
  #   name = "Average Annual Inflation",
  #   color = "#E41A1C",
  #   type = "percentage"
  # ),
  list(
    id = "general_life.living_style_costs",
    name = "Average Annual Living Standard Costs",
    color = "#377EB8",
    type = "currency"
  ),
  list(
    id = "general_life.salaries_growth_start_career",
    name = "Average Annual Salary Growth",
    color = "#4DAF4A",
    type = "percentage"
  ),
  list(
    id = "passive_investing.expected_return_on_investment",
    name = "Average Annual Investment Returns",
    color = "#984EA3",
    type = "percentage"
  ),
  list(
    id = "rental.rental_prices_growth",
    name = "Average Annual Own Rent Growth",
    color = "#FFFF33",
    type = "percentage"
  )
)

# Property parameter template
property_parameter_template <- list(
  value_growth = list(
    id_suffix = "value_growth",
    name_suffix = "Average Annual Property Value Growth",
    color = "#FF7F00",
    type = "percentage"
  ),
  value_today = list(
    id_suffix = "value_today",
    name_suffix = "Property Price Today",
    color = "#8B4513",
    type = "currency"
  ),
  purchase_year = list(
    id_suffix = "purchase_year",
    name_suffix = "Property Purchase Year",
    color = "#2F4F4F",
    type = "year"
  ),
  initial_interest_rate = list(
    id_suffix = "initial_interest_rate",
    name_suffix = "Mortgage Interest Rate at Purchase",
    color = "#800080",
    type = "percentage"
  ),
  cold_lease_today = list(
    id_suffix = "cold_lease_today",
    name_suffix = "Cold Rent per Month",
    color = "#20B2AA",
    type = "currency"
  )
)
