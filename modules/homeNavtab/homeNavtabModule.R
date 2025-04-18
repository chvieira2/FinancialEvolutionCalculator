library(DT)
library(formattable)

source("modules/homeNavtab/welcomePageContent.R")
source("modules/homeNavtab/parametersPageContent.R")
source("modules/homeNavtab/scenario1Content.R")
source("modules/homeNavtab/scenario2Content.R")
source("modules/homeNavtab/scenario3Content.R")
source("modules/homeNavtab/aboutPageContent.R")
NAVTAB_CONTENT <- safelyLoadConfig("config/homeNavtabContent.yaml")


homeNavigationModuleUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      class = "col-lg-12 col-md-4 col-sm-12",
      width = 3,
      shinyTree(
        ns("home_toc"),
        checkbox = FALSE,
        search = FALSE,
        theme = "proton", # default, default-dark, or proton
        themeIcons = FALSE,
        multiple = FALSE
      )
    ),
    mainPanel(
      class = "col-lg-0 col-md-8 col-sm-12",
      width = 9,
      uiOutput(ns("home_content"))
    )
  )
}

homeNavigationModuleServer <- function(id, is_mobile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the tree structure
    output$home_toc <- shinyTree::renderTree({
      list(
        "Welcome" = structure("welcome_page", stopened = TRUE),
        "Calculator Parameters" = structure("parameters_page", stopened = TRUE),
        "Scenario 1: Gene and Jean" = structure("scenario_1", stopened = TRUE),
        "Scenario 2: Alex and Max" = structure("scenario_2", stopened = TRUE),
        "Scenario 3: Jack and Beck" = structure("scenario_3", stopened = TRUE),
        "About" = structure("about_page", stopened = TRUE)
      )
    })

    # Render the content based on the selected tree node
    output$home_content <- renderUI({
      selected <- unlist(shinyTree::get_selected(input$home_toc, format = "names"))

      label_to_handle <- list(
        "Welcome" = "welcome_page",
        "Calculator Parameters" = "parameters_page",
        "Scenario 1: Gene and Jean" = "scenario_1",
        "Scenario 2: Alex and Max" = "scenario_2",
        "Scenario 3: Jack and Beck" = "scenario_3",
        "About" = "about_page"
      )

      selected_handle <- if (is.null(selected)) "welcome_page" else label_to_handle[[selected]]
      switch(selected_handle,
             "welcome_page" = welcomePageContent(is_mobile),
             "parameters_page" = parametersPageContent(is_mobile),
             "scenario_1" = scenario1Content(is_mobile),
             "scenario_2" = scenario2Content(is_mobile),
             "scenario_3" = scenario3Content(is_mobile),
             "about_page" = aboutPageContent(is_mobile)
      )

    })
  })
}



# Functions for scenarios content creation
create_if_missing <- function(df, cols) {
  for(col in setdiff(cols, names(df))) {
    df[[col]] <- 0
  }
  df
}

table_for_vis <- function(calculations_df) {
  table_columns_selection <- c(
    "Year",

    "net_annual_salary",
    "properties_warm_lease_income",
    "properties_income_taxes_adjustment",
    "passive_investment_return_from_previous_year",

    "living_costs",
    "vorabpauschale_tax_from_previous_year",

    "rental_cost", "rental_housing_cost",
    "properties_principal_share", "properties_interest_share",
    "properties_property_taxes", "properties_maintenance_cost",
    "properties_hausgeld_fees_total",
    "properties_property_management_fee", "properties_vacancy_months_cost"
  )

  calculations_df[1:10,] %>%
    create_if_missing(table_columns_selection) %>%
    select(all_of(table_columns_selection)) %>%
    rename("Salary" = net_annual_salary,
           "Rent from Tenants" = properties_warm_lease_income,
           "Tax Deductions" = properties_income_taxes_adjustment,
           "Living Costs" = living_costs
    ) %>%
    mutate(
      "Passive Investments Return" = rowSums(across(c(
        "passive_investment_return_from_previous_year",
        "vorabpauschale_tax_from_previous_year"
      ))),
      "Income Total" = rowSums(across(c(
        "Salary",
        "Rent from Tenants",
        "Tax Deductions",
        "Passive Investments Return"
      ))),
      "Own Rent" = rowSums(across(c(
        rental_cost,
        rental_housing_cost
      ))),
      "Mortgage and Loan" = rowSums(across(c(
        properties_principal_share,
        properties_interest_share
      ))),
      "Property Costs" = rowSums(across(c(
        properties_property_taxes,
        properties_maintenance_cost,
        properties_hausgeld_fees_total,
        properties_property_management_fee,
        properties_vacancy_months_cost
      ))),
      "Expenses Total" = rowSums(across(c(
        "Living Costs",
        "Own Rent",
        "Mortgage and Loan",
        "Property Costs"

      ))),
      "Total Saved" = rowSums(across(c(
        "Income Total",
        "Expenses Total"
      )))
    ) %>%
    select(c(
      "Year",
      "Salary", "Rent from Tenants", "Tax Deductions",
      "Passive Investments Return", "Income Total",
      "Living Costs",
      "Own Rent", "Mortgage and Loan", "Property Costs", "Expenses Total",
      "Total Saved"
    ))
}

format_financial_table <- function(table_data) {
  # Define colors for different column types
  colors <- list(
    year = "#808080",           # Grey for Year
    income = "#d4edda",         # Light green for income items
    income_total = "#28a745",   # Darker green for income total
    expense = "#f8d7da",        # Light red for expense items
    expense_total = "#dc3545",  # Darker red for expense total
    total_saved = "#ffc107"     # Gold/yellow for total saved
  )

  # Format the table with DT
  datatable(
    table_data,
    options = list(
      pageLength = 10,
      dom = 't',
      ordering = FALSE,
      scrollX = FALSE,  # Prevent horizontal scrolling
      autoWidth = FALSE, # Disable auto-width calculation
      columnDefs = list(list(width = "60px", targets = which(colnames(table_data) != "Year") - 1),
                        list(width = "30px", targets = which(colnames(table_data) == "Year") - 1))
    ),
    width = "100%",
    rownames = FALSE
  ) %>%
    formatStyle(
      "Year",
      backgroundColor = colors$year,
      fontWeight = "bold",
      textAlign = "center"
    ) %>%
    formatStyle(
      c("Salary", "Rent from Tenants", "Tax Deductions", "Passive Investments Return"),
      backgroundColor = colors$income,
      color = "black"
    ) %>%
    formatStyle(
      "Income Total",
      backgroundColor = colors$income_total,
      color = "white",
      fontWeight = "bold"
    ) %>%
    formatStyle(
      c("Living Costs", "Own Rent", "Mortgage and Loan", "Property Costs"),
      backgroundColor = colors$expense,
      color = "black"
    ) %>%
    formatStyle(
      "Expenses Total",
      backgroundColor = colors$expense_total,
      color = "white",
      fontWeight = "bold"
    ) %>%
    formatStyle(
      "Total Saved",
      backgroundColor = colors$total_saved,
      fontWeight = "bold"
    ) %>%
    formatCurrency(
      columns = c(2:ncol(table_data)),  # Format all except Year as currency
      currency = "€",
      interval = 3,
      mark = ",",
      digits = 0
    )
}
