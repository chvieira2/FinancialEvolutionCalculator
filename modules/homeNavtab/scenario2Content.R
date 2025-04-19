scenario2Content <- function(is_mobile) {

  couple_name <- "Alex and Max"

  # Generate the figures for this scenario
  for (scenario in c("rent", "homeowner", "landlord")) {
    scenario_name <- paste0("mid_wage_family_", scenario)
    config <- safelyLoadConfig(file.path("config", "templates",
                                         paste0("inputs_", scenario_name, ".yaml")))
    plot_data <- read.csv(file.path("article",
                                    paste0("calculations_", scenario_name, ".csv")))

    # Dynamically create variables from the inputs
    lapply(config, function(section) {
      if (is.list(section) && length(section) > 0 && !is.character(section[[1]])) {
        # This is the properties section - handle each property separately
        lapply(section, function(property) {
          property_type <- property$type
          lapply(property$inputs, function(input) {
            assign(paste0(scenario, "_", property_type, "_", input$id), input$value, envir = .GlobalEnv)
          })
        })
      } else {
        # Regular section with inputs
        if (is.null(section$inputs) && length(section) > 0) section <- section[[1]]
        lapply(section$inputs, function(input) {
          assign(paste0(scenario, "_", input$id), input$value, envir = .GlobalEnv)
        })
      }
    })

    assign(paste0("calculations_", scenario_name),
           plot_data)

    assign(paste0(scenario, "_AssetEvolution"),
           generateYearlyAssetProgressionPlot(
             plot_data,
             config
           ))
    assign(paste0(scenario, "_FinMetrics"),
           generateFinancialMetricsPlot(
             plot_data,
             legend_bool = TRUE
           ))
    assign(paste0(scenario, "_IncomeComp"),
           generateStackedAreaPlot(
             plot_data = plot_data,
             plot_type = "income",
             legend_bool = TRUE
           ))
    assign(paste0(scenario, "_ExpensesComp"),
           generateStackedAreaPlot(
             plot_data = plot_data,
             plot_type = "expenses",
             legend_bool = TRUE
           ))

    if (scenario %in% c("rent", "homeowner", "landlord" )) {
      load(file.path("article", paste0("SensitivityResults_", scenario_name, ".RData")))
      assign(paste0(scenario, "_SensitivityAnalysis_height"),
             calculate_plot_heights(final_results$selected_parameters,
                                    is_mobile)
      )
      assign(paste0(scenario, "_SensitivityAnalysis"),
             generateSensitivityPlot(
               analysis_results = final_results$calculations,
               selected_parameters = final_results$selected_parameters,
               year_range = final_results$year_range,
               is_mobile = is_mobile,
               steps = final_results$steps
             ))

      assign(paste0(scenario, "_table_for_vis"),
             format_financial_table(
               table_for_vis(plot_data)))
    }
  }


  div(
    style = if(is_mobile) "padding-left: 15px; padding-right: 15px;" else "padding-left: 15px",

    # Header Section
    h2(paste0(couple_name, ", average income earners")),
    p(em(paste0(couple_name, " are a 30-year-old mid-income family in Berlin, Germany. With average salaries in Germany, they have built some savings over their careers. They wonder whether transitioning from renting to purchasing a property is a financially sound decision."))),

    # Financial Goals
    h3("Financial Goals"),
    tags$ul(
      tags$li(tags$u("Lifestyle"), " - ", NAVTAB_CONTENT$SCENARIOS$LIFESTYLE_DESCRIPTION),
      tags$li(tags$u("Living Costs"), " - They carefully manage monthly expenses, and having some extra to increase expenses with themselves could improve their life quality."),
      tags$li(tags$u("Work and Retirement"), " - ", NAVTAB_CONTENT$SCENARIOS$LATE_LIFE_DESCRIPTION),
      tags$li(tags$u("Generational Wealth"), " - ", NAVTAB_CONTENT$SCENARIOS$GENERATIONAL_WEALTH_DESCRIPTION),
      tags$li(tags$u("Donations"), " - ", NAVTAB_CONTENT$SCENARIOS$EXTRA_CASH_DESCRIPTION)
    ),
    hr(),

    h3("Personal Finances Parameters"),
    tags$ul(
      tags$li(tags$u("Income"), paste0(" - Their combined net salary after taxes and insurances (e.g.: health insurance) is ", format(rent_net_annual_income, big.mark=","), "€ per month. In the early years, their earnings grow at around ", rent_salaries_growth_start_career, "% each year due to promotions, extra training, and experience. Later on, growth slows down as opportunities become fewer.")),
      tags$li(tags$u("Living standards"), paste0(" - They try their best to keep living costs low but can afford some comfort, spending about ", format(rent_living_style_costs , big.mark=","), "€ each month on groceries, clothing, commuting, entertainment, holidays, and other needs. While what they spend on might change over the years, the overall monthly cost stays fairly constant (after adjusting for inflation). They have some room to reduce these expenses if ever needed.")),
      tags$li(tags$u("Retirement"), paste0(" - They plan to retire latest at age 70 in 2065. Despite having pension schemes included in their living costs, their salary will drop significantly (", rent_expected_salary_reduction_retirement, "%).")),
      tags$li(tags$u("Family Size"), paste0(" - They are expecting their first child and plan to have a second one shortly thereafter.")),
      tags$li(tags$u("Passive Investments"), paste0(" - They put all their savings in accumulating ETFs with a gross return of ", rent_expected_return_on_investment, "% per year, transitioning to safer investments yielding ", rent_expected_conservative_return_on_investment, "% per year over the years. Any capital gains on these investments are taxed at ", rent_capital_gains_tax_rate, "% in Germany upon withdrawal and annually with the advanced tax system in Germamy (Vorabpauschale).")),
      tags$li(tags$u("Taxes"), paste0(" - They pay income tax in Germany (", rent_income_tax, "% plus solidarity surcharge) and do not pay voluntary church taxes.")),
      tags$li(tags$u("Debts"), paste0(" - If they fall into debt (i.e., if their emergency reserve drops below zero), they have to use a credit card, which carries a high interest rate of ", rent_interest_rate_cash_flow_debt, "% per year.")),
      tags$li(tags$u("Lump Sums"), paste0(" - They expect to inherit roughly ", format(rent_lump_sum_1, big.mark=","), "€ (around the year ", rent_lump_sum_1_year, ") and also anticipate major expenses of about ", format(rent_lump_sum_2, big.mark=","), "€ (around the year ", rent_lump_sum_2_year, ")."))
    ),
    hr(),

    h3("Options on the Table"),
    p(paste0(couple_name, " are considering three paths for their future:")),
    tags$ul(
      tags$li(tags$u("Renters"), paste0(" - They continue renting an old, 3-room flat in Berlin (valued at roughly ", format(round(6000/18*rent_rent_month, -3), big.mark=",", scientific = FALSE), "€) on the outskirts of the city center for a monthly rent of ", format(rent_rent_month, big.mark=","), "€ plus an additional ", format(rent_fixed_housing_costs, big.mark=","), "€/month in fixed charges. They expect rent to increase over time (", rent_rental_prices_growth, "% annually). Upon retirement, they plan to use their savings to buy a property for personal use. The maximum price they will be able to afford is ", format(rent_home_value_today, big.mark=","),"€ in today's property price equivalent after market growth and inflation correction.")),
      tags$li(tags$u("Homeowners"), paste0(" - They purchase a 3-room flat in Berlin for ", format(homeowner_home_value_today, big.mark=","),"€. The property is located far from the city center, but within reach of public transportation. As initial capital to secure the cheapest mortgage (", homeowner_home_initial_interest_rate, "% per year interest rate, repayment rate of ", homeowner_home_principal_repayment_rate, "%), they use all available savings (besides their emergency reserve) and borrow from family or friends ", homeowner_home_loan_family_friends, "€, to be paid in ", homeowner_home_duration_payback_family_friends, " years. They do not amortize the loan over the years, instead choosing to invest savings passively. The property they own will appreciate over time (", homeowner_home_value_growth, "% per year).")),
      tags$li(tags$u("Landlords"), paste0(" - They purchase a small and old 2-room flat outside of Berlin for ", format(landlord_investment_value_today, big.mark=","),"€ and rent it out for a monthly rent of ", format(landlord_investment_cold_lease_today + landlord_investment_hausgeld_fees_total, big.mark=","), "€ (warm rent). As initial capital to secure the cheapest mortgage (", landlord_investment_initial_interest_rate, "% per year interest rate, repayment rate of ", landlord_investment_principal_repayment_rate, "%), they use all available savings (besides their emergency reserve). They do not amortize the loan over the years, instead choosing to invest savings passively. The property they own will appreciate over time (", landlord_investment_value_growth, "% per year). Upon retirement, they plan to use their savings to buy a property for personal use. The maximum price they will be able to afford is ", format(landlord_home_value_today, big.mark=","),"€ in today's property price equivalent after market growth and inflation correction."))
    ),
    p("The Financial Evolution Calculator takes into account all taxes, potential rental income losses during vacancies, available income tax deductions, mortgage and family/friend loan repayments, as well as property management and maintenance costs."),
    br(),
    hr(),



    h3("Total Asset Evolution"),
    p(paste0("This section shows how ", couple_name, "'s overall total asset value changes over time (Figure 1). Their net worth is the sum of their assets (cash, investments, property) minus debts (like mortgages or credit card debt).")),
    tags$ul(
      tags$li(tags$u("Long-term Trend:"), " In the long run, all options show growth, although the pace slows down after retirement."),
      tags$li(tags$u("Early-Year Differences:"),
              tags$ul(
                tags$li("Renters: Start off with positive assets that grow steadily."),
                tags$li("Homeowners: Start with negative value because of the mortgage and take about 9 years to break even."),
                tags$li("Landlords: Start with negative value because of the mortgage and take about 4 years to break even.")
              )),
      tags$li(tags$u("Late-life Stability:"), paste0("By retirement (around age ", rent_expected_year_retirement - 2025 + 30, " in ", rent_expected_year_retirement, "), all scenarios reach similar asset levels, showing long-term financial security. However, as homeowners, they reach retirement with significantly more asset value.")),
      tags$li(tags$u("Lump sums:"), paste0("Large incomes or expenses like inheritances don’t change the big picture too much in the homeowner scenario, but significanly impact the renters asset value evolution."))
    ),
    p("Broad-picture analysis of their financial evolution indicates a future of growth and no large discrepancy between scenarios, besides the higher value in the homeowner scenario. Importantly, all scenarios display stable total asset value after retirement, suggestion stable and secure retirement."),
    br(),
    p(strong("A - Renters")), renderUI(renderPlot(rent_AssetEvolution)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_AssetEvolution)),
    p(strong("C - Landlords")), renderUI(renderPlot(landlord_AssetEvolution)),
    p(em(paste0("Figure 1 - ", couple_name, ". Total asset evolution in three scenarios: renting (A), homeownership (B), and property investment (C)."))),
    br(),
    hr(),



    h3("Financial Metrics"),
    p(paste0("Next, we look at some key financial numbers that help us compare these scenarios with more granularity (Figure 2):")),

    tags$ul(
      tags$li(tags$u("Accumulated Investments (Yellow Line in figure 2)")),
      tags$ul(
        tags$li("Renters: Early and consistent investment growth, resulting in the highest accumulated value."),
        tags$li("Homeowners: No early growth until later in life when savings quickly pick up with the other scenarios."),
        tags$li("Landlords: Moderate initial growth, falling in between the other two scenarios.")
      ),
      tags$li(tags$u("Investment Timing")),
      tags$ul(
        tags$li("Renters: Steady contributions throughout their careers."),
        tags$li("Homeowners: About 95% of investments at retirement age accumulate in the decade before retirement (2056–2065)."),
        tags$li("Landlords: Balanced contributions with a slight early-career delay but steady throughout their careers.")
      ),
      tags$li(tags$u("Monthly Cash Flow (Blue Line in figure 2)"), " - Shows the balance between expenses (Red Line in figure 2) and income (Green Line in figure 2):"),
      tags$ul(
        tags$li("Renters: They enjoy a small positive cash flow from the beginning and throughout their careers."),
        tags$li("Homeowners: They often face negative cash flow early on and must use their emergency fund in several occasions."),
        tags$li("Landlords: They sometimes have negative cash flow but smaller than in the homeowner scenario.")
      ),
      tags$li(tags$u("Emergency Reserve (Orange Line in figure 2)"), " - Reflects financial resilience:"),
      tags$ul(
        tags$li("Renters: Their emergency fund stays constant."),
        tags$li("Homeowners: They have to use their emergency funds frequently while paying down their mortgage."),
        tags$li("Landlords: They occasionally use these funds, but not too much.")
      ),
      tags$li(tags$u("Mortgage Payoff (2053)"), " - In the homeowner scenario, paying off their mortgage provides a dramatic boost to monthly cash flow and accelerates investment growth. The effect is less visible for landlords, whose mortgage is smaller."),
      tags$li(tags$u("Empty Nest Effect"), " - As children leave home, expenses drop slightly, improving cash flow in all scenarios."),
      tags$li(tags$u("Retirement Transition"), paste0(" - By age ", rent_expected_year_retirement - 2025 + 30, " (", rent_expected_year_retirement, "), all scenarios show similar post-retirement income reduction. In all scenarios, withdraws from investments to sustain lifestyle (Purple Line in figure 2) are also necessary.")),
    ),
    p("Granular analysis of their finance metrics shows that, ", couple_name, " would have very similar financial lives in the renters and landlords scenarios, marked by the steady accumulation of investments from early on. The homeowner scenario, however, shows a very different journey. As homeowners, they struggle to make ends meet up until their mortgage is paid off, barely accumulating investments. Investments accumulation then skyrockets, allowing them to catch up with the other scenarios before retirement, and live retirement with much higher financial reserves."),
    br(),
    p(strong("A - Renters")), renderUI(renderPlot(rent_FinMetrics)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_FinMetrics)),
    p(strong("C - Landlords")), renderUI(renderPlot(landlord_FinMetrics)),
    p(em(paste0("Figure 2 - ", couple_name, ". Evolution of key financial metrics by scenario: renting (A), homeownership (B), and property investment (C)."))),
    br(),
    hr(),




    h3("Income and Expenses Components"),
    p(paste0("Breaking down ", couple_name, "'s income and expenses provides a detailed view on their finances (Figure 3).")),
    h5("Expense Breakdown"),
    tags$ul(
      tags$li(tags$u("Child-related costs:"), " Low in the first three decades compared to overall lifestyle expenses."),
      tags$li(tags$u("Housing expenses:"), tags$ul(
        tags$li(paste0("Renters: Rent makes up around 40% of their total expenses.")),
        tags$li(paste0("Homeowners: Mortgage payments and house costs represent roughly 40% of total expenses.")),
        tags$li(paste0("Landlords: Mortgage payments and house costs represent roughly 10% of total expenses, while rent makes up around 30%."))
      )),
      tags$li(tags$u("Living costs:"), " Overall living expenses remain steady, reflecting a consistent lifestyle.")
    ),
    h5("Income Breakdown"),
    tags$ul(
      tags$li(tags$u("Salary Income:")),
      tags$ul(
        tags$li("The main income source during working years."),
        tags$li("Accounts for 90–95% of total income early on, proportionally decreasing as investment returns increase.")
      ),
      tags$li(tags$u("Investment Returns:"), tags$ul(
        tags$li("Renters: They receive decent returns that become a meaningful income source by mid-career."),
        tags$li("Homeowners: They barely receive any returns until investing their inheritance. Returns only become meaningful after the mortgage is paid off."),
        tags$li("Landlords: They receive some returns that become a meaningful income source by mid-career.")
      )),
      tags$li(tags$u("Rental Income (Landlord scenario):"), tags$ul(
        tags$li("Contributes about 10% of their total income."),
        tags$li("This income grows with inflation and real estate market, becoming more important after retirement (20% of total income).")
      )),
      tags$li(tags$u("Post-retirement:"), tags$ul(
        tags$li("After retirement, income sources shift dramatically. In the homeowner scenario, capital gains and dividends become an important income source (30–40% of total income), with the rest coming from their reduced salaries. These have only a minor contribution to total income in the renters and landlords scenarios."),
        tags$li("As landlords, they have an extra income stream from their leased property.")
      ))
    ),
    p("The detailed view on their finances helps us better understand the causes behind some of the financial patterns they experience. For example, the impact of rent (or lack thereof in the homeowner scenario) for their finances and the importance of salary as their main source of income. The breakdown also highlights the differences between scenarios, particularly highlighting the importance of investment returns after retirement in the homeowner scenario."),
    br(),
    p(strong("A - Renters")), fluidRow(
      column(6, renderUI(renderPlot(rent_ExpensesComp))),
      column(6, renderUI(renderPlot(rent_IncomeComp)))
    ),
    p(strong("B - Homeowners")), fluidRow(
      column(6, renderUI(renderPlot(homeowner_ExpensesComp))),
      column(6, renderUI(renderPlot(homeowner_IncomeComp)))
    ),
    p(strong("C - Landlords")), fluidRow(
      column(6, renderUI(renderPlot(landlord_ExpensesComp))),
      column(6, renderUI(renderPlot(landlord_IncomeComp)))
    ),
    p(em(paste0("Figure 3 - ", couple_name, ". Detailed breakdown of expenses and income for renters (A), homeowners (B), and landlords (C)."))),
    br(),
    hr(),




    h3("Sensitivity Analysis"),
    h5("Testing Financial Resilience"),
    p("While these scenarios assume an ideal plan, a fundamental aspect of financial stability is resilience to unexpected changes. To put it to test, we perform sensitivity test to each key parameter independently, by varying its value by ±20% one at a time."),

    h5("Insensitive Parameters"),
    tags$ul(
      tags$li(strong("Minimal Impact Parameters"), " - Variations within ±20% have small effects on the long-term trajectory:"),
      tags$ul(
        tags$li(tags$u("Investment returns:"), " Minor influence, though slightly more impactful for renters."),
        tags$li(tags$u("Rent growth:"), " Changes in rent have some impact for them as tenants."),
        tags$li(tags$u("Property appreciation:"), " Affects net worth but not cash flow significantly."),
        tags$li(tags$u("Timing of purchasing:"), "A delay of a few years in purchasing has minimal impact."),
        tags$li(tags$u("Mortgage interest rate:"), "Interest rates on their mortgage mainly caps how much mortgage they can pay, and therefore the price of the property they can afford."),
        tags$li(tags$u("Rent growth (as landlords):"), " Changes in rent have minor impact for them as landlords.")
      )
    ),
    h5("Sensitive Parameters"),
    p("Beyond a certain threshold, small variations in some parameters lead to large-scale impacts on overall finances. Exceeding these thresholds can trigger a debt spiral and collapse, a situation where their debt is not repaid fast enough and generates even more debt."),
    tags$ul(
      tags$li(strong("Threshold Effect Parameters"), " - Critical values where financial stability rapidly deteriorates:"),
      tags$ul(
        tags$li(tags$u("Living costs:"), paste0("If living costs rise by more than ", format(round(0.05 * homeowner_living_style_costs), big.mark=","), "€ (a 5% jump), this would be enough to trigger a debt spiral.")),
        tags$li(tags$u("Career growth rate:"), " Keeping up promotions are necessary to sustain financial growth, specially in the homeowner scenario where they do not have secondary income sources."),
        tags$li(tags$u("Home purchase price:"), " In the homeowner scenario, overshooting the budget by more than 15% makes things much tougher in the long run.")
      )
    ),
    p("Sensitivity analysis then help us find out which parameters matter most in the long run and spot threshold points where a small change makes a big difference. In all scenarios, living costs and salary growth lead to largest impact to their financial evolution. This is a clear sign of the importance of these parameters in their finances. Not surprisingly, many people in similar situation tend to be careful with their living costs and sometimes work extra hours."),
    br(),
    p(strong("A - Renters")), renderUI(renderPlot(rent_SensitivityAnalysis,
                                                  height = rent_SensitivityAnalysis_height)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_SensitivityAnalysis,
                                                     height = homeowner_SensitivityAnalysis_height)),
    p(strong("C - Landlords")), renderUI(renderPlot(landlord_SensitivityAnalysis,
                                                    height = landlord_SensitivityAnalysis_height)),
    p(em(paste0("Figure 4 - ", couple_name, ". Sensitivity analysis of the total asset value in response to single-parameter variations."))),
    br(),
    hr(),





    h3("To Own or Not to Own a Home?"),
    div(
      class = "alert alert-info",
      p(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE_SCENARIOS)
    ),

    h5("Comparing the Risks and Rewards"),
    p(paste0("One common argument in favour of buying a home is that it provides a safe retirement. In fact, this is what we observed in the homeowner scenario as, once paid, the property they purchased early in their lives brought small expenses later during retirement.")),
    p(paste0("Both other scenarios also achieved a safe retirement, although with important differences. In today's price and corrected for inflation, they'd live in a retirement home valued at ", format(rent_home_value_today, big.mark=","), "€ as renters, at ", format(homeowner_home_value_today, big.mark=","), "€ as homeowners and at ", format(landlord_home_value_today, big.mark=","), "€ as landlords, probably reflecting property size, location and/or quality. They'd also have much higher savings invested in the homeowners scenario.")),
    p(paste0("Concentrating assets in a single property also limits diversification and increases reliance on this single investment. While having outstanding mortgage payments, losing their jobs, illnesses, or a market downturn pose significant risks. In some cases, they may even be forced to sell the property at a loss. Due to the high liquidity of savings, renters are better suited to overcome these risks.")),
    p(paste0("On the other hand, investing a smaller portion in a less expensive property and renting it out could yield a more balanced portfolio. However, being a landlord entails additional challenges, including tenant management, property maintenance, and complex tax situations.")),

    p("Let’s quickly compare the scenarios:"),
    tags$ul(
      tags$li(tags$u("Renter Profile"), " - Most flexibility with reduced headache:"),
      tags$ul(
        tags$li(strong("Superior liquidity"), " enabling diversified investments due to exposure to financial markets."),
        tags$li(strong("Geographic flexibility"), " with the ability to relocate if necessary. Very relevant in the search for better opportunities in another cities."),
        tags$li(strong("Fewer responsibilities"), " without property management responsibilities, allowing more time spent on what matters most."),
        tags$li(strong("Exposure to market-based rent increases"), " affecting housing costs somewhat unpredictably.")
      ),

      tags$li(tags$u("Homeownership Profile"), " - Commitment to one asset and one geographic area:"),
      tags$ul(
        tags$li(strong("High emotional value"), " as owning a home can foster a sense of belonging and pride due to personalization and local community ties."),
        tags$li(strong("Higher stress and less flexibility early on"), " while paying off the loan."),
        tags$li(strong("Reduced flexibility"), " in their careers due to the difficulty in relocating cities and high dependency on salaries to make ends meet.")
      ),
      tags$li(tags$u("Landlord Profile"), " - Mix of property ownership and rental income:"),
      tags$ul(
        tags$li(strong("Diversified income streams"), " through rental income and career earnings."),
        tags$li(strong("Moderate stress early on"), " during the first 5–7 years until savings build up."),
        tags$li(strong("Complex property management"), " including tenant relations, property upkeep, and more intricate tax situations that require additional administrative oversight. Less time for what really matters.")
      )
    ),
    br(),



    h5("Making the Best Choice"),
    p("To decide on the best scenario for this couple, we need to evaluate each option against their goals:"),
    tags$ul(
      tags$li(strong("Primary Goals:"), " A stable financial life, increased life quality, and early retirement without giving up their lifestyle."),
      tags$li(strong("Secondary Goals:"), " Fewer working hours, more options to donate or pass on wealth to the next generation.")
    ),
    p("Both scenarios have the potential to fulfill their financial goals and the best choice will ultimatelly depend on personal preferences: "),
    tags$ul(
      tags$li(tags$u("Financial Stability:"), " Renters (and landlords to a lesser extent) build up cash reserves earlier, while homeowners have to wait until the mortgage is paid off. Hence, for financial stability, delaying homeownership may be preferable."),
      tags$li(tags$u("Improving Life Quality:"), " Longer working hours could provide the desired extra income to raise their living quality. Indeed, many people in this situation tend to put on extra hours. However, investment gains that supplement income in the renters scenario (and partially also for landlords) might achieve the same goal."),
      tags$li(tags$u("Early Retirement Dream:"), " Renters (and landlords to a lesser extent) accumulate savings significantly faster, which might allow them to retire earlier instead of purchasing their retirement home.")
    ),
    p("Ultimately, the analysis shown here is static and does not take into account the dynamic nature of life (e.g.: working extra hours during harder years, or moving to a cheaper rent temporarily). It is important to consider that, regardless of the decision taken, one can still manipulate their reality to achieve their financial goals."),
    div(
      style = "background-color: #f8f9fa; border-left: 5px solid #007bff; padding: 15px; margin: 20px 0;",
      h4("Key Takeaway: Financial Flexibility"),
      p("The main difference is not just how much money they make over time, but the journey. As renters (and landlords) they enjoy greatest freedom throughout their 30s and 40s, potentially enabling them to:"),
      tags$ul(
        tags$li("Have the chance to switch jobs or move to different cities without too much hassle."),
        tags$li("Take longer breaks when needed, like an extended parental leave during important child development periods."),
        tags$li("Retire 3–5 years earlier, but without a retirement home."),
        tags$li("Be in a better position to enjoy charitable giving.")
      ),
      p(tags$u("Golden Handcuffs:"), " Homeowners might feel less freedom to making life changes due to mortgage obligations, especially when funds are tight in the early years.")
    ),
    hr(),
    br(),
    br()
  )
}
