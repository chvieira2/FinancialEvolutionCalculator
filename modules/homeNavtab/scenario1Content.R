scenario1Content <- function(is_mobile) {

  couple_name <- "Gene and Jean"

  # Generate the figures for this scenario
  for (scenario in c("rent", "homeowner")) {
    scenario_name <- paste0("low_wage_family_", scenario)
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

    if (scenario %in% c("rent", "homeowner")) {
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
    p(em(paste0(couple_name, " are a 30-years-old low-income family in Berlin, Germany. With low salaries in Germany, they have not built savings over their careers. They are forced to live away from city centres due to high living costs and the housing market. They wonder whether transitioning from renting to homeownership is a financially sound decision."))),

    # Financial Goals
    h3("Financial Goals"),
    tags$ul(
      tags$li(tags$u("Lifestyle"), " - ", NAVTAB_CONTENT$SCENARIOS$LIFESTYLE_DESCRIPTION),
      tags$li(tags$u("Living Costs"), " - Because they spend as little as possible and save wherever they can, having more to spend on themselves would improve their life quality."),
      tags$li(tags$u("Work and Retirement"), " - Reducing working hours to spend more time with loved ones and retiring as soon as possible are desirable goals, but secondary to achieving financial stability and improving their life quality."),
      tags$li(tags$u("Generational Wealth"), " - ", NAVTAB_CONTENT$SCENARIOS$GENERATIONAL_WEALTH_DESCRIPTION),
      tags$li(tags$u("Donations"), " - ", NAVTAB_CONTENT$SCENARIOS$EXTRA_CASH_DESCRIPTION)
    ),
    hr(),

    h3("Personal Finances Parameters"),
    tags$ul(
      tags$li(tags$u("Income"), paste0(" - Their combined net salary after taxes and insurances (e.g.: health insurance) is ", format(rent_net_annual_income, big.mark=","), "€ per month. In the early years, their earnings grow at around ", rent_salaries_growth_start_career, "% each year due to promotions, extra training, and experience. Later on, growth slows down as opportunities become fewer.")),
      tags$li(tags$u("Living standards"), paste0(" - They try their best to keep living costs low, spending as little as " , format(rent_living_style_costs , big.mark=","), "€ each month on groceries, clothing, commuting, entertainment, holidays, and other needs. While what they spend on might change over the years, the overall monthly cost stays fairly constant (after adjusting for inflation). They have no room to further reduce these expenses.")),
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
      tags$li(tags$u("Renters"), paste0(" - They continue renting an old, 3-room flat in Berlin (valued at roughly ", format(round(5000/15*rent_rent_month, -3), big.mark=",", scientific = FALSE), "€) in the outskirts of the city for a monthly rent of ", format(rent_rent_month, big.mark=","), "€ plus an additional ", format(rent_fixed_housing_costs, big.mark=","), "€/month in fixed charges. They expect rent to increase over time (", rent_rental_prices_growth, "% annually).")),
      tags$li(tags$u("Homeowners"), paste0(" - They purchase a 3-room flat outside of Berlin for ", format(homeowner_home_value_today, big.mark=","),"€. The property is located far from the city center and with little access to public transportation. As initial capital to secure the cheapest mortgage (", homeowner_home_initial_interest_rate, "% per year interest rate, repayment rate of ", homeowner_home_principal_repayment_rate, "%), they use all available savings (besides their emergency reserve) and borrow from family or friends ", homeowner_home_loan_family_friends, "€, to be paid in ", homeowner_home_duration_payback_family_friends, " years. They do not amortize the loan over the years, instead choosing to invest savings passively. The property they own will appreciate over time (", homeowner_home_value_growth, "% per year).")),
      tags$li(tags$u("Landlords"), paste0(" - Unfortunately, becoming landlords is not financially realistic for ", couple_name, "."))
    ),
    p("The Financial Evolution Calculator takes into account all taxes, potential rental income losses during vacancies, available income tax deductions, mortgage and family/friend loan repayments, as well as property management and maintenance costs."),
    br(),
    hr(),



    h3("Total Asset Evolution"),
    p(paste0("This section shows how ", couple_name, "'s overall total asset value changes over time (Figure 1). Their net worth is the sum of their assets (cash, investments, property) minus debts (like mortgages or credit card debt).")),
    tags$ul(
      tags$li(tags$u("Long-term Trend:"), " In the long run, becoming homeowners is a clear winner scenario, leading to steady value accumulation over the years."),
      tags$li(tags$u("Early-Year Differences:"),
              tags$ul(
                tags$li("Renters: Start off with positive assets that grow steadily in the first decade, but decreases also steadly in the following 2 decades. Asset value again starts increasing in the decade prior to retirement, but sharply decreases upon retirement."),
                tags$li("Homeowners: Start with negative value because of the mortgage and take about 9 years to break even.")
              )),
      tags$li(tags$u("Late-life Stability:"), paste0("By retirement (around age ", rent_expected_year_retirement - 2025 + 30, " in ", rent_expected_year_retirement, "), the home owner scenario reach significant asset levels, showing long-term financial security. However, as renters, they reach retirement age with some assets value but rapdily lose it over the following years, making it a difficult retirement.")),
      tags$li(tags$u("Lump sums:"), paste0("Large incomes or expenses like inheritances don’t change the big picture too much in the homeowner scenario, but significanly impact the renters asset value evolution."))
    ),
    p("The large discrepancy between scenarios is visible in the evolution of their total asset value. As renters, their finances is at best stagnated while steadly increasing in the homeowner scenario."),
    br(),
    p(strong("A - Renters")), renderUI(renderPlot(rent_AssetEvolution)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_AssetEvolution)),
    p(em(paste0("Figure 1 - ", couple_name, ". Total asset evolution in three scenarios: renting (A) and homeownership (B)."))),
    br(),
    hr(),



    h3("Financial Metrics"),
    p(paste0("Next, we look at some key financial numbers that help us compare these scenarios with more granularity (Figure 2):")),
    tags$ul(
      tags$li(tags$u("Accumulated Investments (Yellow Line in figure 2)")),
      tags$ul(
        tags$li("Renters: Early and consistent investment growth in the first decade, but diminished also steadly in the following 2 decades. Investment accumulates again in the decade prior to retirement, but sharply decreases upon retirement."),
        tags$li("Homeowners: No accumulation until later in life when savings quickly pick up.")
      ),
      tags$li(tags$u("Investment Timing")),
      tags$ul(
        tags$li("Renters: Variable contributions throughout their careers."),
        tags$li("Homeowners: About 95% of investments at retirement age accumulate in the decade before retirement (2056–2065).")
      ),
      tags$li(tags$u("Monthly Cash Flow (Blue Line in figure 2)"), " - Shows the balance between expenses (Red Line in figure 2) and income (Green Line in figure 2):"),
      tags$ul(
        tags$li("Both scenarios enjoy a small positive cash flow in the first decade (2025-2036), but struggle until 2053 to make ends meet and must use their emergency fund in several occasions."),
        tags$li("The bump in cash flow originating from their inheritance prevents them from completly using up emergency reserves and entering debt."),
        tags$li("Cash flow remains positive in both scenarios after that (2053-2065), although it gradually decreases in the renters scenario."),
        tags$li("During retirement (2065-), the cash flow in the homeowner scenario is significantly higher than in the renters scenario."),
      ),
      tags$li(tags$u("Emergency Reserve (Orange Line in figure 2)"), " - Reflects financial resilience:"),
      tags$ul(
        tags$li("In both scenarios, their emergency reserve is positive in the first decade (2025-2036), but drops to zero in the following 2 decades, just to be replenished again in the decade before retirement. The main difference is after retirement, when emergency funds are completly used up in the renters scenario, while remaining constant in the homeowner scenario."),
      ),
      tags$li(tags$u("Mortgage Payoff (2053)"), " - In the homeowner scenario, paying off their mortgage provides a dramatic boost to monthly cash flow and accelerates investment growth."),
      tags$li(tags$u("Empty Nest Effect"), " - As children leave home, expenses drop slightly, improving cash flow in all scenarios."),
      tags$li(tags$u("Retirement Transition"), paste0(" - By age ", rent_expected_year_retirement - 2025 + 30, " (", rent_expected_year_retirement, "), all scenarios show similar post-retirement income reduction. In the renters scenario withdraws from investments to sustain lifestyle (Purple Line in figure 2) are also necessary."))
    ),
    p("Granular analysis of their finance metrics shows that up until retirement, ", couple_name, " would have very similar financial lives. After retirement, however, the homeowner scenario shows a much more stable and positive financial life with continued investments growth, while the renters scenario shows the risk that they may end up without savings and reserves at some point in late life."),
    br(),
    p(strong("A - Renters")), renderUI(renderPlot(rent_FinMetrics)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_FinMetrics)),
    p(em(paste0("Figure 2 - ", couple_name, ". Evolution of key financial metrics by scenario: renting (A) and homeownership (B)."))),
    br(),
    hr(),




    h3("Income and Expenses Components"),
    p(paste0("Breaking down ", couple_name, "'s income and expenses provides a detailed view on their finances (Figure 3).")),
    h5("Expense Breakdown"),
    tags$ul(
      tags$li(tags$u("Child-related costs:"), " Significant in the first three decades compared to overall lifestyle expenses, representing up to 30% of their total expenses."),
      tags$li(tags$u("Housing expenses:"), tags$ul(
        tags$li(paste0("Renters: Rent makes up around 60% of their total expenses.")),
        tags$li(paste0("Homeowners: Mortgage payments and house costs represent roughly 60% of total expenses."))
      )),
      tags$li(tags$u("Living costs:"), " Overall living expenses remain steady, reflecting a consistent lifestyle.")
    ),
    h5("Income Breakdown"),
    tags$ul(
      tags$li(tags$u("Salary Income:")),
      tags$ul(
        tags$li("The main income source during working years."),
        tags$li("Accounts for 90–95% of total income throughout their lives, except in the homeowner scenario when capital gains during their investments becomes a significant income source.")
      ),
      tags$li(
        tags$u("Investment Returns:"),
        tags$ul(
          tags$li("They barely receive any returns until investing their inheritance. Returns only become meaningful after the mortgage is paid down and they accumulate enough savings.")
        )),
      tags$li(tags$u("Post-retirement:"), tags$ul(
        tags$li("After retirement, income sources shift dramatically. In the homeowner scenario, capital gains and dividends become an important income source (20–30% of total income), with the rest coming from their reduced salaries. These have only a minor contribution to total income in the renters scenario.")
      ))
    ),
    p("The detailed view on their finances helps us better understand the causes behind some of the financial patterns they experience. For example, the impact of rent (or lack thereof in the homeowner scenario) for their finances and the importance of salary as their main source of income. The breakdown also highlights the differences between the two scenarios, particularly highlighting the importance of investment returns after retirement."),
    br(),
    p(strong("A - Renters")), fluidRow(
      column(6, renderUI(renderPlot(rent_ExpensesComp))),
      column(6, renderUI(renderPlot(rent_IncomeComp)))
    ),
    p(strong("B - Homeowners")), fluidRow(
      column(6, renderUI(renderPlot(homeowner_ExpensesComp))),
      column(6, renderUI(renderPlot(homeowner_IncomeComp)))
    ),
    p(em(paste0("Figure 3 - ", couple_name, ". Detailed breakdown of expenses and income for renters (A) and homeowners (B)."))),
    br(),
    hr(),




    h3("Sensitivity Analysis"),
    h5("Testing Financial Resilience"),
    p("While these scenarios assume an ideal plan, a fundamental aspect of financial stability is resilience to unexpected changes. To put it to test, we perform sensitivity test to each key parameter independently, by varying its value by ±20% one at a time. With that we hope to find out which changes matter most in the long run and spot threshold points where a small change makes a big difference."),

    h5("Insensitive Parameters in homeowner scenario"),
    p("In the renters scenario, ", couple_name, " are sensitive to minor variations to all parameters tested. This highlights how people in the lowest economical group are financially vulnerable."),
    tags$ul(
      tags$li(strong("Minimal Impact Parameters (homeowner scenario)"), " - Variations within ±20% have small effects on the long-term trajectory:"),
      tags$ul(
        tags$li(tags$u("Investment returns:"), " Minor influence, though slightly more impactful for renters."),
        tags$li(tags$u("Rent growth:"), " Changes in rent have some impact for them as tenants."),
        tags$li(tags$u("Property appreciation:"), " Affects net worth but not cash flow significantly.")
      )
    ),
    h5("Sensitive Parameters"),
    p("Beyond a certain threshold, small variations in some parameters lead to large-scale impacts on overall finances. Exceeding these thresholds can trigger a debt spiral and collapse, a situation where their debt is not repaid fast enough and generates even more debt."),
    tags$ul(
      tags$li(strong("Threshold Effect Parameters (homeowner scenario)"), " - Critical values where financial stability rapidly deteriorates:"),
      tags$ul(
        tags$li(tags$u("Living costs:"), paste0("If living costs rise by more than ", format(round(0.05 * homeowner_living_style_costs), big.mark=","), "€ (a 5% jump), this would be enough to trigger a debt spiral.")),
        tags$li(tags$u("Career growth rate:"), " Keeping up promotions are necessary to sustain financial growth."),
        tags$li(tags$u("Home purchase price:"), " Overshooting the budget by more than 15% makes things much tougher in the long run."),
        tags$li(tags$u("Timing of purchasing:"), "A delay of a few years in purchasing has significant impact."),
        tags$li(tags$u("Mortgage interest rate:"), "Interest rates on their mortgage mainly caps how much mortgage they can pay, and therefore the price of the property they can afford.")
      )
    ),
    p("Sensitivity analysis then help us find out which parameters matter most in the long run and spot threshold points where a small change makes a big difference. In all scenarios, living costs and salary growth, and rent growth lead to largest impact to their financial evolution. This is a clear sign of the importance of these parameters in their finances. Not surprisingly, many people in similar situation tend to be very careful with their living costs, work extra hours and move away from city centers for cheaper rents."),
    br(),
    p(strong("A - Renters")), renderUI(renderPlot(rent_SensitivityAnalysis,
                                                  height = rent_SensitivityAnalysis_height)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_SensitivityAnalysis,
                                                     height = homeowner_SensitivityAnalysis_height)),
    p(em(paste0("Figure 4 - ", couple_name, ". Sensitivity analysis of the total asset value in response to single-parameter variations."))),
    br(),
    hr(),

    h3("To Own or Not to Own a Home?"),
    div(
      class = "alert alert-info",
      p(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE_SCENARIOS)
    ),

    h5("Comparing the Risks and Rewards"),
    p(paste0("One common argument in favour of buying a home is that it provides a safe retirement. In fact, this is what we observed in the homeowner scenario as, once paid, the property they purchased early in their lives brought small expenses later during retirement. Living on a rent for their whole lives has a large risk of not been able to main their lifestyle during retirement.")),
    p(paste0("Concentrating assets in a single property also limits diversification and increases reliance on this single investment. While having outstanding mortgage payments, losing their jobs, illnesses, or a market downturn pose significant risks. In some cases, they may even be forced to sell the property at a loss. However, these fears are not reduced in the renters scenario as they cannot accumulate significant savings to create a safe net.")),

    p("Let’s quickly compare the scenarios:"),
    tags$ul(
      tags$li(tags$u("Renter Profile"), " - Most flexibility with reduced headache:"),
      tags$ul(
        tags$li(strong("Geographic flexibility"), " with the ability to relocate if necessary. Very relevant in the search for better opportunities in another cities."),
        tags$li(strong("Fewer responsibilities"), " without property management responsibilities, allowing more time spent on what matters most."),
        tags$li(strong("Exposure to market-based rent increases"), " affecting housing costs somewhat unpredictably.")
      ),

      tags$li(tags$u("Homeownership Profile"), " - Commitment to one asset and one geographic area:"),
      tags$ul(
        tags$li(strong("High emotional value"), " as owning a home can foster a sense of belonging and pride due to personalization and local community ties."),
        tags$li(strong("Possibility for stable financial life and safer retirement"), " once they manage to payoff their mortgage."),
        tags$li(strong("Reduced flexibility"), " in their careers due to the difficulty in relocating cities and high dependency on salaries to make ends meet.")
      )
    ),
    br(),

    h5("Making the Best Choice"),
    p("To decide on the best scenario for this couple, we need to evaluate each option against their goals:"),
    tags$ul(
      tags$li(strong("Primary Goals:"), " A stable financial life and increased life quality."),
      tags$li(strong("Secondary Goals:"), " Fewer working hours, earlier retirement, more options to donate or pass on wealth to the next generation.")
    ),
    p("Both scenarios have the potential to fulfill their financial goals and the best choice will ultimatelly depend on personal preferences: "),
    tags$ul(
      tags$li(tags$u("Financial Stability:"), " In both scenarios, financial stability is tricky. In the homeowner scenario they achieve secure retirement with significant savings. Instead, they could have bought a cheaper property, with lower mortgage payments, and accumulated savings earlier. That would have greatly improved their financial stability."),
      tags$li(tags$u("Improving Life Quality:"), " Longer working hours could provide the desired extra income to raise their living quality. Indeed, many people in this situation tend to put on extra hours at work in both scenarios."),
      tags$li(tags$u("Early Retirement Dream:"), " As homeowners, they reach retirement with significant amount of investments accumulated. They could have instead retired earlier with less investments and still live a secure retirement.")
    ),
    p("Ultimately, the analysis shown here is static and does not take into account the dynamic nature of life (e.g.: working extra hours during harder years, or moving to a cheaper rent temporarily). It is important to consider that, regardless of the decision taken, one can still manipulate their reality to achieve their financial goals."),
    hr(),
    br(),
    br()
  )
}
