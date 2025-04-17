scenario1Content <- function(is_mobile) {

  couple_name <- "Gene and Jean"

  main_scenario <- safelyLoadConfig(file.path("config", "templates", "inputs_low_wage_family_rent.yaml"))

  # Dynamically create variables from the inputs
  lapply(main_scenario, function(section) {
    if (is.null(section$inputs) && length(section) > 0) section <- section[[1]] # For properties
    lapply(section$inputs, function(input) {
      assign(input$id, input$value, envir = .GlobalEnv)
    }
    )
  })

  # Generate the figures for this scenario
  for (scenario in c("rent",
                     "rent_higher_salary", "rent_lower_rent",
                     "homeowner", "landlord")) {
    scenario_name <- paste0("low_wage_family_", scenario)
    config <- safelyLoadConfig(file.path("config", "templates",
                                         paste0("inputs_", scenario_name, ".yaml")))
    plot_data <- read.csv(file.path("article",
                                    paste0("calculations_", scenario_name, ".csv")))
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

    h2(paste0(couple_name, ", the poverty trap")),
    p(em(paste0(couple_name, " are a 30-years old poor family in Germany. Hard-working class, working jobs with low education requirements, forced to live away from city centres due to high living costs and the housing market."))),

    h3("Financial Goals"),
    tags$ul(
      tags$li(strong("Lifestyle"), "-", NAVTAB_CONTENT$SCENARIOS$LIFESTYLE_DESCRIPTION),
      tags$li(strong("Living Costs"), "-", "Because they spend as little as possible saving wherever they can, having more to spend on themselves would improve their life quality and that’s a main financial goal."),
      tags$li(strong("Work and Retirement"), "-", NAVTAB_CONTENT$SCENARIOS$LATE_LIFE_DESCRIPTION),
      tags$li(strong("Generational Wealth"), "-", NAVTAB_CONTENT$SCENARIOS$GENERATIONAL_WEALTH_DESCRIPTION),
      tags$li(strong("Donations"), "-", NAVTAB_CONTENT$SCENARIOS$EXTRA_CASH_DESCRIPTION)
    ),
    hr(),



    h3("Personal Finances Parameters"),
    tags$ul(
      tags$li(strong("Income"), paste0("- Their net income together is ", net_annual_income, "€/month. Working on consumer-based jobs, their incomes are expected to increase at an annual rate of ", salaries_growth_start_career, "%/year at the start of their careers (promotions, more education, more clients, more experience), and this rate slows down over time to half of that, as options for growth saturate.")),
      tags$li(strong("Living standards"), paste0("- They try to keep a simpler life, spending ", living_style_costs, "€ each month in groceries, clothing, commuting, entertainment, holidays and all other living costs. As life changes, the types of expenses are expected to change while the overall costs of living are expected to remain the same (after correcting for inflation). They have no room to further reduce these expenses.")),
      tags$li(strong("Retirement"), paste0("- They hope to retire at the age of ", as.character(as.numeric(expected_year_retirement) - 2025 + 30), " in ", expected_year_retirement, " and expect their salaries to drop by ", expected_salary_reduction_retirement, "%.")),
      tags$li(strong("Family Size"), paste0("- They are pregnant with their first child and plan to have a second one right after that.")),
      tags$li(strong("Housing"), paste0("- They rent a small 3-room flat in the outskirts of Berlin (", rent_month, "€ + ", fixed_housing_costs, "€/month extra expenses for housing). The rent is expected to grow as the real estate market grows (", rental_prices_growth, "%/year), while extra expenses raise as per the inflation (", fixed_housing_costs_change, "%/year). They could find a cheaper flat. However, due to their growing family, they need the extra space.")),
      tags$li(strong("Passive Investments"), paste0("- They invest all their savings in accumulating ETFs with ", expected_return_on_investment, "%/year gross return and will change it into safer investment options over time, receiving ", expected_conservative_return_on_investment, "%/year returns by retirement age. Any investment gains are taxed at ", capital_gains_tax_rate, "% in Germany upon withdraw and after considering Vorabpauschale tax that is charged every year.")),
      tags$li(strong("Taxes"), paste0("- They pay the lowest income tax in Germany (", income_tax, "% + solidarity surcharge tax) and no voluntary taxes to the church.")),
      tags$li(strong("Debts"), paste0("- Any time they go into debt (emergency reserve below zero), their only way of financial support is to contract a debt with a credit card while paying high interest rate (", interest_rate_cash_flow_debt, "%/year).")),
      tags$li(strong("Lump Sums"), paste0("- They expect to inherit some money from their parents (", lump_sum_1, "€, around ", lump_sum_1_year, "), but also to have other large expenses at some point (", lump_sum_2, "€, around ", lump_sum_2_year, ")."))
    ),
    hr(),



    h3("Total Asset Evolution"),
    p(paste0(couple_name, "’s total asset value remains mostly stable over their life time (Figure 1), with periods of growth, stagnation or even deline.")),
    p("Their situation is strongly affected by major lump sums (positive and negative) and shows strong improvement in the final years of their careers, assuming they manage to tightly control living costs throughout their lives."),
    p(paste0("Upon retirement at age of ", as.character(as.numeric(expected_year_retirement) - 2025 + 30), " in ", expected_year_retirement, ", their total asset value rapdly declines indicating that they’d be living a similar lifestyle and living costs but with the risk of not been able to sustain themselves at late life.")),
    p("They hold at advanced age no assets that could be passed on to their kids as generational wealth."),
    renderUI(renderPlot(rent_AssetEvolution)),
    p(em(paste0("Figure 1 - Scenario 1, ", couple_name, ". Total asset value evolution."))),
    hr(),



    h3("Financial Metrics"),
    p(paste0(couple_name, "’s main financial metrics reflect their stagnated financial situation with a lifestyle of cutting costs and saving wherever possible (Figure 2).")),
    p("A key observation is that their expenses (red line) slowly build up in the first 3 decades, while their income (green line) remains almost constant. This leads to a gradial switch in cash flow (blue line) balance."),
    p("In the first decade (2025-2035), positive cash flow drives accumulation of savings that are passively invested (Yellow line). However, over the following two decades (2036-2052), they must withdraw from their emergency reserve (orange line) and investments (purple line) to pay-off debts due to the negative cash flow."),
    p(paste0("The impact of punctual lump sums (", lump_sum_1, "€ in ", lump_sum_1_year, " and ", lump_sum_2, "€ in ", lump_sum_2_year, ") is significant, driving large changes.")),
    p("In the following decade (2053-2065), they return to a period of positive cash flow and saving."),
    p(paste0("Finally, from retirement on (", expected_year_retirement, "-2080), their cash flow is again negative and is offset by withdrawing from invested savings.")),
    renderUI(renderPlot(rent_FinMetrics)),
    p(em(paste0("Figure 2 - Scenario 1, ", couple_name, ". Evolution of main financial metrics."))),
    hr(),



    h3("Income and Expenses Components"),
    p(paste0("Decomposition of ", couple_name, "’s income and expenses paints a granular picture of their financial evolution.")),
    p("Living costs are constant throughout the years, except in the years when the kids are born, reflecting a sustained lifestyle (Figure 3A)."),
    p("Rent is by far their largest expenditure, representing between 60-70% of all expenses and slowly increasing over the years. Costs with family represent a significant expense during the first 3 decades and the fast increase leads to the switch in the cash flow balance observed above."),
    p("Salary is their main source of income throughout their lives as returns on passively invested savings is small and they have no other source of income (Figure 3B)."),
    p(strong("A")), renderUI(renderPlot(rent_ExpensesComp)),
    p(strong("B")), renderUI(renderPlot(rent_IncomeComp)),
    p(em(paste0("Figure 3 - Scenario 1, ", couple_name, ". Detailed view of the financial components in their expenses (A) and income (B)."))),
    hr(),



    h3("Sensitivity Analysis"),
    p("The scenario simulated above assumes that everything goes according to a plan. However, a fundamental aspect of having a stable financial life is robustness. That is, the ability to resist to unforeseen life changes."),
    p(paste0("Sensitivity analysis identifies factors to which finances are most sensitive by iteratively repeating the simulation while slightly varying a single parameters per iteration. This analysis shows that changing key parameters by up to 20% margin has significant impacts on the couple’s financial evolution.")),
    p(paste0("For example, if this family spends 40€ more than the expected ", living_style_costs, "€/month (5% variation to their monthly living costs) while keeping all other parameters the same, they will have accumulated zero total asset value by retirement age and are not able to sustain their lifestyle (Figure 4, top left plot). Spending any more than this could lead to debt collapse – a situation where their debt is not paid off fast enough and generates even more debt, exponentially increasing itself. Therefore, tightly controlling their living costs is of great importance for ", couple_name, ", as they have a tiny safety margin.")),
    p(paste0("To avoid debt collapse, similar analysis on their salary increase over time leads to the conclusion that ", couple_name, "’s must also keep working hard to achieve the minimum required salary growth (above 2.5%). Similarly, they must also negotiate well their rental price increase rate keeping bellow 3%, if possible. Failing to achieve these hallmarks puts them at risk of debt collapse.")),
    p("Although with less extreme consequences, choosing good investments for their money also impacts their financial life, specially upon retirement."),
    renderUI(renderPlot(rent_SensitivityAnalysis, height = rent_SensitivityAnalysis_height)),
    p(em(paste0("Figure 4 - Scenario 1, ", couple_name, ". Sensitivity analysis on total asset value of single parameter perturbations."))),
    hr(),



    h3(paste0("How can ", couple_name, " improve their financial situation?")),
    p(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE_SCENARIOS),

    h4("Guided by Financial Goals"),
    p(paste0("To improve the financial situation of ", couple_name, " we must keep in mind their goals: to have a stable financial life, to enhance their live quality by increasing expenses and to retire at later age while maintaining their lifestyle focused on family, friends and hobbies.")),
    p(paste0(couple_name,"’s financial reality has overarching impacts over their life quality. Given the current market prices in Berlin, the property they rent for a family with 2 kids (1000€/month + 120€/month extra costs) is likely distant from their workplaces, family and friends. They also live in constant financial stress as they have a real risk of debt collapse.")),
    p("They already save every penny, so there is not much room for reducing their expenses further without impacting their health, which would lead to reduced income over time."),
    p("Rental increases by landlords due to real estate market development or gentrification leads to major financial stress, possibly forcing them to look for a cheaper alternative. Here, living in government-backed housing projects could be a good alternative to the private housing market."),
    p("Also, living far from city centres makes it difficult to get better education and opportunities to improve their salaries. The many hours commuting may also negatively affect their wellbeing and consequently their performance at work."),
    hr(),

    h4("Aligning Financial Goals and Decisions"),
    p(paste0("What could they have done differently to better align their financial goals and decisions? The Financial Evolution Calculator can be used to test multiple scenarios. For example, we can re-simulate ", couple_name, "’s scenario to identify critical factors for improving their financial situation (Figure 5).")),
    p(paste0("For example, increasing their current salaries by 50€ each would strongly impact their financial evolution (Figure 5A). Not surprisingly, people in similar situations to ", couple_name, " often work extra hours. Although impactful, this approach might not be a sustainable solution. Focusing on improving skills set and fighting for better salaries could have higher impact long-term.")),
    p("Similarly, reducing their current rent by 50€ would also improve their situation significantly (Figure 5B). Not surprisingly, people in this situation tend to move away from city centres into peripheries."),
    p(strong("A")), renderUI(renderPlot(rent_higher_salary_AssetEvolution)),
    p(strong("B")), renderUI(renderPlot(rent_lower_rent_AssetEvolution)),
    p(em(paste0("Figure 5 - Scenario 1, ", couple_name, ". Total asset value evolution in simulations of scenarios with single parameter variations. A) Salary increased from 2400€ to 2500€. B) Rent reduced from 1000€ to 950€. C) Salary increased and rent reduced."))),
    hr(),

    h4("Going Beyond Initial Goals"),
    p(paste0("This analysis points to a possible but difficult financial improvement for ", couple_name, " by increasing salaries and reducing rent. This highlights how people in the lowest income range are vulnerable in society, at ther mercy of the employees and the real estate market.")),
    p("However, there is another alternative to consider: becoming landlords. There seems to be a general misconception that becoming landlords is something only rich people can do. Although more difficult to obtain a bank loan when one’s salaries are low, with help of family and friends it could be possible. But is it a good idea for this couple?"),
    p(paste0("Let’s re-simulate a second scenario for ", couple_name, ". Instead of trying to save and invest passively, they invest actively in real estate by purchasing a property (200.000€) with help of family and friends. Also, instead of moving in, they stay in their rental apartment (1000€/month). Their property is then leased to someone else for 500€/month, following the same rental contract they have on their own rent (3% increase per year).")),
    p("The Financial Evolution Calculator considers all taxes they’d need to pay, risk of not having a tenant, tax deductible costs, mortgage for the investment property, property depreciation, management fees and maintenance costs. All these factors considered, their accumulated asset value at retirement age as landlords would be seven-fold higher - 550.000€ as landlords and 80.000€ when only renting their home (Figure 6A). They would not experience debt (Figure 6B) and would be financially resilient due to increased savings (Figure 6C)."),
    p("With the extra income source, they could boost their living costs to enhance life quality and even pursue their secondary financial goals, like donating to causes, working less and passing on generational wealth."),
    p("Alternatively, they could have saved enough money by retirement age to purchase a second property – evaluated at 200.000€ in today’s price. That is, without a bank loan, without selling their leased property, and still having savings in their bank account after the purchase to sustain their lifestyle in retirement!"),
    p(strong("A")), renderUI(renderPlot(landlord_AssetEvolution)),
    p(strong("B")), renderUI(renderPlot(landlord_FinMetrics)),
    p(strong("C")), renderUI(renderPlot(landlord_SensitivityAnalysis, height = landlord_SensitivityAnalysis_height)),
    p(em(paste0("Figure 6 - Scenario 1, ", couple_name, ". Scenario simulation leasing the property purchased in 2025. Total asset value evolution (A), evolution of main financial metrics (B), and sensitivity analysis on total asset value of single parameter perturbations (C)."))),

    h4("Detailed Comparison"),
    p(strong("Table 1A - Initial years living as renters")),
    div(style = "width: 100%; max-width: 100%; overflow-x: auto; margin-bottom: 1.5rem; font-size: 12px;",
        renderDT({
          rent_table_for_vis})
    ),
    br(),
    p(strong("Table 1B - Initial years living as landlord")),
    div(style = "width: 100%; max-width: 100%; overflow-x: auto; margin-bottom: 1.5rem; font-size: 12px;",
        renderDT({
          landlord_table_for_vis})
    ),
    br(),
    p("How is this possible? It might come as a surprise, but it’s really just numbers. By becoming landlords, they pay their own rent (1000€/month) and house expenses (110€/year) plus the bank loan (1000€/month) and loan from friends and family (200€/month). House expenses for the investment property are paid by the tenant."),
    p("Altogether, their housing expenses (1000 + 110 + 1000 + 200= 2310€/month) in the first years are barely covered by the leftover salary after discounting living costs plus the lease income (2400 – 800 + 500 = 2100€/month). Income tax deductions from expenses with the investment property (100€/month) help significantly."),
    p("The secret is time. While their incomes and expenses increase every year, the mortgage is fixed at least until refinancing 5-20 years later (Figure 6B). Every year their cashflow becomes more positive and, while all their available cash is re-directed to pay the flat in the first years, it slowly builds savings."),
    p("From the investment perspective, they diversified their assets over the years, increasing financial robustness. For example, their own rent will increase every year but, so long as their leased property increases rent at a similar rate, they are immune to real estate market changes (Figure 6C)."),
    p("Also, their income does not depend only on their salaries anymore, as they receive lease. Been less dependent on salary means that they could work less and try less hard for promotions. Although, they still live a restricted life saving every penny and higher salaries could still help."),
    p("The downside of becoming a landlord is, of course, the increased complexity. They’d need to deal with tenants or property management agencies, repair and management companies and more taxes. This extra headache holds back people from pursuing such investments, even when the numbers clearly point to this alternative as a valid solution."),
    br(),
    br(),
  )
}
