scenario3Content <- function(is_mobile) {

  couple_name <- "Jack and Beck"

  main_scenario <- safelyLoadConfig(file.path("config", "templates", "inputs_high_wage_family_homeowner.yaml"))

  # Dynamically create variables from the inputs
  lapply(main_scenario, function(section) {
    if (is.null(section$inputs) && length(section) > 0) section <- section[[1]] # For properties
    lapply(section$inputs, function(input) {
      assign(input$id, input$value, envir = .GlobalEnv)
    }
    )
  })

  landlord_scenario <- safelyLoadConfig(file.path("config", "templates", "inputs_high_wage_family_landlord.yaml"))
  lapply(landlord_scenario, function(section) {
    if (is.null(section$inputs) && length(section) > 0) section <- section[[1]] # For properties
    lapply(section$inputs, function(input) {
      assign(paste0("landlord_", input$id), input$value, envir = .GlobalEnv)
    }
    )
  })

  # Generate the figures for this scenario
  for (scenario in c("rent",
                     "homeowner", "landlord")) {
    scenario_name <- paste0("high_wage_family_", scenario)
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

    h2(paste0(couple_name, ", early retirement")),
    p(em(paste0(couple_name, " are a 30-years old high-income family in Germany. They work on knowledge jobs requiring high level of education. Their salaries depend mostly on their education rather than their performance. Their income is among the highest in Germany, but they are still part of the working class and do not live off inherited assets. Their salaries allow them to save and buy a property in Berlin not far from the city centre."))),

    h3("Financial Goals"),
    tags$ul(
      tags$li(strong("Lifestyle"), "-", NAVTAB_CONTENT$SCENARIOS$LIFESTYLE_DESCRIPTION),
      tags$li(strong("Living Costs"), "-", "Because their monthly living expenses already provide them with a comfortable life, having more wealth wouldn't significantly improve their life quality and isn't a primary goal."),
      tags$li(strong("Work and Retirement"), "-", NAVTAB_CONTENT$SCENARIOS$LATE_LIFE_DESCRIPTION),
      tags$li(strong("Generational Wealth"), "-", NAVTAB_CONTENT$SCENARIOS$GENERATIONAL_WEALTH_DESCRIPTION),
      tags$li(strong("Donations"), "-", NAVTAB_CONTENT$SCENARIOS$EXTRA_CASH_DESCRIPTION)
    ),
    hr(),



    h3("Personal Finances Parameters"),
    tags$ul(
      tags$li(strong("Income"), paste0("- Their net income together is ", format(net_annual_income, big.mark=","), "€/month. Their incomes are expected to increase at an annual rate of ", salaries_growth_start_career, "%/year at the start of their careers (promotions, more education, more clients, more experience), and this rate slows down over time to half of that, as options for growth saturate.")),
      tags$li(strong("Living standards"), paste0("- They don't care much about cutting costs and live a comfortable life, spending ", format(living_style_costs , big.mark=","), "€ each month in groceries, clothing, commuting, entertainment, holidays and all other living costs. As life changes, the types of expenses are expected to change while the overall costs of living are expected to remain the same (after correcting for inflation). They have no room to further reduce these expenses.")),
      tags$li(strong("Retirement"), paste0("- They hope to retire at the age of ", as.character(as.numeric(expected_year_retirement) - 2025 + 30), " in ", expected_year_retirement, ".  Despite having private pension schemes included in their living costs, the drop in salary will still be significant (", expected_salary_reduction_retirement, "%), given their high salaries.")),
      tags$li(strong("Family Size"), paste0("- They are pregnant with their first child and plan to have a second one right after that.")),
      tags$li(strong("Housing"), paste0("- They used to rent a big 3-room flat in Berlin (", format(rent_month, big.mark=","), "€ + ", format(fixed_housing_costs, big.mark=","), "€/month extra expenses for housing) but just recently bought a large flat in Berlin for ", format(value_today, big.mark=","),"€, not in the city centre but also not far. For that, they contracted a mortgage from the bank (", initial_interest_rate, "%/year interest rate, repayment rate of ", principal_repayment_rate, "%) and used all their savings besides their emergency reserve, but did not need to borrow money from their families and friends. They expect this property to increase in value over time (", value_growth, "%/year). They will not use positive yearly cash flow to amortize the loan, investing it passively instead.")),
      tags$li(strong("Passive Investments"), paste0("- They invest all their savings in accumulating ETFs with ", expected_return_on_investment, "%/year gross return and will change it into safer investment options over time, receiving ", expected_conservative_return_on_investment, "%/year returns by retirement age. Any investment gains are taxed at ", capital_gains_tax_rate, "% in Germany upon withdraw and after considering Vorabpauschale tax that is charged every year.")),
      tags$li(strong("Taxes"), paste0("- They pay the lowest income tax in Germany (", income_tax, "% + solidarity surcharge tax) and no voluntary taxes to the church.")),
      tags$li(strong("Debts"), paste0("- Any time they go into debt (emergency reserve below zero), their only way of financial support is to contract a debt with a credit card while paying high interest rate (", interest_rate_cash_flow_debt, "%/year).")),
      tags$li(strong("Lump Sums"), paste0("- They expect to inherit some money from their parents (", format(lump_sum_1, big.mark=","), "€, around ", lump_sum_1_year, "), but also to have other large expenses at some point (", format(lump_sum_2, big.mark=","), "€, around ", lump_sum_2_year, ")."))
    ),
    hr(),



    h3("Total Asset Evolution"),
    p(paste0("As ", couple_name, " just bought a home with a loan from the bank, their total asset value is negative but grows over the years (Figure 1). It takes 9 years for their total asset value to reach zero.")),
    p("By the time major lump sums are expected to arrive (positive and negative), they will have accumulated enough asset value that these will not have a significant impact."),
    p(paste0("Upon retirement at age of ", as.character(as.numeric(expected_year_retirement) - 2025 + 30), " in ", expected_year_retirement, ", their accumulated asset value decreases slightly, indicating that they'd be living stable financial lives.")),
    p("They hold until advanced age a significant total asset value that could be passed on to their kids as generational wealth."),
    renderUI(renderPlot(homeowner_AssetEvolution)),
    p(em(paste0("Figure 1 - Scenario 3, ", couple_name, ". Total asset value evolution."))),
    hr(),



    h3("Financial Metrics"),
    p(paste0(couple_name, "'s main financial metrics are marked by the purchase of their home property early in their lives.")),
    p("In the first five years (2025-2030), they balance total expenses (red line) and income (green line) well, having no extra savings to invest passively and relying on their emergency reserve (orange line) whenever necessary to cover expenses (Figure 2)."),
    p(paste0("In the following years, their income surpasses expenses, and the positive cash flow (blue line) is used to build savings invested passively (yellow line). The inheritance, although not significantly impacting the total value asset, adds to their savings.")),
    p("Significant reduction in their expenses can be observed around when they finally pay-off their home (2053). The positive cash flow resulting from vanishing mortgage expenses boosts the pace in which they've been accumulating savings."),
    p(paste0("Upon retirement they experience  negative cash flow duethe drop in their salaries. This cash flow gap is however small and is easily offset by withdrawing from their passive investments (purple line).")),
    p("They live a comfortable lifestyle throughout retirement, with a significant amount of money invested (>200,000€) and a property to live in."),
    renderUI(renderPlot(homeowner_FinMetrics)),
    p(em(paste0("Figure 2 - Scenario 3, ", couple_name, ". Evolution of main financial metrics."))),
    hr(),



    h3("Income and Expenses Components"),
    p(paste0("Decomposition of ", couple_name, "'s income and expenses help paint a better picture of their financial evolution."),
      p("Their living costs are constant throughout the years, except when the kids are born, reflecting a sustained life quality (Figure 3A).")),
    p("In the first three decades of their lives, expenses with kids are small relative to their own lifestyle expenses, while mortgage – composed of principal and interest payments – is significant and represents around 50% of their total expenses."),
    p("Upon paying off the property around 2053, expenses drop significantly and most of their money is spend on themselves while a small fraction covers home maintenance."),
    p("Salary is their main source of income throughout their lives (Figure 3B). However, the money accumulated in the years after they've paid off their property and before retirement leads to significant returns on investments."),
    p("After retirement, capital gains represent around 20% of their total income and is essential to offset the lost in salary."),
    p(strong("A")), renderUI(renderPlot(homeowner_ExpensesComp)),
    p(strong("B")), renderUI(renderPlot(homeowner_IncomeComp)),
    p(em(paste0("Figure 3 - Scenario 3, ", couple_name, ". Detailed view of the financial components in their expenses (A) and income (B)."))),
    hr(),



    h3("Sensitivity Analysis"),
    p("The scenario simulated above assumes that everything goes according to a plan. However, a fundamental aspect of having a stable financial life is robustness. That is, the ability to resist to unforeseen life changes."),
    p(paste0("Sensitivity analysis identifies factors to which finances are most sensitive by iteratively repeating the simulation while slightly varying a single parameters per iteration.")),
    p("Within the range tested (+- 20%), individual changes to some parameters do not significantly impact the couple's financial evolution. That's the case for the return on investments as they only invest passively later in life, or the rate of rent prices growth as they don't pay rent."),
    p("Surprisingly, the property value growth is also a minor parameter, as most of their asset value later in life will come from money invested passively. Delaying the purchase of the property by a few years also has minor impact."),
    p("Other parameters show a linear response to incremental changes, but only when it's in the direction that increases the total asset value. In the decreasing direction, the total asset value evolution response to changes is exponential, meaning that a small change to individual parameters have a larger than expected impact to their overall finances."),
    p(paste0("For example, if ", couple_name, " spend 420€ more than the expected ", format(living_style_costs , big.mark=","), "€/month – a 10% variation to their monthly living costs – they will have accumulated 500,000€ less in total asset value by retirement age, a whopping 50% variation to their total asset value.")),
    p("Spending any more than this could lead to debt collapse – a situation where their debt is not paid off fast enough and generates even more debt, exponentially increasing itself. Therefore, tightly controlling their living costs is not of great importance for Jack and Beck, as they have a large safety margin and could easily reduce living costs if needed."),
    p(paste0("To avoid debt collapse, similar analysis of other exponential parameters leads to the conclusion that ", couple_name, "'s must also keep working hard to achieve the minimum required salary growth (above ", salaries_growth_start_career, "%/year), must negotiate well the price of their home (bellow 525,000€). Failing to achieve these hallmarks puts them at risk of debt collapse, assuming all other parameters remain the same.")),
    renderUI(renderPlot(homeowner_SensitivityAnalysis, height = homeowner_SensitivityAnalysis_height)),
    p(em(paste0("Figure 4 - Scenario 3, ", couple_name, ". Sensitivity analysis on total asset value of single parameter perturbations."))),
    hr(),



    h3(paste0("How can ", couple_name, " improve their financial life?")),
    p(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE_SCENARIOS),

    h4("Guided by Financial Goals"),
    p(paste0("To improve the financial situation of ", couple_name, " we must keep in mind their goals: to have a stable financial life and to retire as soon as possible while maintaining their lifestyle. Because of their high incomes, stability is achieveble due to the large margins for financial adaptations they have. Moreover, some improvements could be desirable, for example in retiring earlier.")),
    p("In the pursue of these goals, they became homeowners. One common argument in favour of buying a home is that it provides a safe retirement. In fact, this is what we observed for Jack and Beck as, once paid, the property they purchased early in their lives brought significant but small expenses later. They also accumulate significant savings over their working lives, and maybe just saving their money would have been enough for retirement."),
    p(paste0("To become homeowners, ", couple_name, " had to pour all their income into paying their mortgage, leaving no capacity to build savings in initial years. This could represent increased financial risk, although lasting less than a decade.")),
    p("This restrictive situation ends when their salaries increase, and total income surpasses expenses. This reliance on their salaries increasing would mean that, for at least the first decade, they'd need to keep on working hard to get promotions."),
    p("From an investment perspective, this is also a highly risky decision as all their wealth is invested into a single asset. Besides the added risk of lacking diversification, for the three decades in which they pay their mortgage, they'd mildly fear losing their jobs, getting sick, or the next market downturn. This is however less of a problem in the last decade, when they'll have significant savings."),
    p("Altogether, becoming homeowners helps safeguarding their retirement although not necessary. It also comes with significant risks, especially in the initial decade."),
    hr(),

    h4("Aligning Financial Goals and Decisions"),
    p(paste0("What can they do differently to better align their financial goals and decisions? The Financial Evolution Calculator can be used to test multiple scenarios. For example, we can re-simulate ", couple_name, "'s scenario but instead of purchasing their home they kept on renting it (Figure 5).")),
    p("In this second scenario, Jack and Beck would build smaller total asset value than before: 800,000€ as renters vs 1,100,00€ as homeowners at retirement age (Figure 5A). However, they'd be able to retire ten years earlier at age of 57 (2052). The amount saved would be enough to sustain their lifestyle during retirement, indicating a stable and safe later life (Figure 5B). Therefore, in their case, living as Renters is also an option for a safe retirement, while allowing even earlier retirement."),
    p("Their assets would also be easily accessible in investment accounts, instead of buried in their property value. This form of asset is liquid and effectively works as an emergency fund for larger financial crisis, like extended sickness or loss of job. As Renters, they'd have throughout their lives a monetary safety net."),
    p("Iif upon retirement they'd still would like to own their home, they could use their savings to purchase the same property as in the homeowner scenario, but without mortgage or loans and still having funds to sustain their lifestyle. That is the case, even after considering the real estate market growth, inflation and taxes they'd need to pay on capital gains."),
    p(paste0("Given their personal goals, it would have been better for ", couple_name, " to not become homeowners and continue renting their home. As Renters, their financial evolution would still be sensitive to living costs and the rate their salaries grow but without risk of debt collapse (Figure 5C).")),
    p("Finally, it is important to note that few people have the discipline to not use the invested money, relying on a private pension schemes safeguarding their long-term investment."),
    p(strong("A")), renderUI(renderPlot(rent_AssetEvolution)),
    p(strong("B")), renderUI(renderPlot(rent_FinMetrics)),
    p(strong("C")), renderUI(renderPlot(rent_SensitivityAnalysis, height = rent_SensitivityAnalysis_height)),
    p(em(paste0("Figure 5 - Scenario 3, ", couple_name, ". Scenario simulation after delaying purchasing the home property to 2065. Total asset value evolution (A) and sensitivity analysis in simulations of scenarios with single parameter variations (B)."))),
    hr(),


    h4("Going Beyond Initial Goals"),
    p(paste0("In Buy-vs-Rent discussions, often people don't consider the third option: becoming landlords. There seems to be a general misconception that becoming landlords is something only rich people can do. Although more difficult to obtain a bank loan when one's salaries are low, with help of family and friends it could be possible. But is it a good idea for ", couple_name, "?")),
    p(paste0("Let's re-simulate a third scenario for ", couple_name, ". As before, they bought the same property (", format(value_today, big.mark=","), "€) in Berlin. Instead of moving in, they stayed in their rental apartment (", format(rent_month, big.mark=","), "€/month). Their property is then leased to someone else for ", format(landlord_cold_lease_today, big.mark=","), "€/month (cold lease), following the same rental contract they have on their own rent (", landlord_lease_rental_growth, "% increase per year).")),
    p("The Financial Evolution Calculator considers all taxes they’d need to pay in such scenario, the income loss of not having a tenant temporarily, possible income tax deductibles, the mortgage payment and payments for the family/friends loan, property management fees and property maintenance costs."),
    p("All these factors considered, their accumulated asset value at retirement age as landlords would be similar as before - 1,100,000€ as landlords and 1,100,000€ as homeowners. They would also retire at age 63 as landlords (Figure 6A) and their savings would be enough to sustain their lifestyle during retirement, indicating a stable and safe later life (Figure 6B)."),
    p("With the extra income source, they could pursue their secondary financial goals, like donating to causes, and passing on generational wealth."),
    p(paste0("Alternatively, they could have saved enough money by retirement age to buy the same property as in the homeowner scenario. That is, without a bank loan, without selling their leased property, and still having savings in their bank account after the purchase to sustain their lifestyle in retirement!")),
    p(strong("A")), renderUI(renderPlot(landlord_AssetEvolution)),
    p(strong("B")), renderUI(renderPlot(landlord_FinMetrics)),
    p(strong("C")), renderUI(renderPlot(landlord_SensitivityAnalysis, height = landlord_SensitivityAnalysis_height)),
    p(em(paste0("Figure 6 - Scenario 3, ", couple_name, ". Scenario simulation subleasing the property they bought in 2025 and purchasing their home property in 2065. Total asset value evolution (A) and sensitivity analysis in simulations with single parameter variations (B)."))),

    h4("Detailed Comparison"),
    p(paste0("How is this possible? It might come as a surprise, but it's really just numbers. The secret is time. While their incomes and expenses increase every year, the mortgage is fixed at least until refinancing 5-20 years later. Every year their cash flow becomes more positive and, while all their available cash is re-directed to pay the flat in the first years, it slowly builds savings (Table 1)."),
      p("By becoming landlords, ", couple_name, " pay their own rent (", format(12*rent_month, big.mark=","), "€/year) and house expenses (", format(12*fixed_housing_costs, big.mark=","), "€/year) plus the bank loan (33,000€/year). House expenses for the investment property are paid by the tenant.")),
    p(paste0("Altogether, their housing expenses (", format(12*rent_month, big.mark=","), " + ", format(12*fixed_housing_costs, big.mark=","), " + 33,000 = 5080€/month) in the first years are barely covered by the leftover salary after discounting living costs and adding the lease income (", format(12*net_annual_income, big.mark=","), " – ", format(12*living_style_costs, big.mark=","), " + ", format(12*landlord_cold_lease_today, big.mark=","), " = ", format(12*net_annual_income, - 12*living_style_costs + 12*landlord_cold_lease_today, big.mark=","), "€/year). Income tax deductions from expenses with the investment property (1000€/year) help significantly.")),
    p(paste0("From the investment perspective, ", couple_name, " diversified their assets over the years, increasing financial robustness. For example, their own rent will increase every year but, so long as their leased property increases rent at a similar rate, they are resistant to real estate market changes (Figure 6C).")),
    p("Also, their income does not depend only on their salaries anymore, as they receive lease. Been less dependent on salary means that they could try less hard for promotions."),
    p("The downside of becoming a landlord is, of course, the increased complexity. They'd need to deal with tenants or property management agencies, repair and management companies and more taxes. This extra headache holds back many people from pursuing such investments, even when the numbers clearly point to this alternative as a valid solution for the Buy-vs-Rent dilemma in some cases."),
    br(),
    p(strong("Table 1A - Initial years living as homeowner")),
    div(style = "width: 100%; max-width: 100%; overflow-x: auto; margin-bottom: 1.5rem; font-size: 12px;",
        renderDT({
          homeowner_table_for_vis})
    ),
    br(),
    p(strong("Table 1B - Initial years living as landlord")),
    div(style = "width: 100%; max-width: 100%; overflow-x: auto; margin-bottom: 1.5rem; font-size: 12px;",
        renderDT({
          landlord_table_for_vis})
    ),
    br(),
    br(),
  )
}
