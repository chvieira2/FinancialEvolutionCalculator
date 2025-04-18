scenario3Content <- function(is_mobile) {

  couple_name <- "Jack and Beck"

  main_scenario <- safelyLoadConfig(file.path("config", "templates", "inputs_high_wage_family_homeowner.yaml"))

  # Generate the figures for this scenario
  for (scenario in c("rent", "homeowner", "landlord")) {
    scenario_name <- paste0("high_wage_family_", scenario)
    config <- safelyLoadConfig(file.path("config", "templates",
                                         paste0("inputs_", scenario_name, ".yaml")))
    plot_data <- read.csv(file.path("article",
                                    paste0("calculations_", scenario_name, ".csv")))

    # Dynamically create variables from the inputs
    lapply(config, function(section) {
      if (is.null(section$inputs) && length(section) > 0) section <- section[[1]] # For properties
      lapply(section$inputs, function(input) {
        assign(paste0(scenario, "_", input$id), input$value, envir = .GlobalEnv)
      }
      )
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

    h2(paste0(couple_name, ", early retirement")),
    p(em(paste0(couple_name, " are a 30-years old high-income family in Germany. Their income is among the highest in Germany, but they are still part of the working class and do not live off inherited assets. They saved quite some money and consider buying a property to move out of rent."))),
    p("In this analysis, we will explore the financial evolution of", couple_name, "over the next 50 years. As they consider their options for the future,", strong("does it makes sense for them to buy a property in Berlin and move into it?"), "This is the main question we'll explore in this analysis."),

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
      tags$li(strong("Income"), paste0("- Their net income together is ", format(rent_net_annual_income, big.mark=","), "€/month. Their incomes are expected to increase at an annual rate of ", rent_salaries_growth_start_career, "%/year at the start of their careers (promotions, more education, more clients, more experience), and this rate slows down over time to half of that, as options for growth saturate. The net salary is calculated after payment of income taxes, pension and other mandatory insurances (e.g.: health insurance).")),
      tags$li(strong("Living standards"), paste0("- They don't care much about cutting costs and live a comfortable life, spending ", format(rent_living_style_costs , big.mark=","), "€ each month in groceries, clothing, commuting, entertainment, holidays and all other living costs. As life changes, the types of expenses are expected to change while the overall costs of living are expected to remain the same (after correcting for inflation). They have no room to further reduce these expenses.")),
      tags$li(strong("Retirement"), paste0("- They hope to retire lastest at the age of 70 in 2065.  Despite having private pension schemes included in their living costs, the drop in salary will still be significant (", rent_expected_salary_reduction_retirement, "%), given their high salaries.")),
      tags$li(strong("Family Size"), paste0("- They are pregnant with their first child and plan to have a second one right after that.")),
      tags$li(strong("Passive Investments"), paste0("- They invest all their savings in accumulating ETFs with ", rent_expected_return_on_investment, "%/year gross return and will change it into safer investment options over time, receiving ", rent_expected_conservative_return_on_investment, "%/year returns by retirement age. Any investment gains are taxed at ", rent_capital_gains_tax_rate, "% in Germany upon withdraw and after considering Vorabpauschale tax that is charged every year.")),
      tags$li(strong("Taxes"), paste0("- They pay the lowest income tax in Germany (", rent_income_tax, "% + solidarity surcharge tax) and no voluntary taxes to the church.")),
      tags$li(strong("Debts"), paste0("- Any time they go into debt (emergency reserve below zero), their only way of financial support is to contract a debt with a credit card while paying high interest rate (", rent_interest_rate_cash_flow_debt, "%/year).")),
      tags$li(strong("Lump Sums"), paste0("- They expect to inherit some money from their parents (", format(rent_lump_sum_1, big.mark=","), "€, around ", rent_lump_sum_1_year, "), but also to have other large expenses at some point (", format(rent_lump_sum_2, big.mark=","), "€, around ", rent_lump_sum_2_year, ")."))
    ),
    hr(),


    h3("Options on the Table"),
    p(paste0(couple_name, " are considering three options for their future:")),
    tags$ul(
      tags$li(strong("Renters"), paste0("- They continue renting a modern, large 4-room flat in Berlin (roughly evaluated at ", format(round(8000/23*rent_rent_month, -3), big.mark=",", scientific = FALSE), "€) close to the city center for ", format(rent_rent_month, big.mark=","), "€ + ", format(rent_fixed_housing_costs, big.mark=","), "€/month extra expenses. They expect this rent to increase over time (", rent_rental_prices_growth, "%/year). Upon retirement, they plan to use all their savings to buy a property to live in, without needing a bank load, and that is similar to the property bought in the homeowner scenario.")),
      tags$li(strong("Homeowners"), paste0("- They buy and move into a 3-room flat in Berlin for ", format(homeowner_value_today, big.mark=","),"€, not in the city centre but also not far. They use all their savings besides their emergency reserve as downpayment and contract a mortgage from the bank (", homeowner_initial_interest_rate, "%/year interest rate, repayment rate of ", homeowner_principal_repayment_rate, "%), but do not need to borrow money from their families and friends. They expect this property to increase in value over time (", homeowner_value_growth, "%/year). They do not amortize the loan, investing postive cash flow passively instead.")),
      tags$li(strong("Landlords"), paste0("- They buy and lease out a small 2-room flat in Berlin for ", format(landlord_value_today, big.mark=","),"€, not in the city centre but also not far. They use all their savings besides their emergency reserve as downpayment and contract a mortgage from the bank (", landlord_initial_interest_rate, "%/year interest rate, repayment rate of ", landlord_principal_repayment_rate, "%), but do not need to borrow money from their families and friends. They expect this property to increase in value over time (", landlord_value_growth, "%/year). They do not amortize the loan, investing postive cash flow passively instead. Upon retirement, they plan to use all their savings to buy a property to live in, without needing a bank load or selling their investment property, and that is similar to the property bought in the homeowner scenario."))
    ),
    hr(),


    h3("Total Asset Evolution"),
    p(paste0("The total asset evolution summarises ", couple_name, "'s financial situation in a single measurement. It is calculated as the sum of all their assets (cash, investments, property value) minus all their debts (mortgage, credit card debt). While limited, we can use it to explore the impact of taking different decisions regarding their housing situation (Figure 1).")),
    p("In all three scenarios analysed, their total asset evolution shows steady growth with a peak at or slowed increase from retirement age. By the time major lump sums are expected to arrive (positive and negative), they will have accumulated enough asset value that these will not have a significant impact."),
    p(paste0("Although similar at first glance, total asset value in these scenarios have remarkable differences as well. As homeowners or landlords ", couple_name, " bought a property with a loan from the bank. Due to this loan, their total asset value is negative at the start and takes 9 and 4 years, respectively, for their total asset value to reach zero.")),
    p(paste0("They expect to retire at the same age (", rent_expected_year_retirement - 2025 + 30, " in ", rent_expected_year_retirement,
             "). In all scenarios, they would have accumulated similar assets value by retirement age and that would sustain until late life, indicating that they'd be living stable financial lives after retirement.")),
    p("In all three scenarios, they hold until advanced age a significant total asset value that could be passed on to their kids as generational wealth."),
    p(strong("A - Renters")), renderUI(renderPlot(rent_AssetEvolution)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_AssetEvolution)),
    p(strong("C - Landlords")), renderUI(renderPlot(landlord_AssetEvolution)),
    p(em(paste0("Figure 1 - ", couple_name, ". Total asset value evolution in three scenarios: as renters (A), as homeowners (B), or as landlords (C)."))),
    br(),
    hr(),



    h3("Financial Metrics"),
    p(paste0("Deciding to purchase property is the main financial decision in ", couple_name, "'s life and its consequences can be better assessed by looking at their financial metrics (Figure 2).")),
    p(paste0("Depending on their housing situation, ", couple_name, " accumulate different amounts of savings as passive investments (yellow line) at retirement age. They accumualte the most as renters, even after buying their home for retirement. Regardless, in all scenarios their invested savings after retirement is significant.")),
    p("Important to notice that the money saved was not saved similarly in all scenarios (yellow line). As homeowners, the first ten years (2025-2035) are rather bumpy followed by two decades (2036-2055) of steady saving period that is then further exacerbated in the last decade before retirement (2056-2065). Most of their savings at retirement age was accumulated in this last decade."),
    p(paste0("This dynamic is due to how they balance total expenses (red line) and income (green line) in the first years, leaving hardly any cash flow (blue line) to invest passively and relying on their emergency reserve (orange line) whenever necessary to cover expenses. On the other hand, as renters, they have positive cash flow from the start, allowing them to invest passively from the beginning. As landlords, they also have a positive cash flow but it is smaller than in the renter scenario.")),
    p("As homeowners, significant reduction in their expenses can be observed around when they finally pay-off their property (2053) and kids leave home. The vanishing mortgage expenses and decreased costs with kids boosts the pace in which they've been accumulating savings during their last decade before retirement. This effect is less proeminet in the landlord scenario, as mortgage for the investment property is smaller."),
    p(paste0("Upon retirement they experience negative cash flow in all scenarios due the drop in salaries. However, this cash flow gap is easily offset with capital gains and by withdrawing (purple line) from their passive investments. They live a comfortable lifestyle throughout retirement, with a significant amount of money invested (>1,000,000€) and a property to live in.")),
    p(strong("A - Renters")), renderUI(renderPlot(rent_FinMetrics)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_FinMetrics)),
    p(strong("C - Landlords")), renderUI(renderPlot(landlord_FinMetrics)),
    p(em(paste0("Figure 2 - ", couple_name, ". Evolution of main financial metrics in three scenarios: as renters (A), as homeowners (B), or as landlords (C)."))),
    br(),
    hr(),



    h3("Income and Expenses Components"),
    p(paste0("Decomposition of ", couple_name, "'s income and expenses help paint a better picture of their financial evolution (Figure 3).")),
    p("Expenses with kids in the first 3 decades are small relative to their own lifestyle expenses, while living costs are constant throughout the years reflecting a sustained life quality. Mortgage, composed of principal and interest payments, is significant as homeowners and represent around 40% of their total expenses, but is less so as landlords, representing only 15%. As renters, their rent is significant and represents about 40% of their expenses, while only 25% in the landlord scenario."),
    p("Upon paying off their property (homeowner and landlord scenarios) and purchasing their home property (renter and landlord scenarios), expenses drop significantly and most of their money is spent on themselves as living costs while a small fraction covers home maintenance."),
    p("In all three scenarios, salary is their main source of income but capital gains on passive investments becomes significant over the years, even overshadowing their retirement salary. As landlords, the lease from tenants (and associated income tax deductions) supplements income and becomes relatively more significant after retirement."),
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
    p(em(paste0("Figure 3 - ", couple_name, ". Detailed view of expenses and income components in three scenarios: as renters (A), as homeowners (B), or as landlords (C)."))),
    br(),
    hr(),



    h3("Sensitivity Analysis"),
    p("These simulated scenarios assume that everything goes according to a plan. However, a fundamental aspect of having a stable financial life is robustness, that is, the ability to resist to unforeseen life changes. Sensitivity analysis identifies factors to which their finances are most sensitive by iteratively repeating the simulation while slightly varying a single parameters per iteration."),
    p("Within the range tested (+- 20%), individual changes to some parameters do not significantly impact the couple's financial evolution in any scenario. That's the case for the annual investment returns, the rate of growth on their own rent, the annual property value growth for their home property, the year when the first property is bought (home or investment property, for the homeowner and landlord scenarios respectively), how much mortgage they pay on their bank loan, and how much rent they charge from tenants in the landlord scenario."),
    p("Other parameters show linear response to incremental changes. Importantly, this linear relantionship changes after a threshold from which further parameter changes in the direction that reduces total asset value lead to larger than expected impact to their overall finances."),
    p(paste0("For example, in the homeowner scenario, if ", couple_name, " spend 420€ more than the expected ", format(homeowner_living_style_costs , big.mark=","), "€/month – a 10% variation to their monthly living costs – they will have accumulated 1,000,000€ less in total asset value by retirement age, a whopping 50% variation to their total asset value.")),
    p("Spending any more than this could lead to debt collapse – a situation where their debt is not paid off fast enough and generates even more debt. This risk seems distant for ", couple_name, ", as they have a large safety margin and could easily reduce living costs if needed. Such marging is even larger in both renter and landlord scenarios, where they could spend at least 840€ more than expected without risking debt collapse."),
    p(paste0("Similar analysis of other parameters leads to the conclusion that, while achieving the minimum required salary growth through promotions is an important factor in all scenarios, the only other identifiable risk facotr for debt colapse within the tested margins is the price they pay on their home in the homeowner scenario (but not as landlords).")),
    p("Overall, the financial evolution of ", couple_name, " is robust to significant changes in most parameters. It is also significant that they would have the most robust financial situation living as renters among all scenarios and within the margins tested."),
    p(strong("A - Renters")), renderUI(renderPlot(rent_SensitivityAnalysis,
                                                  height = rent_SensitivityAnalysis_height)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_SensitivityAnalysis,
                                                     height = homeowner_SensitivityAnalysis_height)),
    p(strong("C - Landlords")), renderUI(renderPlot(landlord_SensitivityAnalysis,
                                                    height = landlord_SensitivityAnalysis_height)),
    p(em(paste0("Figure 4 - ", couple_name, ". Sensitivity analysis on total asset value of single parameter perturbations."))),
    br(),
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
    p(em(paste0("Figure 5 - ", couple_name, ". Scenario simulation after delaying purchasing the home property to 2065. Total asset value evolution (A) and sensitivity analysis in simulations of scenarios with single parameter variations (B)."))),
    br(),
    hr(),


    h4("Going Beyond Initial Goals"),
    p(paste0("In Buy-vs-Rent discussions, often people don't consider the third option: becoming landlords. There seems to be a general misconception that becoming landlords is something only rich people can do. Although more difficult to obtain a bank loan when one's salaries are low, with help of family and friends it could be possible. But is it a good idea for ", couple_name, "?")),
    p(paste0("Let's re-simulate a third scenario for ", couple_name, ". As before, they bought the same property (", format(landlord_value_today, big.mark=","), "€) in Berlin. Instead of moving in, they stayed in their rental apartment (", format(landlord_rent_month, big.mark=","), "€/month). Their property is then leased to someone else for ", format(landlord_cold_lease_today, big.mark=","), "€/month (cold lease), following the same rental contract they have on their own rent (", landlord_lease_rental_growth, "% increase per year).")),
    p("The Financial Evolution Calculator considers all taxes they’d need to pay in such scenario, the income loss of not having a tenant temporarily, possible income tax deductibles, the mortgage payment and payments for the family/friends loan, property management fees and property maintenance costs."),
    p("All these factors considered, their accumulated asset value at retirement age as landlords would be similar as before - 1,100,000€ as landlords and 1,100,000€ as homeowners. They would also retire at age 63 as landlords (Figure 6A) and their savings would be enough to sustain their lifestyle during retirement, indicating a stable and safe later life (Figure 6B)."),
    p("With the extra income source, they could pursue their secondary financial goals, like donating to causes, and passing on generational wealth."),
    p(paste0("Alternatively, they could have saved enough money by retirement age to buy the same property as in the homeowner scenario. That is, without a bank loan, without selling their leased property, and still having savings in their bank account after the purchase to sustain their lifestyle in retirement!")),
    p(strong("A")), renderUI(renderPlot(landlord_AssetEvolution)),
    p(strong("B")), renderUI(renderPlot(landlord_FinMetrics)),
    p(strong("C")), renderUI(renderPlot(landlord_SensitivityAnalysis, height = landlord_SensitivityAnalysis_height)),
    p(em(paste0("Figure 6 - ", couple_name, ". Scenario simulation subleasing the property they bought in 2025 and purchasing their home property in 2065. Total asset value evolution (A) and sensitivity analysis in simulations with single parameter variations (B)."))),
    br(),

    h4("Detailed Comparison"),
    p(paste0("How is this possible? It might come as a surprise, but it's really just numbers. The secret is time. While their incomes and expenses increase every year, the mortgage is fixed at least until refinancing 5-20 years later. Every year their cash flow becomes more positive and, while all their available cash is re-directed to pay the flat in the first years, it slowly builds savings (Table 1)."),
      p("By becoming landlords, ", couple_name, " pay their own rent (", format(12*landlord_rent_month, big.mark=","), "€/year) and house expenses (", format(12*landlord_fixed_housing_costs, big.mark=","), "€/year) plus the bank loan (33,000€/year). House expenses for the investment property are paid by the tenant.")),
    p(paste0("Altogether, their housing expenses (", format(12*landlord_rent_month, big.mark=","), " + ", format(12*landlord_fixed_housing_costs, big.mark=","), " + 33,000 = 5080€/month) in the first years are barely covered by the leftover salary after discounting living costs and adding the lease income (", format(12*landlord_net_annual_income, big.mark=","), " – ", format(12*landlord_living_style_costs, big.mark=","), " + ", format(12*landlord_cold_lease_today, big.mark=","), " = ", format(12*landlord_net_annual_income, - 12*landlord_living_style_costs + 12*landlord_cold_lease_today, big.mark=","), "€/year). Income tax deductions from expenses with the investment property (1000€/year) help significantly.")),
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
  )
}
