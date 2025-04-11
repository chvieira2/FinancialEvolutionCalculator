scenario3Content <- function() {

  main_scenario <- safelyLoadConfig(file.path("config", "templates", "inputs_high_wage_family_homeowner.yaml"))

  # Dynamically create variables from the inputs
  lapply(main_scenario, function(section) {
    if (is.null(section$inputs) & length(section) > 0) section <- section[[1]] # For properties
    lapply(section$inputs, function(input) {
      assign(input$id, input$value, envir = .GlobalEnv)
    }
    )
  })


  div(
    h2("Jack and Beck, wearing golden cuffs"),
    p(em("Jack and Beck are a 30-years old high-income family in Germany. They likely work on knowledge jobs requiring high level of education. Their salaries depend mostly on their education rather than their performance. Their income is among the highest of all Germany, but they are still part of the working class meaning they do not live off inherited assets. Their salaries allow them to save and buy a property in Berlin not far from the city centre.")),

    h3("Financial Goals"),
    tags$ul(
      tags$li(strong("Lifestyle"), "-", NAVTAB_CONTENT$SCENARIOS$LIFESTYLE_DESCRIPTION),
      tags$li(strong("Living Costs"), "-", "Because their monthly living expenses already provide them with a comfortable life, having more wealth wouldn’t significantly improve their life quality and isn’t a primary goal."),
      tags$li(strong("Work and Retirement"), "-", NAVTAB_CONTENT$SCENARIOS$LATE_LIFE_DESCRIPTION),
      tags$li(strong("Generational Wealth"), "-", NAVTAB_CONTENT$SCENARIOS$GENERATIONAL_WEALTH_DESCRIPTION),
      tags$li(strong("Donations"), "-", NAVTAB_CONTENT$SCENARIOS$EXTRA_CASH_DESCRIPTION)
    ),
    hr(),



    h3("Personal Finances Parameters"),
    tags$ul(
      tags$li(strong("Income"), paste0("- Their net income together is ", net_annual_income, "€/month. Their incomes are expected to increase at an annual rate of ", salaries_growth_start_career, "%/year at the start of their careers (promotions, more education, more clients, more experience), and this rate slows down over time to half of that, as options for growth saturate.")),
      tags$li(strong("Living standards"), paste0("- They don’t care much about cutting costs and live a comfortable life, spending ", living_style_costs, "€ each month in groceries, clothing, commuting, entertainment, holidays and all other living costs. As life changes, the types of expenses are expected to change while the overall costs of living are expected to remain the same (after correcting for inflation). They have no room to further reduce these expenses.")),
      tags$li(strong("Retirement"), paste0("- They hope to retire at the age of ", as.character(as.numeric(expected_year_retirement) - 2025 + 30), " in ", expected_year_retirement, ".  Despite having private pension schemes included in their living costs, the drop in salary will still be significant (", expected_salary_reduction_retirement, "%), given their high salaries.")),
      tags$li(strong("Family Size"), "- They are pregnant with their first child and plan to have a second one right after that."),
      tags$li(strong("Housing"), paste0("- They used to rent a big 3-room flat in Berlin (", rent_month, "€ + ", fixed_housing_costs, "€/month extra expenses for housing) but just recently bought a large flat in Berlin for ", value_today,"€, not in the city centre but also not far. For that, they contracted a mortgage from the bank (", initial_interest_rate, "%/year interest rate, repayment rate of ", principal_repayment_rate, "%) and used all their savings besides their emergency reserve, but did not need to borrow money from their families. They expect this property to increase in value over time (", value_growth, "%/year). They will not use positive yearly cash flow to amortize the loan, investing it passively instead.")),
      tags$li(strong("Passive Investments"), paste0("- They invest all their savings in accumulating ETFs with ", expected_return_on_investment, "%/year gross return and will change it into safer investment options over time, receiving ", expected_conservative_return_on_investment, "%/year returns by retirement age. Any investment gains are taxed at ", capital_gains_tax_rate, "% in Germany upon withdraw and after considering Vorabpauschale tax that is charged every year.")),
      tags$li(strong("Taxes"), paste0("- They pay the lowest income tax in Germany (", income_tax, "% + solidarity surcharge tax) and no voluntary taxes to the church.")),
      tags$li(strong("Debts"), paste0("- Any time they go into debt (emergency reserve below zero), their only way of financial support is to contract a debt with a credit card while paying high interest rate (", interest_rate_cash_flow_debt, "%/year).")),
      tags$li(strong("Lump Sums"), paste0("- They expect to inherit some money from their parents (", lump_sum_1, "€, around ", lump_sum_1_year, "), but also to have other large expenses at some point (", lump_sum_2, "€, around ", lump_sum_2_year, ")."))
    ),
    hr(),



    h3("Total Asset Evolution"),
    p(em("Figure 1 - Scenario 3, Jack and Beck. Total asset value evolution.")),
    p("As Jack and Beck just bought a home with a loan from the bank, their total asset value is negative but grows over the years (Figure 1). It takes 9 years for their total asset value to reach zero."),
    p("By the time major lump sums are expected to arrive (positive and negative), they will have accumulated enough asset value that these will not have a significant impact on their overall financial evolution."),
    p("Upon retirement at age of 63 (2058), their accumulated asset value decreases slightly, indicating that they’d be living stable financial lives without capacity to increase their expenses even further."),
    p("They hold until advanced age a significant total asset value that could be passed on to their kids as generational wealth."),
    hr(),



    h3("Financial Metrics"),
    p(em("Figure 2 - Scenario 3, Jack and Beck. Evolution of main financial metrics.")),
    p("Jack and Beck’s main financial metrics are marked by the purchase of their home property early in their lives."),
    p("In the following five years (2025-2030), they balance total expenses and income well, having no extra savings to invest passively and relying on their emergency reserve whenever necessary to cover expenses (Figure 2)."),
    p("In the following years, their income surpasses expenses, and the positive cash flow is used to start build savings. The inheritance from their parents in 2045, although not significantly impacting the total value asset, adds to their savings."),
    p("Their financial metrics evolution takes off when they finally pay-off their home (2053). The positive cash flow resulting from vanishing mortgage expenses boosts the pace in which they’ve been accumulating savings."),
    p("The pace with which they build savings changes upon retirement, when the drop in their salaries brings their cash flow to negative. This cash flow gap is however small and is easily offset by their passive investments."),
    p("They live a comfortable lifestyle throughout retirement, with a significant amount of money invested (>200.000€) and a property to live in."),
    hr(),



    h3("Income and Expenses Components"),
    p(em("Figure 3 - Scenario 3, Jack and Beck. Detailed view of the financial components of income and expenses.")),
    p("Decomposition of Jack and Beck’s income and expenses help paint a better picture of their financial evolution. Their living costs are constant throughout the years, except when the kids are born, reflecting a sustained life quality (Figure 3A)."),
    p("In the first three decades of their lives, expenses with kids are small relative to their own lifestyle expenses, while mortgage – composed of principal and interest payments – is significant and represents around 50% of their total expenses."),
    p("Upon paying off the property around 2053, expenses drop significantly and most of their money is spend on themselves while a small fraction covers home maintenance."),
    p("Salary is their main source of income throughout their lives (Figure 3B). However, the money accumulated in the years after they’ve paid off their property and before retirement leads to significant returns during retirement."),
    p("After retirement, capital gains represent around 20% of their total income and is essential to offset the lost in salary that leads to decreasing savings."),
    hr(),



    h3("Sensitivity Analysis"),
    p(em("Figure 4 - Scenario 3, Jack and Beck. Sensitivity analysis on total asset value of single parameter perturbations.")),
    p("A fundamental aspect of having a stable financial life is financial robustness. That is, the ability to resist to unforeseen life changes. The simulated scenario assumes that everything goes according to the plan, without deviations."),
    p("Sensitivity analysis identifies factors to which Jack and Beck’s finances are most sensitive by iteratively repeating the simulation upon slightly varying a single parameters per iteration (Figure 4)."),
    p("Within the range tested (+- 20%), individual changes to some parameters do not significantly impact the couple’s financial evolution. That’s the case for the return on investments as they only invest passively later in life, or the rate of rent prices growth as they don’t pay rent."),
    p("Surprisingly, the property value growth is also a minor parameter, as most of their asset value later in life will come from money invested passively. Delaying the purchase of the property by a few years also has minor impact."),
    p("Other parameters show a linear response to incremental changes, but only when it’s in the direction that increases the total asset value. In the decreasing direction, the total asset value evolution response to changes is exponential, meaning that a small change to individual parameters have a larger than expected impact to their overall finances."),
    p("For example, if this family spends 210€ more than the expected 4200€/month – a 5% variation to their monthly living costs – they will have accumulated 400.000€ less in total asset value by retirement age – a whopping 40% variation to their total asset value."),
    p("Spending any more than this could lead to debt collapse – a situation where their debt is not paid off fast enough and generates even more debt, exponentially increasing. Therefore, tightly controlling their living costs is not of great importance for Jack and Beck, as they have a large safety margin and could easily reduce their living costs if needed."),
    p("To avoid debt collapse, similar analysis of other exponential parameters leads to the conclusion that Jack and Beck’s must also keep working hard to achieve the minimum required salary growth (above 3.6%/year), must negotiate well the price of their home (bellow 525.000€) and must negotiate a good interest rate on their bank loan (bellow 4.4%). Failing to achieve these hallmarks puts them at risk of debt collapse, assuming all other parameters remain the same."),
    hr(),



    h3("How can Jack and Beck improve their financial life?"),
    p(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE_SCENARIOS),

    h4("Guided by Financial Goals"),
    p("To improve the financial situation of Jack and Beck we must keep in mind their goals: to have a stable financial life and to retire as soon as possible while maintaining their lifestyle. Because of their high incomes, stability is easily obtained due to their large margin to adapt finances. Moreover, some improvements could be desirable for example to achieve earlier retirement."),
    p("In the pursue of these goals, they became homeowners. One common argument in favour of buying a home is that it provides a safe retirement. In fact, this is what we observed for Jack and Beck as, once paid, the property they purchased early in their lives brought significant but small expenses later during retirement. However, they also accumulate significant savings over their working lives, and it is unclear whether savings would have been enough for retirement."),
    p("To become homeowners, Jack and Beck had to pour all their income into paying their mortgage, leaving no capacity to build savings in initial years. This could represent increased financial risk, although only really lasting less than a decade."),
    p("This restrictive situation ends when their salaries increase, and income surpasses expenses. This reliance on their salaries increasing would mean that, for at least the first decade, they’d need to keep on working hard to get promotions."),
    p("From an investment perspective, this is also a highly risky decision as all their wealth is invested into a single asset. Besides the added risk of lacking diversification, for the three decades in which they pay their mortgage, they’d mildly fear losing their jobs, getting sick, or the next market downturn. This is however less of a problem in the last decade, when they’ll have significant savings."),
    p("Altogether, becoming homeowners helps safeguarding their retirement although not necessary. It also comes with significant risks, especially in the initial decade."),
    hr(),

    h4("Aligning Financial Goals and Decisions"),
    p(em("Figure 5 - Scenario 3, Jack and Beck. Scenario simulation after delaying purchasing the home property to 2065. Total asset value evolution (A) and sensitivity analysis in simulations of scenarios with single parameter variations (B).")),
    p("What could they have done differently to better align their financial goals and decisions? The Financial Evolution Calculator can be used to test multiple scenarios. For example, we can re-simulate Jack and Beck’s scenario but instead of purchasing their home they kept on renting it (Figure 5)."),
    p("In this second scenario, Jack and Beck would build a similar total asset value as before (800.000€, at retirement age) while retiring at age 60 (Figure 5A). The amount saved would be enough to sustain their lifestyle during retirement, indicating a stable and safe later life (Figure 5B). Therefore, in their case, living as a renter is also an option for a safe retirement, while allowing even earlier retirement."),
    p("Their assets would also be easily accessible in investment accounts, instead of buried in their property value. This form of asset is liquid and effectively works as an emergency fund for larger financial crisis, like extended sickness or loss of job. As a renter, they’d have throughout their lives a monetary safety net."),
    p("In addition, if upon retirement they’d still would like to own their home, they could use their savings at retirement age to purchase the same property as in the homeowner scenario, without a bank loan and still having funds to sustain their lifestyle. That is the case, even after considering the real estate market growth, inflation and taxes they’d need to pay on capital gains."),
    p("Given their personal goals, it would have been better for Jack and Beck to not become homeowners and continue renting their home. As renters, their financial evolution would still be sensitive to living costs and the rate their salaries grow but without risk of debt collapse (Figure 5C)."),
    p("Finally, it might also be important to consider that few people have the discipline to not use the invested money, relying on a private pension scheme to safeguard their money from themselves spending it too early."),
    hr(),

    h4("Going Beyond Initial Goals"),
    p(em("Figure 6 - Scenario 3, Jack and Beck. Scenario simulation subleasing the property they bought in 2025 and purchasing their home property in 2065. Total asset value evolution (A) and sensitivity analysis in simulations with single parameter variations (B).")),
    p("In “Buy-vs-Rent” discussions, rarely people consider a third alternative: to keep on renting their home but still purchase a property and renting it out to someone else!"),
    p("Let’s re-simulate a third scenario for Jack and Beck. As before, they bought the same property (500.000€) in Berlin. Instead of moving in, they stayed in their rental apartment (2000€/month). Their property is then leased to someone else at 1000€/month rental price, following the same rental contract they have on their own rent (3% increase per year)."),
    p("The Financial Evolution Calculator considers all taxes they’d need to pay, risk of not having a tenant, tax deductible costs, mortgage for the investment property, property depreciation, management fees and maintenance costs."),
    p("Their total accumulated asset value at retirement age as landlords would be similar as before (800.000€, at retirement age) while retiring at age 47 (Figure 6A). The amount saved would be enough to sustain their lifestyle during retirement, indicating a stable and safe later life (Figure 6B)."),
    p("With the extra income source, they could pursue their secondary financial goals, like donating to causes, and passing on generational wealth."),
    p("Interestingly, they would have saved enough money by retirement age to buy the same property as in the homeowner scenario. That is, without a bank loan, without selling their leased property, and still having savings in their bank account after the purchase to sustain their lifestyle in retirement!"),
    p("How is this possible? It might come as a surprise, but it’s really just numbers. By becoming landlords, Jack and Beck pay their own rent (2000€/month) and house expenses (330€/year) plus the bank loan (2750€/month). House expenses for the investment property are paid by the tenant."),
    p("Altogether, their housing expenses in the first years are barely covered by the leftover salary after discounting living costs plus the lease income (7400 – 4200 + 1200 = 4400€/month). Income tax deductions from expenses with the investment property (1000€/month) help significantly."),
    p("The secret is time. While their incomes and expenses increase every year, the mortgage is fixed at least until refinancing 5-20 years later. Every year their cashflow becomes more positive and, while all their available cash is re-directed to pay the flat in the first years, it slowly builds savings."),
    p("From the investment perspective, Jack and Beck diversified their assets over the years, increasing financial robustness. For example, their own rent will increase every year but, so long as their leased property increases rent at a similar rate, they are immune to real estate market changes (Figure 6C)."),
    p("Also, their income does not depend only on their salaries anymore, as they receive lease. Been less dependent on salary means that they could work less and try less hard for promotions."),
    p("The downside of becoming a landlord is, of course, the increased complexity. They’d need to deal with tenants or property management agencies, repair and management companies and more taxes. This extra headache holds back people from pursuing such investments, even when the numbers clearly point to this alternative as a valid solution for the Buy-vs-Rent dilemma."),
    hr(),
    hr(),
  )
}
