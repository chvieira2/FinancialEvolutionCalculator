scenario2Content <- function() {

  main_scenario <- safelyLoadConfig(file.path("config", "templates", "inputs_mid_wage_family_homeowner.yaml"))

  # Dynamically create variables from the inputs
  lapply(main_scenario, function(section) {
    if (is.null(section$inputs) && length(section) > 0) section <- section[[1]] # For properties
    lapply(section$inputs, function(input) {
      assign(input$id, input$value, envir = .GlobalEnv)
    }
    )
  })

  div(
    h2("Alex and Max, the home-poor lifestyle"),
    p(em("Alex and Max are a 30-years old middle-income family in Berlin, Germany. They likely work on technical jobs, services or other functions requiring some level of higher education. Their salaries are directly linked to their performance.")),

    h3("Financial Goals"),
    tags$ul(
      tags$li(strong("Lifestyle"), "-", NAVTAB_CONTENT$SCENARIOS$LIFESTYLE_DESCRIPTION),
      tags$li(strong("Living Costs"), "-", "Because they still must closely limit their monthly expenses, having more to spend on themselves could improve their life quality."),
      tags$li(strong("Work and Retirement"), "-", NAVTAB_CONTENT$SCENARIOS$LATE_LIFE_DESCRIPTION),
      tags$li(strong("Generational Wealth"), "-", NAVTAB_CONTENT$SCENARIOS$GENERATIONAL_WEALTH_DESCRIPTION),
      tags$li(strong("Donations"), "-", NAVTAB_CONTENT$SCENARIOS$EXTRA_CASH_DESCRIPTION)
    ),
    hr(),



    h3("Personal Finances Parameters"),
    tags$ul(
      tags$li(strong("Income"), paste0("- Their net income together is ", net_annual_income, "€/month. Their incomes are expected to increase at an annual rate of ", salaries_growth_start_career, "%/year at the start of their careers (promotions, more education, more clients, more experience), and this rate slows down over time to half of that, as options for growth saturate.")),
      tags$li(strong("Living standards"), paste0("- They try to save but can afford some comfort, spending ", living_style_costs, "€ each month in groceries, clothing, commuting, entertainment, holidays and all other living costs. As life changes, the types of expenses are expected to change while the overall costs of living are expected to remain the same (after correcting for inflation). They have no room to further reduce these expenses.")),
      tags$li(strong("Retirement"), paste0("- They hope to retire at the age of ", as.character(as.numeric(expected_year_retirement) - 2025 + 30), " in ", expected_year_retirement, " and expect their salaries to drop by ", expected_salary_reduction_retirement, "%.")),
      tags$li(strong("Family Size"), "- They are pregnant with their first child and plan to have a second one right after that."),
      tags$li(strong("Housing"), paste0("- They used to rent a small 3-room flat in Berlin (", rent_month, "€ + ", fixed_housing_costs, "€/month extra expenses for housing) but just recently bought a small flat further away from the city centre for ", value_today,"€. For that, they contracted a mortgage from the bank (", initial_interest_rate, "%/year interest rate, repayment rate of ", principal_repayment_rate, "%), used all their savings besides their emergency reserve, and borrowed money (", loan_family_friends, "€) from their families. They expect this property to increase in value over time (", value_growth, "%/year). They will not use positive yearly cash flow to amortize the loan, investing it passively instead.")),
      tags$li(strong("Passive Investments"), paste0("- They invest all their savings in accumulating ETFs with ", expected_return_on_investment, "%/year gross return and will change it into safer investment options over time, receiving ", expected_conservative_return_on_investment, "%/year returns by retirement age. Any investment gains are taxed at ", capital_gains_tax_rate, "% in Germany upon withdraw and after considering Vorabpauschale tax that is charged every year.")),
      tags$li(strong("Taxes"), paste0("- They pay the lowest income tax in Germany (", income_tax, "% + solidarity surcharge tax) and no voluntary taxes to the church.")),
      tags$li(strong("Debts"), paste0("- Any time they go into debt (emergency reserve below zero), their only way of financial support is to contract a debt with a credit card while paying high interest rate (", interest_rate_cash_flow_debt, "%/year).")),
      tags$li(strong("Lump Sums"), paste0("- They expect to inherit some money from their parents (", lump_sum_1, "€, around ", lump_sum_1_year, "), but also to have other large expenses at some point (", lump_sum_2, "€, around ", lump_sum_2_year, ")."))
    ),
    hr(),



    h3("Total Asset Evolution"),
    p(em("Figure 1 - Scenario 2, Alex and Max. Total asset value evolution.")),
    p("As Alex and Max just bought a home with a loan from the bank, their total asset value is negative but grows over the years (Figure 1). It takes 10 years for their total asset value to reach zero."),
    p("By the time major lump sums are expected to arrive (positive and negative), they will have accumulated enough asset value that these will not have a significant impact on their overall financial evolution."),
    p("Upon retirement at age of 70 (2065), their total asset curve stagnates, indicating that they’d be living stable financial lives so long as they keep the same lifestyle and living costs."),
    p("They hold until advanced age a significant total asset value that could be passed on to their kids as generational wealth."),
    hr(),



    h3("Financial Metrics"),
    p(em("Figure 2 - Scenario 2, Alex and Max. Evolution of main financial metrics.")),
    p("Alex and Max’s main financial metrics are marked by the purchase of their home property early in their lives."),
    p("In the following decades (2025-2052), they balance total expenses and income well, having no extra savings to invest passively and rely on their emergency reserve whenever necessary to cover expenses (Figure 2)."),
    p("The inheritance from their parents in 2045, although not significantly impacting the total value asset, marks the beginning of their passive investment journey."),
    p("Their financial metrics evolution takes off when they finally pay-off their home (2053). The positive cash flow resulting from vanishing mortgage expenses quickly recovers their emergency fund and boosts the pace in which they’ve been accumulating savings."),
    p("The pace with which they build savings changes upon retirement in 2065, when the drop in their salaries brings their cash flow to negative. This cash flow gap is however small and is easily offset by the returns from their large investments."),
    p("They live a stable lifestyle throughout retirement, with a significant amount of money invested (450.000€) and a property to live in."),
    hr(),



    h3("Income and Expenses Components"),
    p(em("Figure 3 - Scenario 2, Alex and Max. Detailed view of the financial components of income and expenses.")),
    p("Decomposition of Alex and Max’s income and expenses help paint a better picture of their financial evolution. Their living costs are constant throughout the years, except in the years after the kids are born, reflecting a sustained lifestyle (Figure 3A)."),
    p("In the first three decades of their lives, kids represent a significant but not large expense, while mortgage – composed of principal and interest payments – is very significant and represents around 50% of their total expenses."),
    p("Upon paying off the property around 2053, expenses drop significantly and most of their money is spend on themselves and on maintaining their home."),
    p("Salary is their main source of income throughout their entire lives (Figure 3B). However, the money accumulated in the years after they’ve paid off their property and before retirement leads to significant returns."),
    p("After retirement, capital gains represent around 40% of their total income and is essential for the fine balance in cash flow that provides them with a stable retirement."),
    hr(),



    h3("Sensitivity Analysis"),
    p(em("Figure 4 - Scenario 2, Alex and Max. Sensitivity analysis on total asset value of single parameter perturbations.")),
    p("A fundamental aspect of having a stable financial life is financial robustness. That is, the ability to resist to unforeseen life changes. The simulated scenario assumes that everything goes according to the plan, without deviations."),
    p("Sensitivity analysis identifies factors to which Alex and Max’s finances are most sensitive by iteratively repeating the simulation upon slightly varying a single parameters per iteration (Figure 4)."),
    p("Within the range tested (+- 20%), individual changes to some parameters do not significantly impact the couple’s financial evolution. That’s the case for the return on investments as they only invest passively later in life, or the rate rent prices grow as they don’t pay rent. Surprisingly, that’s also the case for the property value growth, as most of their asset value later in life will come from money invested passively."),
    p("Other parameters show a linear response to incremental changes. Delaying the purchase of their property by a few years is an example. Buying the property earlier means it’s paid off earlier and the investing phase at later life is longer."),
    p("Interestingly, some parameters display an exponential impact on total asset value evolution, meaning that a small change to individual parameters have a larger than expected impact to their overall finances."),
    p("For example, if this family spends 110€ more than the expected 2200€/month – a 5% variation to their monthly living costs – they will have accumulated 200.000€ less in total asset value by retirement age – a whopping 25% variation to their total asset value."),
    p("Spending any more than this could lead to debt collapse – a situation where their debt is not paid off fast enough and generates even more debt, exponentially increasing. Therefore, controlling their living costs is important for Alex and Max, as they have a tight safety margin."),
    p("To avoid debt collapse, similar analysis of other exponential parameters leads to the conclusion that Alex and Max’s must also keep working hard to achieve the minimum required salary growth (above 2.6%/year), must negotiate well the price of their home (bellow 262.500€), and must also negotiate a good interest rate on their bank loan (bellow 4.1%). Failing to achieve these hallmarks puts them at risk of debt collapse, assuming all other parameters remain the same."),
    p("Unfortunately, this exponential pattern is only observed in the negative direction. Changes to parameters in the direction that would increase their total asset value only brings an equivalent, proportional increase in total asset value."),
    hr(),



    h3("How can Alex and Max improve their financial life?"),
    p(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE_SCENARIOS),

    h4("Guided by Financial Goals"),
    p("To improve the financial situation of Alex and Max we must keep in mind their goals: to have a stable financial life, to enhance their live quality by increasing expenses and to retire at later age while maintaining their lifestyle focused on family, friends and hobbies."),
    p("In the pursue of these goals, they became homeowners. This is by far the biggest financial decision in their lives. One common argument in favour of buying a home is that it provides a safe retirement. In fact, this is what we observed for Alex and Max as, once paid, the property they purchased early in their lives brought significant but small expenses later during retirement."),
    p("Buying their home property helped them achieve a safe retirement but in detriment of their other financial goals. Alex and Max had to pour all their income into paying their home, leaving no capacity to build savings."),
    p("Practically, that means they would need to sustain their current lifestyle and living costs until they pay their mortgage. As shown in our sensitivity analysis, deviating by 200€ from their average living costs for too long could have drastic effect on their overall financial situation, risking debt collapse."),
    p("From an investment perspective, this is also a highly risky decision as all their wealth is invested into a single asset. Besides the added risk of lacking diversification, for three decades, they’d be constantly afraid of losing their jobs, getting sick, or the next market downturn. And in case anything bad happens to them, their only option is to sell their family home and move out."),
    p("At the same time, given the current market prices in Berlin in 2025 and that they need a home with at least 2 bedrooms, the property they can afford (250.000€) is likely far from their workplaces, family and friends. Living this far from the city centre might not be ideal for them, although that’s mostly a lifestyle choice."),
    p("Altogether, while been homeowners safeguards their retirement, it also comes with extreme risks carried throughout most of their adult lives."),
    hr(),

    h4("Aligning Financial Goals and Decisions"),
    p(em("Figure 5 - Scenario 2, Alex and Max. Scenario re-simulation without homeownership. Total asset value evolution (A), evolution of main financial metrics (B), and sensitivity analysis on total asset value of single parameter perturbations (C).")),
    p("What could they have done differently to better align their financial goals and decisions? The Financial Evolution Calculator can be used to test multiple scenarios. For example, we can re-simulate Alex and Max’s scenario where instead of purchasing their home they keep on renting it (Figure 5)."),
    p("In this second scenario, Alex and Max would build less than half of the total asset value as before (300.000€ vs 800.000€, at retirement age) (Figure 5A). However, this amount would be enough to sustain their lifestyle during retirement, indicating a stable and safe later life (Figure 5B). Therefore, in their case, living as a renter is also an option for a safe retirement."),
    p("Importantly, the rent is a key factor in their finances in this scenario. Every cash saved in rent could be directly spent on increasing their living standards, and vice-versa. Paying 1500€/month would let them sustain their current lifestyle while also building savings."),
    p("Their assets would also be easily accessible in investment accounts, instead of buried in their property value. This form of asset is liquid and effectively works as an emergency fund for larger financial crisis, like extended sickness or loss of job. Unlike in the homeowner scenario, as a renter, they’d have throughout their lives a monetary safety net."),
    p("At this rental price, they could also live closer to the city centre and access better public transportation options. This would improve their mobility while possibly also reducing costs. Alternatively, they could choose to live further away, saving on rent and spending on themselves instead."),
    p("This flexibility in their living situation, as they could move in and out easily, might also be used to offset financial burdens, enhancing their financial stability."),
    p("Finally, if upon retirement they’d still would like to own their home, they could use their savings that at this moment will be almost covering the property price entirely. That is the case, even after considering the real estate market growth, inflation and taxes they’d need to pay on capital gains. However, it might be difficult to obtain a mortgage to cover the remaining purchase cost."),
    p("Given their personal goals, it would have been better for Alex and Max to not become homeowners and instead rent their home. However, living in rental would still leave them sensitive to some parameters, including living costs, salary growth rate and rent price growth rate (Figure 5C)."),
    p("Also, few people have the discipline to not use the invested money, relying on a private pension scheme to safeguard their money from themselves spending it too early, which takes away from the flexibility of having such money accumulated."),
    hr(),

    h4("Going Beyond Initial Goals"),
    p(em("Figure 6 - Scenario 2, Alex and Max. Scenario simulation leasing the property purchased in 2025. Total asset value evolution (A), evolution of main financial metrics without Total Investment value (B), and sensitivity analysis on total asset value of single parameter perturbations (C).")),
    p("In “Buy-vs-Rent” discussions, rarely people consider a third alternative: to keep on renting their home but still purchase a property and renting it out to someone else! There seems to be a general misconception that becoming a landlord is something only rich people do. There is no reason for that, and everyone could benefit from this type of active investment in real estate."),
    p("Let’s re-simulate a third scenario for Alex and Max. As before, they bought the same property (250.000€) in the periphery of Berlin. Instead of moving in, they stayed in their rental apartment (1500€/month) closer to the city centre. Their property is then leased to someone else at 750€/month rental price, following the same rental contract they have on their own rent (3% increase per year)."),
    p("The Financial Evolution Calculator considers all taxes they’d need to pay, risk of not having a tenant, tax deductible costs, mortgage for the investment property, property depreciation, management fees and maintenance costs."),
    p("Their total accumulated asset value at retirement age as landlords would be 50% higher than when they were homeowners (1.250.000€ vs 850.000€) (Figure 6A). They would not experience debt (Figure 6B) and would be resilient to unexpected events throughout their lives due to their savings (Figure 6C)."),
    p("With the extra income source, they could boost their living costs to enhance life quality and even pursue their secondary financial goals, like donating to causes, working less and passing on generational wealth."),
    p("Interestingly, they would have saved enough money by retirement age to buy a bigger home – evaluated at 650.000€ in today’s price. That is, without a bank loan, without selling their leased property, and still having savings in their bank account after the purchase to sustain their lifestyle in retirement!"),
    p("How is this possible? It might come as a surprise, but it’s really just numbers. By becoming landlords, Alex and Max pay their own rent (1500€/month) and house expenses (220€/year) plus the bank loan (1500€/month). House expenses for the investment property are paid by the tenant."),
    p("Altogether, their housing expenses in the first years are barely covered by the leftover salary after discounting living costs plus the lease income (4500 – 2200 + 750 = 3050€/month). Income tax deductions from expenses with the investment property (250€/month) help significantly."),
    p("The secret is time. While their incomes and expenses increase every year, the mortgage is fixed at least until refinancing 5-20 years later (Figure 6B). Every year their cashflow becomes more positive and, while all their available cash is re-directed to pay the flat in the first years, it slowly builds savings."),
    p("From the investment perspective, Alex and Max diversified their assets over the years, increasing financial robustness. For example, their own rent will increase every year but, so long as their leased property increases rent at a similar rate, they are immune to real estate market changes (Figure 6C)."),
    p("Also, their income does not depend only on their salaries anymore, as they receive lease. Been less dependent on salary means that they could work less and try less hard for promotions."),
    p("The downside of becoming a landlord is, of course, the increased complexity. They’d need to deal with tenants or property management agencies, repair and management companies and more taxes. This extra headache holds back people from pursuing such investments, even when the numbers clearly point to this alternative as a valid solution for the Buy-vs-Rent dilemma."),
    hr(),
    hr(),
  )
}
