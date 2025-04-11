scenario1Content <- function() {

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
                     "rent_higher_salary", "rent_lower_rent", "rent_higher_salary_lower_rent",
                     "homeowner", "landlord")) {
    scenario_name <- paste0("low_wage_family_", scenario)
    config <- safelyLoadConfig(file.path("config", "templates",
                                         paste0("inputs_", scenario_name, ".yaml")))
    plot_data <- read.csv(file.path("article",
                                    paste0("calculations_", scenario_name, ".csv")))

    assign(paste0(scenario, "_AssetEvolution"),
           renderUI(renderPlot(generateYearlyAssetProgressionPlot(
             plot_data,
             config
             ))))
    assign(paste0(scenario, "_FinMetrics"),
           renderUI(renderPlot(generateFinancialMetricsPlot(
             plot_data,
             legend_bool = TRUE
             ))))
    assign(paste0(scenario, "_IncomeComp"),
           renderUI(renderPlot(generateStackedAreaPlot(
             plot_data = plot_data,
             plot_type = "income",
             legend_bool = TRUE
           ))))
    assign(paste0(scenario, "_ExpensesComp"),
           renderUI(renderPlot(generateStackedAreaPlot(
             plot_data = plot_data,
             plot_type = "expenses",
             legend_bool = TRUE
           ))))
  }

  div(
    h2("Gene and Jean, the poverty trap"),
    p(em("Gene and Jean are a 30-years old poor family in Germany. Hard-working class, working jobs with low education requirements, forced to live away from city centres due to high living costs and the housing market.")),

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
    rent_AssetEvolution,
    p(em("Figure 1 - Scenario 1, Gene and Jean. Total asset value evolution.")),
    p("Gene and Jean’s total asset value expands slowly (Figure 1), with periods of stagnation and been strongly affected by major lump sums (positive and negative). Their finances only really take off later in life, assuming they manage to maintain this level of living costs, indicating costs with kids are a significant burden."),
    p("Upon retirement at age of 70 (2065), their total asset curve stagnates, indicating that they’d be living stable financial lives so long as they keep the same lifestyle and living costs."),
    p("They hold until advanced age a significant total asset value that could be passed on to their kids as generational wealth."),
    hr(),



    h3("Financial Metrics"),
    rent_FinMetrics,
    p(em("Figure 2 - Scenario 1, Gene and Jean. Evolution of main financial metrics.")),
    p("Gene and Jean’s main financial metrics reflect their slowly evolving financial situation with a life of cutting costs and saving wherever possible (Figure 2)."),
    p("In the first decade (2025-2037), while the kids are small, they balance total expenses and income allowing for the accumulation of passive investments."),
    p("Over the following two decades (2038-2052), they struggle maintaining cash flow and are forced to withdraw from their invested capital to pay-off debts. The impact of punctual lump sums is also significant, leading to large variation to their invested capital."),
    p("In the following decade (2053-2064), at around when the kids stop presenting a significant expense, they return to a period of positive cash flow and passive investment accumulation."),
    p("Finally, from retirement on (2065-2080), their cash flow is again slightly negative, but they manage to live a stable lifestyle by offsetting cash flow gaps with returns on passive investment (80.000€) and withdrawing cash when necessary."),
    hr(),



    h3("Income and Expenses Components"),
    p(strong("A")), rent_IncomeComp,
    p(strong("B")), rent_ExpensesComp,
    p(em("Figure 3 - Scenario 1, Gene and Jean. Detailed view of the financial components of income (A) and expenses (B).")),
    p("Decomposition of Gene and Jean’s income and expenses help paint a better picture of their financial evolution. Their living costs are constant throughout the years after inflation correction, except in the years after the kids are born, reflecting a sustained lifestyle (Figure 3A)."),
    p("Rent is by far their largest expenditure, representing between 50-70% of all expenses and relatively increasing over the years. Kids also represent a significant expense throughout almost the first 3 decades."),
    p("Salary is their main source of income and investing has very little impact on their income. Capital gains from their passive investments become relatively more significant for income upon retirement with the reduction of their salaries (Figure 3B)."),
    hr(),



    h3("Sensitivity Analysis"),
    p(em("Figure 4 - Scenario 1, Gene and Jean. Sensitivity analysis on total asset value of single parameter perturbations.")),
    p("A fundamental aspect of having a stable financial life is financial robustness. That is, the ability to resist to unforeseen life changes. The scenario simulated above assumes that everything goes according to the plan, without deviations."),
    p("Sensitivity analysis identifies factors to which Gene and Jean’s finances are most sensitive by iteratively repeating the simulation upon slightly varying single parameters per iteration. This analysis shows that changing key parameters by a 20% margin has significant impacts on the couple’s financial evolution."),
    p("For example, if this family spends 40€ more than the expected 800€/month (5% variation to their monthly living costs) while keeping all other parameters the same, they will have accumulated zero total asset value by retirement age and are not able to sustain their lifestyle (Figure 4, top left plot). Spending any more than this could lead to debt collapse – a situation where their debt is not paid off fast enough and generates even more debt, exponentially increasing itself. Therefore, tightly controlling their living costs is of great importance for Gene and Jean, as they have a tiny safety margin."),
    p("To avoid debt collapse, similar analysis on their salary increase over time leads to the conclusion that Gene and Jean’s must also keep working hard to achieve the minimum required salary growth (above 2.5%). Similarly, they must also negotiate well their rental price increase rate keeping bellow 3%, if possible. Failing to achieve these hallmarks puts them at risk of debt collapse."),
    p("Although also impactful but with less extreme consequences, choosing good investments for their money also impacts their financial life, specially upon retirement."),
    hr(),



    h3("How can Gene and Jean improve their financial situation?"),
    p(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE_SCENARIOS),

    h4("Guided by Financial Goals"),
    p("To improve the financial situation of Gene and Jean we must keep in mind their goals: to have a stable financial life, to enhance their live quality by increasing expenses and to retire at later age while maintaining their lifestyle focused on family, friends and hobbies."),
    p("Gene and Jean’s financial reality has overarching impacts over their life quality. Given the current market prices in Berlin, the property they rent for a family with 2 kids (1000€/month + 120€/month extra costs) is likely distant from their workplaces, family and friends. They also live in constant financial stress as they have a real risk of debt collapse."),
    p("They already save every penny, so there is not much room for reducing their expenses further without impacting their health, which would lead to reduced income over time."),
    p("Rental increases by landlords due to real estate market development or gentrification leads to major financial stress, possibly forcing them to look for a cheaper alternative. Here, living in government-backed housing projects could be a good alternative to the private housing market."),
    p("Also, living far from city centres makes it difficult to get better education and opportunities to improve their salaries. The many hours commuting may also negatively affect their wellbeing and consequently their performance at work."),
    hr(),

    h4("Aligning Financial Goals and Decisions"),
    p(strong("A")), rent_higher_salary_AssetEvolution,
    p(strong("B")), rent_lower_rent_AssetEvolution,
    p(strong("C")), rent_higher_salary_lower_rent_AssetEvolution,
    p(em("Figure 5 - Scenario 1, Gene and Jean. Total asset value evolution in simulations of scenarios with single parameter variations. A) Salary increased from 2400€ to 2500€. B) Rent reduced from 1000€ to 950€. C) Salary increased and rent reduced.")),
    p("What could they have done differently to better align their financial goals and decisions? The Financial Evolution Calculator can be used to test multiple scenarios. For example, we can re-simulate Gene and Jean’s scenario to identify critical factors for improving their financial situation (Figure 5)."),
    p("For example, increasing their current salaries by 50€ each would strongly impact their financial evolution (Figure 5A). Not surprisingly, people in similar situations to Gene and Jean often work extra hours. Although impactful, this approach might not be a sustainable solution. Focusing on improving skills set and fighting for better salaries could higher impact long-term."),
    p("Similarly, reducing their current rent by 50€ would also improve their situation significantly (Figure 5B). Not surprisingly, people in this situation tend to move away from city centres into peripheries."),
    hr(),

    h4("Going Beyond Initial Goals"),
    p(strong("A")), landlord_AssetEvolution,
    p(strong("B")), landlord_FinMetrics,
    p(strong("C")),
    p(em("Figure 6 - Scenario 1, Gene and Jean. Scenario simulation leasing the property purchased in 2025. Total asset value evolution (A), evolution of main financial metrics (B), and sensitivity analysis on total asset value of single parameter perturbations (C).")),
    p("These two factors together point to a possible but difficult financial improvement for Gene and Jean: increasing their salaries and saving on rent should be their priorities. It also highlights how people in the lowest income range are vulnerable in society."),
    p("However, there is another alternative to consider. There seems to be a general misconception that becoming a landlord is something only rich people do. Although more difficult to obtain a bank loan when one’s salaries are low, with help of family and friends it can be possible, and the benefits are immense."),
    p("Let’s re-simulate a second scenario for Gene and Jean. Instead of trying to save and invest passively, they invest actively in real estate by purchasing a property (200.000€) with help of family and friends. Also, instead of moving in, they stay in their rental apartment (1000€/month). Their property is then leased to someone else at 500€/month rental price, following the same rental contract they have on their own rent (3% increase per year)."),
    p("The Financial Evolution Calculator considers all taxes they’d need to pay, risk of not having a tenant, tax deductible costs, mortgage for the investment property, property depreciation, management fees and maintenance costs."),
    p("Their total accumulated asset value at retirement age as landlords would be seven-fold higher than in the first scenario (550.000€ vs 80.000€) (Figure 6A). They would not experience debt (Figure 6B) and would be resilient to unexpected events throughout their lives (Figure 6C)."),
    p("Interestingly, they would have saved enough money by retirement age to purchase a second property – evaluated at 200.000€ in today’s price. That is, without a bank loan, without selling their leased property, and still having savings in their bank account after the purchase to sustain their lifestyle in retirement! Alternatively, they could also choose to not buy their home and increase their living costs and move into a bigger rented flat, improving their life quality at later age."),
    p("Interestingly, they would have saved enough money by retirement age to purchase a second property – evaluated at 200.000€ in today’s price. That is, without a bank loan, without selling their leased property, and still having savings in their bank account after the purchase to sustain their lifestyle in retirement! Alternatively, they could also choose to not buy their home and increase their living costs and move into a bigger rented flat, improving their life quality at later age."),
    p("How is this possible? It might come as a surprise, but it’s really just numbers. By becoming landlords, Gene and Jean pay their own rent (1000€/month) and house expenses (110€/year) plus the bank loan (1000€/month) and loan from friends and family (200€/month). House expenses for the investment property are paid by the tenant."),
    p("Altogether, their housing expenses in the first years are barely covered by the leftover salary after discounting living costs plus the lease income (2400 – 800 + 500 = 2100€/month). Income tax deductions from expenses with the investment property (100€/month) help significantly."),
    p("The secret is time. While their incomes and expenses increase every year, the mortgage is fixed at least until refinancing 5-20 years later (Figure 6B). Every year their cashflow becomes more positive and, while all their available cash is re-directed to pay the flat in the first years, it slowly builds savings."),
    p("From the investment perspective, Gene and Jean diversified their assets over the years, increasing financial robustness. For example, their own rent will increase every year but, so long as their leased property increases rent at a similar rate, they are immune to real estate market changes (Figure 6C)."),
    p("Also, their income does not depend only on their salaries anymore, as they receive lease. Been less dependent on salary means that they could work less and try less hard for promotions. Although, they still live a restricted life saving every penny and higher salaries could still help."),
    p("The downside of becoming a landlord is, of course, the increased complexity. They’d need to deal with tenants or property management agencies, repair and management companies and more taxes. This extra headache holds back people from pursuing such investments, even when the numbers clearly point to this alternative as a valid solution."),
    hr(),
    hr(),
  )
  }
