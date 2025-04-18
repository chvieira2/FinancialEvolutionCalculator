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

    h2(paste0(couple_name, ", high income earners")),
    p(em(paste0(couple_name, " are a 30-year-old high-income family in Germany. With earnings in the top income bracket, they've built substantial savings through their careers. Now they're evaluating whether to transition from renting to property ownership."))),
    p("This analysis examines their financial trajectory over the next 50 years, addressing the central question:", strong("Is purchasing a home in Berlin financially advantageous"), "compared to continuing to rent or becoming property investors?"),

    h3("Financial Goals"),
    tags$ul(
      tags$li(tags$u("Lifestyle"), "-", NAVTAB_CONTENT$SCENARIOS$LIFESTYLE_DESCRIPTION),
      tags$li(tags$u("Living Costs"), "-", "Because their monthly living expenses already provide them with a comfortable life, having more wealth wouldn't significantly improve their life quality and isn't a primary goal."),
      tags$li(tags$u("Work and Retirement"), "-", NAVTAB_CONTENT$SCENARIOS$LATE_LIFE_DESCRIPTION),
      tags$li(tags$u("Generational Wealth"), "-", NAVTAB_CONTENT$SCENARIOS$GENERATIONAL_WEALTH_DESCRIPTION),
      tags$li(tags$u("Donations"), "-", NAVTAB_CONTENT$SCENARIOS$EXTRA_CASH_DESCRIPTION)
    ),
    hr(),



    h3("Personal Finances Parameters"),
    tags$ul(
      tags$li(
        tags$u("Income"),
        paste0("- Their combined net monthly income is ", format(rent_net_annual_income, big.mark=","), "€. Career progression is modeled with:"),
        tags$ul(
          tags$li(paste0("Initial growth rate: ", rent_salaries_growth_start_career, "% annually (reflecting promotions, education, and experience)")),
          tags$li("Gradual reduction to half this rate as career opportunities plateau"),
          tags$li("All income figures represent post-tax, post-mandatory contribution amounts (e.g.: health insurance)")
        )
      ),
      tags$li(tags$u("Living standards"), paste0("- They don't care much about cutting costs and live a comfortable life, spending ", format(rent_living_style_costs , big.mark=","), "€ each month in groceries, clothing, commuting, entertainment, holidays and all other living costs. As life changes, the types of expenses are expected to change while the overall costs of living are expected to remain the same (after correcting for inflation). They have no room to further reduce these expenses.")),
      tags$li(tags$u("Retirement"), paste0("- They hope to retire lastest at the age of 70 in 2065.  Despite having private pension schemes included in their living costs, the drop in salary will still be significant (", rent_expected_salary_reduction_retirement, "%), given their high salaries.")),
      tags$li(tags$u("Family Size"), paste0("- They are pregnant with their first child and plan to have a second one right after that.")),
      tags$li(tags$u("Passive Investments"), paste0("- They invest all their savings in accumulating ETFs with ", rent_expected_return_on_investment, "%/year gross return and will change it into safer investment options over time, receiving ", rent_expected_conservative_return_on_investment, "%/year returns by retirement age. Any investment gains are taxed at ", rent_capital_gains_tax_rate, "% in Germany upon withdraw and after considering Vorabpauschale tax that is charged every year.")),
      tags$li(tags$u("Taxes"), paste0("- They pay the lowest income tax in Germany (", rent_income_tax, "% + solidarity surcharge tax) and no voluntary taxes to the church.")),
      tags$li(tags$u("Debts"), paste0("- Any time they go into debt (emergency reserve below zero), their only way of financial support is to contract a debt with a credit card while paying high interest rate (", rent_interest_rate_cash_flow_debt, "%/year).")),
      tags$li(tags$u("Lump Sums"), paste0("- They expect to inherit some money from their parents (", format(rent_lump_sum_1, big.mark=","), "€, around ", rent_lump_sum_1_year, "), but also to have other large expenses at some point (", format(rent_lump_sum_2, big.mark=","), "€, around ", rent_lump_sum_2_year, ")."))
    ),
    hr(),


    h3("Options on the Table"),
    p(paste0(couple_name, " are considering three options for their future:")),
    tags$ul(
      tags$li(tags$u("Renters"), paste0("- They continue renting a modern, large 4-room flat in Berlin (roughly evaluated at ", format(round(8000/23*rent_rent_month, -3), big.mark=",", scientific = FALSE), "€) close to the city center for ", format(rent_rent_month, big.mark=","), "€ + ", format(rent_fixed_housing_costs, big.mark=","), "€/month extra expenses. They expect this rent to increase over time (", rent_rental_prices_growth, "%/year). Upon retirement, they plan to use all their savings to buy a property to live in, without needing a bank load, and that is similar to the property bought in the homeowner scenario.")),
      tags$li(tags$u("Homeowners"), paste0("- They buy and move into a 3-room flat in Berlin for ", format(homeowner_value_today, big.mark=","),"€, not in the city centre but also not far. They use all their savings besides their emergency reserve as downpayment and contract a mortgage from the bank (", homeowner_initial_interest_rate, "%/year interest rate, repayment rate of ", homeowner_principal_repayment_rate, "%), but do not need to borrow money from their families and friends. They expect this property to increase in value over time (", homeowner_value_growth, "%/year). They do not amortize the loan, investing postive cash flow passively instead.")),
      tags$li(tags$u("Landlords"), paste0("- They buy and lease out a small 2-room flat in Berlin for ", format(landlord_value_today, big.mark=","),"€, not in the city centre but also not far. They use all their savings besides their emergency reserve as downpayment and contract a mortgage from the bank (", landlord_initial_interest_rate, "%/year interest rate, repayment rate of ", landlord_principal_repayment_rate, "%), but do not need to borrow money from their families and friends. They expect this property to increase in value over time (", landlord_value_growth, "%/year). They do not amortize the loan, investing postive cash flow passively instead. Upon retirement, they plan to use all their savings to buy a property to live in, without needing a bank load or selling their investment property, and that is similar to the property bought in the homeowner scenario."))
    ),
    p("The Financial Evolution Calculator considers all taxes they would need to pay in each scenario, the income loss of not having a tenant temporarily, possible income tax deductibles, the mortgage payment and payments for the family/friends loan, property management fees and property maintenance costs."),
    hr(),


    h3("Total Asset Evolution"),
    p(paste0("The total asset evolution provides a comprehensive view of ", couple_name, "'s financial trajectory across different housing scenarios (Figure 1). It represents their net worth as the sum of all assets (cash, investments, property) minus all debts (mortgage, credit card debt).")),
    tags$ul(
      tags$li(tags$u("Long-term Trend:"), " All three scenarios show overall growth with wealth plateauing or slowing after retirement"),
      tags$li(tags$u("Early-Year Differences:"),
              tags$ul(
                tags$li("Renters:", " Positive asset value from the beginning with steady, consistent growth"),
                tags$li("Homeowners:", " Negative initial asset value requiring 9 years to break even due to mortgage debt"),
                tags$li("Landlords:", " Negative initial asset value requiring 4 years to break even (less severe than homeowners)")
              )
      ),
      tags$li(tags$u("Late-life Stability:"), paste0(" All scenarios reach similar asset values by retirement age (", rent_expected_year_retirement - 2025 + 30, "-years-old in ", rent_expected_year_retirement,
                                                     "), indicating financial security and generational wealth"))
    ),
    p("External financial events like inheritances or major expenses (shown as jumps in the graphs) have minimal impact on the overall trajectory, demonstrating the robustness of all three approaches."),
    p(strong("A - Renters")), renderUI(renderPlot(rent_AssetEvolution)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_AssetEvolution)),
    p(strong("C - Landlords")), renderUI(renderPlot(landlord_AssetEvolution)),
    p(em(paste0("Figure 1 - ", couple_name, ". Total asset value evolution in three scenarios: as renters (A), as homeowners (B), or as landlords (C)."))),
    br(),
    hr(),



    h3("Financial Metrics"),
    p(paste0("Beyond total assets, examining specific financial metrics provides deeper insight into ", couple_name, "'s financial health under each housing scenario (Figure 2):")),

    h5("Investment Growth Patterns"),
    tags$ul(
      tags$li(tags$u("Investment Accumulation (Yellow Line)"), " - Remarkable variance across scenarios"),
      tags$ul(
        tags$li("Renters:", " Early and consistent investment growth, reaching highest value among scenarios even after purchasing a home for retirement"),
        tags$li("Homeowners:", " Delayed investment accumulation with significant growth only in later years"),
        tags$li("Landlords:", " Moderate early investment growth, positioned between the other scenarios")
      ),
      tags$li(tags$u("Investment Timing")),
      tags$ul(
        tags$li("Renters:", " Steady accumulation throughout their careers"),
        tags$li("Homeowners:", " Bulk of investments (60%) occur in the decade before retirement (2056-2065)"),
        tags$li("Landlords:", " More balanced accumulation with a moderate early-career delay")
      )
    ),

    h5("Cash Flow Dynamics"),
    tags$ul(
      tags$li(tags$u("Monthly Cash Flow (Blue Line)"), " - Reflects positive balance between expenses (Red line) and income (Green line):"),
      tags$ul(
        tags$li("Renters:", " Consistently positive from the beginning"),
        tags$li("Homeowners:", " Frequently negative in early years, requiring emergency funds"),
        tags$li("Landlords:", " Frequently negative in early years, requiring emergency funds, but less severe than homeowners")
      ),
      tags$li(tags$u("Emergency Reserve (Orange Line)"), " - Shows financial resilience:"),
      tags$ul(
        tags$li("Renters:", " Stable emergency fund with minimal drawdowns"),
        tags$li("Homeowners:", " Frequent emergency fund usage in early years"),
        tags$li("Landlords:", " Occasional emergency fund usage but less severe than homeowners")
      ),
      tags$li(tags$u("Mortgage Payoff (2053)"), ": In the homeowner scenario, completing mortgage payments creates a dramatic boost to monthly cash flow and accelerates investment growth. This boost is less dramatic in the landlord scenario, as the mortgage for the investment property is smaller."),
      tags$li(tags$u("Empty Nest Effect"), ": Children leaving home reduces expenses across all scenarios, slightly boosting cash flow"),
      tags$li(tags$u("Retirement Transition"), paste0(": At age ", rent_expected_year_retirement - 2025 + 30, " (", rent_expected_year_retirement, "), all scenarios show similar patterns of income reduction and investment withdrawals"))
    ),

    h5("Post-Retirement Financial Security"),
    p(paste0("Upon retirement, ", couple_name, " shift to a new financial pattern in all scenarios:")),
    tags$ul(
      tags$li(tags$u("Income Reduction (Green line)"), paste0(": Salary drops to ", rent_expected_salary_reduction_retirement, "% of pre-retirement level")),
      tags$li(tags$u("Investment Withdrawals (Purple Line)"), ": Begin across all scenarios to maintain lifestyle"),
      tags$li(tags$u("Living Standard"), ": Maintained consistently across all scenarios"),
      tags$li(tags$u("Long-term Security"), ": Significant investment reserves (>€1,000,000) in all scenarios")
    ),
    p(strong("A - Renters")), renderUI(renderPlot(rent_FinMetrics)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_FinMetrics)),
    p(strong("C - Landlords")), renderUI(renderPlot(landlord_FinMetrics)),
    p(em(paste0("Figure 2 - ", couple_name, ". Evolution of main financial metrics in three scenarios: as renters (A), as homeowners (B), or as landlords (C)."))),
    br(),
    hr(),



    h3("Income and Expenses Components"),
    p(paste0("Decomposition of ", couple_name, "'s income and expenses help paint a better picture of their financial evolution (Figure 3).")),
    h5("Expense Breakdown"),
    tags$ul(
      tags$li(tags$u("Child-related costs:"), " Relatively small compared to lifestyle expenses over the first three decades"),
      tags$li(tags$u("Housing expenses:"),
              tags$ul(
                tags$li(paste0("Homeowner scenario: Mortgage payments constitute approximately 40% of total expenses")),
                tags$li(paste0("Landlord scenario: Mortgage represents only 15% of expenses. Rent accounts for roughly 25% of expenses")),
                tags$li(paste0("Renter scenario: Rent accounts for roughly 40% of expenses"))
              )
      ),
      tags$li(tags$u("Living costs:"), " Remain consistent throughout all scenarios, reflecting maintained lifestyle quality")
    ),
    h5("Income Breakdown"),
    tags$ul(
      tags$li(tags$u("Salary income:")),
      tags$ul(
        tags$li("The primary source in all scenarios, particularly during working years"),
        tags$li("Constitutes 90-95% of income during early career years gradually decreasing in relative importance as investment returns grow")
      ),
      tags$li(tags$u("Investment returns:"),
              tags$ul(
                tags$li("Renters:", " Most significant investment growth with returns becoming a substantial income source by mid-career"),
                tags$li("Homeowners:", " Minimal investment returns until mortgage is substantially paid down"),
                tags$li("Landlords:", " Moderate investment returns supplemented by rental income from tenants")
              )
      ),
      tags$li(tags$u("Rental income (Landlord scenario):"),
              tags$ul(
                tags$li(paste0("Contributes approximately 15-20% of total income")),
                tags$li("Provides consistent cash flow that grows with inflation becoming proportionally more significant after retirement")
              )
      ),
      tags$li(tags$u("Post-retirement:")),
      tags$ul(
        tags$li("By retirement, income sources shift dramatically in all scenarios"),
        tags$li("Capital gains/dividends become the dominant income source (60-70%)"),
        tags$li("Reduced salary constitutes the remainder"),
        tags$li("Landlord scenario features the most diversified post-retirement income streams")
      )
    ),
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
    h5("Testing Financial Resilience"),
    p("While these scenarios assume that everything goes according to a plan, a fundamental aspect of having a stable financial life is ", strong("resilience to unexpected changes"), ". Our sensitivity analysis examines this robustness by:"),
    tags$ul(
      tags$li("Testing each key parameter independently within a ±20% range"),
      tags$li("Identifying which variables most significantly impact long-term outcomes"),
      tags$li("Determining financial breaking points where small changes trigger disproportionate consequences")
    ),
    h5("Insensitive Parameters"),
    tags$ul(
      tags$li(strong("Minimal Impact Parameters"), " - Changes within ±20% produce negligible effects:"),
      tags$ul(
        tags$li(tags$u("Investment returns:"), "Minor impact over the long investment horizon, although more significant for renters and landlord scenarios"),
        tags$li(tags$u("Their own rent:"), "As tenants, variations to their own rent have minor impact"),
        tags$li(tags$u("Property value appreciation:"), "Affects net worth but not cash flow significantly"),
        tags$li(tags$u("First property purchase year:"), "Waiting a few years to buy a property has little impact"),
        tags$li(tags$u("Interest rate on mortgage:"), "Interest rate negotiation affects the overal cost of the mortgage but with little impact on the overall financial evolution"),
        tags$li(tags$u("Tenant rent income:"), "As landlords, variations in tenant rent have little impact on their overall financial evolution"),
      )
    ),

    h5("Sensitive Parameters"),
    p(paste0("After a threshold value, variations to some parameters results in larger than expected impact to their overall finances. Going beyond these limits leads to debt collapse – a situation where their debt is not paid off fast enough and generates even more debt.")),
    tags$ul(
      tags$li(strong("Threshold Effect Parameters"), " - Critical values beyond which financial stability deteriorates rapidly:"),
      tags$ul(
        tags$li(tags$u("Living costs:"), paste0("Increases beyond ", format(round(0.1*homeowner_living_style_costs), big.mark=","), "€ extra (10%) in the homeowner scenario create disproportionate (50%) long-term impacts")),
        tags$li(tags$u("Career growth rate:"), "Promotions are needed to maintain financial trajectory"),
        tags$li(tags$u("Home purchase price:"), "In homeowner scenario, exceeding budget by >15% creates significant long-term strain")
      )
    ),
    p(paste0("Debt collapse is a distant risk for ", couple_name, ", as they have large safety margins in which parameters can vary and could easily reduce living costs if needed. Such margins are larger in the renter scenario, than in the landlord scenario than in the homeowner scenario.")),
    p("Overall, the financial evolution of", couple_name, "is robust to significant changes in most parameters. Still, they achieve highest financial resilience living as renters among all scenarios and within the margins tested."),
    p(strong("A - Renters")), renderUI(renderPlot(rent_SensitivityAnalysis,
                                                  height = rent_SensitivityAnalysis_height)),
    p(strong("B - Homeowners")), renderUI(renderPlot(homeowner_SensitivityAnalysis,
                                                     height = homeowner_SensitivityAnalysis_height)),
    p(strong("C - Landlords")), renderUI(renderPlot(landlord_SensitivityAnalysis,
                                                    height = landlord_SensitivityAnalysis_height)),
    p(em(paste0("Figure 4 - ", couple_name, ". Sensitivity analysis on total asset value of single parameter perturbations."))),
    br(),
    hr(),



    h3("To own or not to own a property?"),
    div(
      class = "alert alert-info",
      p(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE_SCENARIOS)
    ),

    h5("Risk-Return Profile Comparison"),
    p(paste0("One common argument in favour of buying a home instead of renting it is that it provides a safe retirement. In fact, this is what we observe for ", couple_name, " in the homeowner scenario but in both other scenarios too.")),
    p(paste0("From an investment perspective, it is a limiting decision to invest into a single asset. Besides lacking diversification, they would fear losing their jobs, getting sick, or been hit by the next market downturn while mortgage payments are due.")),
    p(paste0("Investing a smaller fraction of assets into a cheaper property and renting it out could be a more balanced option. Becoming a landlord comes, however, with significant additional costs and risks, such as the need to manage tenants, property management agencies, repair and management companies, and more taxes.")),

    p("Let's better understand the risk-return profile of each scenario:"),
    tags$ul(
      tags$li(tags$u("Homeownership Risk Profile"), " - Concentration of wealth in a single asset class and geographic location:"),
      tags$ul(
        tags$li(strong("Elevated financial stress"), " during the first decade (2025-2035)"),
        tags$li(strong("Limited liquidity"), " throughout mortgage period (30+ years)"),
        tags$li(strong("Single-asset risk"), " - dependent on local property market performance"),
        tags$li(strong("Career constraint"), " - reduced ability to pursue opportunities requiring relocation")
      ),
      tags$li(tags$u("Landlord Risk Profile"), " - Balanced exposure to property and financial markets:"),
      tags$ul(
        tags$li(strong("Moderate financial stress"), " during the first 5-7 years"),
        tags$li(strong("Property management complexity"), " - tenant relationships, maintenance coordination"),
        tags$li(strong("Diversified income streams"), " - rental income plus career earnings"),
        tags$li(strong("More complex tax situation"), " requiring additional administrative attention")
      ),
      tags$li(tags$u("Renter Risk Profile"), " - Maximum financial flexibility with investment diversification:"),
      tags$ul(
        tags$li(strong("Market-based housing cost exposure"), " - threatened by rent increases"),
        tags$li(strong("Maximum liquidity"), " and investment diversification through exposure to financial markets"),
        tags$li(strong("Geographic flexibility"), " - ability to relocate for career or lifestyle reasons"),
        tags$li(strong("Minimal administrative burden"), " - no property management responsibilities")
      )
    ),


    h5("Determining the Optimal Path"),
    p("To identify", strong("the best scenario for this couple"), ", we should evaluate each option against their stated priorities:"),
    tags$ul(
      tags$li(strong("Primary goals:"), "Financial stability, reduced working hours, earliest possible retirement while maintaining lifestyle"),
      tags$li(strong("Secondary goals:"), "Increased charitable giving and building generational wealth")
    ),
    p("Let's analyze how each scenario addresses these specific objectives:"),
    tags$ul(
      tags$li(tags$u("Savings are their financial safety net:"), " As renters or landlords, they accumulate savings from early on, unlike in the homeowner scenario. Therefore, to achieve financial stability, it might be better to not become homeowners this early in their lives to stay on the safe side."),
      tags$li(tags$u("Working less for same comfort:"), " To reduce working hours would inevitably decrease their salaries, or how much their salaries increase every year as they achieve less promotions. As renters they have the highest robustness to diminished Annual Salary Growth due to their savings quickly delivering capital gains (or dividends), effectively working as a second source of income. Similarly, as renters, they are the most resilient also against increased living costs, suggesting that they could increase spending with donations."),
      tags$li(tags$u("Early retirement is possible:"), " They retire with signicantly more savings as renters than in the other scenarios. Instead of accumulating this extra value, they could have chosen to retire earlier."),
      tags$li(tags$u("Emotional attachment:"), " Living in the home they own could create emotional attachment and feelings of belonging to their surrounding communities. Emotional factors like pride of ownership, personalization freedom, and community attachment play important roles.")
    ),
    div(
      style = "background-color: #f8f9fa; border-left: 5px solid #007bff; padding: 15px; margin: 20px 0;",
      h4("Critical Insight: Financial Flexibility"),
      p(paste0("The most striking difference between scenarios isn't the final outcome but the journey to get there. As renters, ", couple_name, " maintain greater financial flexibility throughout their 30s and 40s, potentially enabling:"),
        tags$ul(
          tags$li("Career changes or entrepreneurial ventures with less financial risk"),
          tags$li("Extended parental leave options during critical child development years"),
          tags$li("Earlier retirement by 3-5 years"),
          tags$li("Greater capacity for charitable giving")
        )
      ),
      p(tags$u("Golden handcuffs effect:"), " Homeowners often feel constrained from making life changes due to mortgage obligations, especially during the first decade when their financial cushion is minimal."),
      p("That said, the decision to become homeowners or landlords is not a bad one but fall behind in terms of their personal goals in comparison to living as renters. Still, all three scenarios could fullfill their financial goals.")
    ),
    h5("Conclusion for", couple_name),
    tags$ul(
      tags$li(strong("Most financially advantageous:"), " Remaining renters provides the highest liquidity, earliest potential retirement, and greatest resilience to unexpected changes"),
      tags$li(strong("Middle path:"), " The landlord scenario offers property market exposure with less risk than full homeownership"),
      tags$li(strong("Valid but less optimal:"), " Direct homeownership remains viable but constrains early-career financial flexibility")
    ),
    hr(),
    br(),
    br()
  )
}

