# Home Navigation Module

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

homeNavigationModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the tree structure
    output$home_toc <- shinyTree::renderTree({
      list(
        "Welcome" = structure("welcome_page", stopened = TRUE),
        "Calculator Parameters" = structure("parameters_page", stopened = TRUE),
        "Scenario 1: the poverty trap" = structure("scenario_1", stopened = TRUE),
        "Scenario 2: home-poor lifestyle" = structure("scenario_2", stopened = TRUE),
        "Scenario 3: wearing golden cuffs" = structure("scenario_3", stopened = TRUE),
        "About" = structure("about_page", stopened = TRUE)
      )
    })

    # Render the content based on the selected tree node
    output$home_content <- renderUI({
      selected <- unlist(shinyTree::get_selected(input$home_toc, format = "names"))

      label_to_handle <- list(
        "Welcome" = "welcome_page",
        "Calculator Parameters" = "parameters_page",
        "Scenario 1: the poverty trap" = "scenario_1",
        "Scenario 2: home-poor lifestyle" = "scenario_2",
        "Scenario 3: wearing golden cuffs" = "scenario_3",
        "About" = "about_page"
      )

      # Default selection if nothing is selected
      if (is.null(selected)) {
        selected_handle <- "welcome_page"
      } else {
        # Translate the selected label to the corresponding handle
        selected_handle <- label_to_handle[[selected]]
      }

      switch(selected_handle,
             "welcome_page" = div(
               h2("Welcome to the Financial Evolution Calculator!"),
               p(em(DISCLAIMER_FINANCIAL_ADVISE)),

               p(strong("Ever wondered how today's decisions shape your financial future? This calculator can help answer some questions about your financial future, such as:")),
               tags$ul(
                 style = "list-style-type: none; padding-left: 0;",  # Remove default bullet points
                 tags$li(icon("graduation-cap"), " Should I start working sooner or pursue more education?"),
                 tags$li(icon("baby"), " Will I be able to afford having kids?"),
                 tags$li(icon("home"), " Should I buy a property and move out of renting?"),
                 tags$li(icon("money-bill-wave"), " How much money do I need for retirement?"),
                 tags$li(icon("clock"), " Will I even retire one day?"),
                 tags$li(icon("chart-line"), " Will I be able to build generational wealth?")
               ),

               p("Ain't no time for reading? Go ahead and try the Calculator by switching the navigation tab on the top of the page."),
               hr(),




               h3("Simulate Your Financial Journey in 3 Steps"),

               p("The calculator is designed to be user-friendly, even for those without a financial background. Its interactive interface allows you to input your current financial situation and future assumptions with ease."),

               p("Follow these 3 simple steps to start the simulations:"),

               tags$ol(
                 style = "list-style-type: none; padding-left: 0;",  # Remove default bullet points
                 tags$li(icon("edit"), strong("Input Baseline"), "- Quickly set up your financial profile using smart defaults."),
                 tags$li(icon("sliders-h"), strong("Experiment Freely"), "- Adjust over 40 parameters and see real-time results."),
                 tags$li(icon("chart-bar"), strong("Discover Insights"), "- Explore interactive visualizations to uncover trends and non-linear effects.")
               ),

               p(em("Pro-tip: Further information can be found by hovering over the info icon next to each parameter in the calculator page.")),
               hr(),




               h3("Financial Timeline Scenario"),
               p("Using your selected parameters, the calculator generates a financial timeline that covers four key areas:"),
               tags$ul(
                 style = "list-style-type: none; padding-left: 0;",  # Remove default bullet points
                 tags$li(icon("piggy-bank"), strong("Liquid and Illiquid Assets"), "- Cash, emergency funds, liquid investments, property equities."),
                 tags$li(icon("user-graduate"), strong("Human Capital"), "- Education and skills, career trajectory models."),
                 tags$li(icon("child"), strong("Life Variables"), "- Childcare costs, unexpected expenses like health shocks, retirement."),
                 tags$li(icon("chart-pie"), strong("Macroeconomics Factors"), "* - Taxes, inflation, interest rates, market volatility.")
               ),

               p(em("*Some Macroeconomic Factors, specially the tax system, are based on Germany in 2025.")),

               p("Year-by-year, we calculate the financial evolution based on these inputs and assumptions. The logic is simple: how much money you have today, how much you expect to spend today and how much you expect to earn today will define how much money you'll have tomorrow."),
               hr(),




               h3("Visualising your Financial Journey"),
               p("The calculator visually represents your financial evolution over time, showing how your Total Asset Value changes based on your inputs and assumptions."),

               p("A detailed view of key financial metrics, including Total Expenses, Total Income, Cash Flow, Emergency Reserve, Investment Withdrawals, and Total Invested provide a clearer picture of your financial health and help you understand the impact of your decisions."),

               p("Expenses and income are broken down into key components allowing you to see where your money is going and how different income sources contribute to your overall financial picture."),

               p("Finally, the Sensitivity Analysis feature lets you test how changes in key parameters impact your financial projections. By adjusting these parameters, you can see how sensitive your financial future is to different scenarios."),
               hr(),




               h3("Start Now With Template Scenarios!"),

               p("It is easy to get started: click on the 'Calculator' tab in the navigation bar and set the parameters. The calculations will dynamically reflect your changes."),

               p("And if you don't know your financial parameters, no problem! Try out the pre-built template scenarios to get started. It's like playing The Sims, but  with finances instead of virtual homes ;)"),


               tags$ul(
                 tags$li(strong("Low-income family"), "- A family with limited resources living in a rented property, focusing on basic needs and savings while planning for children and retirement."),
                 tags$li(strong("Mid-income family"), "- A family with average income and expenses, balancing basic needs, savings and exploring investment options for financial stability while planning for children and retirement. This scenario includes three variations: renting, homeownership, and becoming a landlord."),
                 tags$li(strong("High-income family"), "- A family with above-average income and expenses, focusing on investments for financial stability, early retirement, and generational wealth while also planning to have kids. This scenario also includes three variations: renting, homeownership, and becoming a landlord.")
               ),
               p("These scenarios are based on typical financial profiles for low-, mid-, and high-income families in Germany in 2025. You can adjust the parameters to fit your own situation and see how they affect your financial evolution."),
               p("For a detailed analysis of each scenario, check the side menu and explore the individual breakdowns. Or load each of these templates in the calculator and explore them yourself."),
               hr(),
               hr()

             ),






             "parameters_page" = div(
               h2("Calculator Parameters"),
               p("See below a detailed explanation of each parameter used by the Financial Evolution Calculator."),
               hr(),

               # Load and process the YAML configuration
               {
                 config <- safelyLoadConfig("config/templates/inputs_mid_wage_family_landlord.yaml")

                 if (!is.null(config)) {
                   # Iterate over each category in the YAML file
                   lapply(names(config), function(category) {
                     category_data <- config[[category]]

                     # Handle the "properties" category differently
                     if (category == "properties") {
                       div(
                         h3("Properties"),
                         lapply(category_data, function(property) {
                           div(
                             h4(property$name),  # Property name
                             lapply(property$inputs, function(input) {
                               div(
                                 br(),  # Line break
                                 strong(input$label),  # Bold label
                                 ": ",
                                 HTML(input$description),  # Render description as HTML to support links
                                 br()  # Additional spacing between parameters
                               )
                             }),
                             hr()  # Separator between properties
                           )
                         })
                       )
                     } else {
                       # Handle other categories
                       category_title <- category_data$title
                       inputs <- category_data$inputs

                       div(
                         h3(category_title),
                         lapply(inputs, function(input) {
                           div(
                             br(),  # Line break
                             strong(input$label),  # Bold label
                             ": ",
                             HTML(input$description),  # Render description as HTML to support links
                             br()  # Additional spacing between parameters
                           )
                         }),
                         hr()  # Separator between categories
                       )
                     }
                   })
                 } else {
                   div(p("Error loading configuration. Please check the YAML file."))
                 }
               },
               hr()
             ),






             "scenario_1" = div(
               h2("Simulation scenario 1: Gene and Jean, the poverty trap"),
               p("Gene and Jean are a poor family in Germany. Hard-working class, working jobs with low education requirements, forced to live away from city centres due to high living costs and the housing market."),
               p(SCENARIO_LIFESTYLE_DESCRIPTION),
               p("Because they spend as little as possible saving wherever they can, having more wealth would improve their life quality and that’s a main financial goal."),
               p(SCENARIO_EXTRA_CASH_DESCRIPTION),
               p(SCENARIO_LATE_LIFE_DESCRIPTION),
               hr(),



               h3("Personal Finances Parameters"),
               tags$ul(
                 tags$li(strong("Income"), "- Their net income together is 2400€/month. Working on consumer-based jobs, their incomes are expected to increase at an annual rate of 2.5%/year at the start of their careers (promotions, more education, more clients, more experience), and this rate slows down over time to half of that, as options for growth saturate."),
                 tags$li(strong("Living standards"), "- They try to keep a simpler life, spending 800€ each month in groceries, clothing, commuting, entertainment, holidays and all other living costs. As life changes, the types of expenses are expected to change while the overall costs of living are expected to remain the same (after correcting for inflation). They have no room to further reduce these expenses."),
                 tags$li(strong("Retirement"), "- They hope to retire at the age of 70 in 2065. Due to the government pension scheme, they’ll receive a pension, but their salaries will still drop by 30%."),
                 tags$li(strong("Family Size"), "- They are pregnant with their first child and plan to have a second one right after that."),
                 tags$li(strong("Housing"), "- They rent a small 3-room flat in the outskirts of Berlin (1000€ + 120€/month extra expenses for housing). The rent is expected to grow as the real estate market grows (3%/year), while extra expenses raise as per the inflation (2,5%/year). They could find a cheaper flat. However, due to their growing family, they need the extra space."),
                 tags$li(strong("Passive Investments"), "- They invest all their savings in accumulating ETFs with 10%/year gross return and will change it into safer investment options over time, receiving 6%/year returns by retirement age. Any investment gains are taxed at 25% in Germany upon withdraw and after considering Vorabpauschale tax that is charged every year."),
                 tags$li(strong("Taxes"), "- They pay the lowest income tax in Germany (14% + solidarity surcharge tax) and no voluntary taxes to the church."),
                 tags$li(strong("Debts"), "- Any time they go into debt (emergency reserve below zero), their only way of financial support is to contract a debt with a credit card while paying high interest rate (12%/year)."),
                 tags$li(strong("Lump Sums"), "- They expect to inherit some money from their parents (50.000€, around 2045), but also to have other large expenses at some point (25.000€, around 2050).")
               ),
               hr(),



               h3("Total Asset Evolution"),
               p(em("Figure 1 - Scenario 1, Gene and Jean. Total asset value evolution.")),
               p("Gene and Jean’s total asset value expands slowly (Figure 1), with periods of stagnation and been strongly affected by major lump sums (positive and negative). Their finances only really take off later in life, assuming they manage to maintain this level of living costs, indicating costs with kids are a significant burden."),
               p("Upon retirement at age of 70 (2065), their total asset curve stagnates, indicating that they’d be living stable financial lives so long as they keep the same lifestyle and living costs."),
               p("They hold until advanced age a significant total asset value that could be passed on to their kids as generational wealth."),
             hr(),



             h3("Financial Metrics"),
             p(em("Figure 2 - Scenario 1, Gene and Jean. Evolution of main financial metrics.")),
             p("Gene and Jean’s main financial metrics reflect their slowly evolving financial situation with a life of cutting costs and saving wherever possible (Figure 2)."),
             p("In the first decade (2025-2037), while the kids are small, they balance total expenses and income allowing for the accumulation of passive investments."),
             p("Over the following two decades (2038-2052), they struggle maintaining cash flow and are forced to withdraw from their invested capital to pay-off debts. The impact of punctual lump sums is also significant, leading to large variation to their invested capital."),
             p("In the following decade (2053-2064), at around when the kids stop presenting a significant expense, they return to a period of positive cash flow and passive investment accumulation."),
             p("Finally, from retirement on (2065-2080), their cash flow is again slightly negative, but they manage to live a stable lifestyle by offsetting cash flow gaps with returns on passive investment (80.000€) and withdrawing cash when necessary."),
             hr(),



             h3("Income and Expenses Components"),
             p(em("Figure 3 - Scenario 1, Gene and Jean. Detailed view of the financial components of income and expenses.")),
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
             p(DISCLAIMER_FINANCIAL_ADVISE_SCENARIOS),

             h4("Guided by Financial Goals"),
             p("To improve the financial situation of Gene and Jean we must keep in mind their goals: to have a stable financial life, to enhance their live quality by increasing expenses and to retire at later age while maintaining their lifestyle focused on family, friends and hobbies."),
             p("Gene and Jean’s financial reality has overarching impacts over their life quality. Given the current market prices in Berlin, the property they rent for a family with 2 kids (1000€/month + 120€/month extra costs) is likely distant from their workplaces, family and friends. They also live in constant financial stress as they have a real risk of debt collapse."),
             p("They already save every penny, so there is not much room for reducing their expenses further without impacting their health, which would lead to reduced income over time."),
             p("Rental increases by landlords due to real estate market development or gentrification leads to major financial stress, possibly forcing them to look for a cheaper alternative. Here, living in government-backed housing projects could be a good alternative to the private housing market."),
             p("Also, living far from city centres makes it difficult to get better education and opportunities to improve their salaries. The many hours commuting may also negatively affect their wellbeing and consequently their performance at work."),
             hr(),

             h4("Aligning Financial Goals and Decisions"),
             p(em("Figure 5 - Scenario 1, Gene and Jean. Total asset value evolution in simulations of scenarios with single parameter variations. A) Salary increased from 2400€ to 2500€. B) Rent reduced from 1000€ to 950€. C) Salary increased and rent reduced.")),
             p("What could they have done differently to better align their financial goals and decisions? The Financial Evolution Calculator can be used to test multiple scenarios. For example, we can re-simulate Gene and Jean’s scenario to identify critical factors for improving their financial situation (Figure 5)."),
             p("For example, increasing their current salaries by 50€ each would strongly impact their financial evolution (Figure 5A). Not surprisingly, people in similar situations to Gene and Jean often work extra hours. Although impactful, this approach might not be a sustainable solution. Focusing on improving skills set and fighting for better salaries could higher impact long-term."),
             p("Similarly, reducing their current rent by 50€ would also improve their situation significantly (Figure 5B). Not surprisingly, people in this situation tend to move away from city centres into peripheries."),
             hr(),

             h4("Going Beyond Initial Goals"),
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
             ),








             "scenario_2" = div(
               h2("Scenario 2: Alex and Max, the home-poor lifestyle"),
               p("Alex and Max are a middle-income family in Berlin, Germany. They likely work on technical jobs, services or other functions requiring some level of higher education. Their salaries are directly linked to their performance."),
               p(SCENARIO_LIFESTYLE_DESCRIPTION),
               p("Because they still must closely limit their monthly expenses, having more wealth could improve their life quality."),
               p(SCENARIO_EXTRA_CASH_DESCRIPTION),
               p(SCENARIO_LATE_LIFE_DESCRIPTION),
               hr(),



               h3("Personal Finances Parameters"),
               tags$ul(
                 tags$li(strong("Income"), "- Their net income together is 4500€/month. Their incomes are expected to increase at an annual rate of 3%/year at the start of their careers (promotions, more education, more clients, more experience), and this rate slows down over time to half of that, as options for growth saturate."),
                 tags$li(strong("Living standards"), "- They try to save but can afford some comfort, spending 2200€ each month in groceries, clothing, commuting, entertainment, holidays, private pension schemes and all other living costs. As life changes, the types of expenses are expected to change while the overall costs of living are expected to remain the same (after correcting for inflation). They can reduce these expenses if ever needed, but significantly impacting their living standards."),
                 tags$li(strong("Retirement"), "- They hope to retire at the age of 70 in 2065 and expect their salaries to drop by 50%."),
                 tags$li(strong("Family Size"), "- They are pregnant with their first child and plan to have a second one right after that."),
                 tags$li(strong("Housing"), "- They used to rent a small 3-room flat in Berlin (1500€ + 220€/month extra expenses for housing) but just recently bought a small flat further away from the city centre for 250.000€. For that, they contracted a mortgage from the bank (3.7%/year interest rate, repayment rate of 2%), used all their savings besides their emergency reserve, and borrowed money (25.000€) from their families. They expect this property to increase in value over time (3%/year). They will not use positive yearly cash flow to amortize the loan, investing it passively instead."),
                 tags$li(strong("Passive Investments"), "- They invest all their savings in accumulating ETFs with expected 10%/year gross return and will change it into safer investment options over time, expecting 6%/year returns by retirement age. Any investment gains are taxed at 25% in Germany upon withdraw and after considering the advanced tax (Vorabpauschale) that is charged every year."),
                 tags$li(strong("Taxes"), "- They pay medium income tax in Germany (25% + solidarity surcharge tax) and no voluntary taxes to the church."),
                 tags$li(strong("Debts"), "- Any time they go into debt (emergency reserve below zero), their only way of financial support is to contract a debt with a credit card while paying high interest rate (12%/year)."),
                 tags$li(strong("Lump Sums"), "- They expect to inherit some money from their parents (50.000€, around 2045), but also to have other large expenses at some point (25.000€, around 2050).")
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
               p(DISCLAIMER_FINANCIAL_ADVISE_SCENARIOS),

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
             ),








             "scenario_3" = div(
               h2("Scenario 3: Jack and Beck, wearing golden cuffs"),
               p("Jack and Beck are a high-income family in Germany. They likely work on knowledge jobs requiring high level of education. Their salaries depend mostly on their education rather than their performance. Their income is among the highest of all Germany, but they are still part of the working class meaning they do not live off inherited assets. Their salaries allow them to save and buy a property in Berlin not far from the city centre."),
               p(SCENARIO_LIFESTYLE_DESCRIPTION),
               p("Because their monthly living expenses already provide them with a comfortable life, having more wealth wouldn’t significantly improve their life quality and isn’t a primary goal."),
               p(SCENARIO_EXTRA_CASH_DESCRIPTION),
               p(SCENARIO_LATE_LIFE_DESCRIPTION),
               hr(),



               h3("Personal Finances Parameters"),
               tags$ul(
                 tags$li(strong("Income"), "- Their net income together is 7800€/month. Their incomes are expected to increase at fast annual rate (5.5%/year) faster at start of career as they grow (promotions, more education, more clients, more experience), and this rate slows down over time as they saturate their growth options (0.5%/year)."),
                 tags$li(strong("Living standards"), "- They don’t care much about saving and live a comfortable life, spending 4200€ each month in groceries, clothing, commuting, entertainment, holidays and all other living costs. As life changes, the types of expenses will change as well but the overall costs of living for a similar life standard remain the same (after correcting for inflation). They can adapt these expenses if ever needed."),
                 tags$li(strong("Retirement"), "- They hope to retire at the age of 70 in 2065. Due to the government pension scheme and their private pension schemes, they’ll receive a pension. Despite having private pension schemes included in their living costs, the drop in salary will still be significant (70%), given their high salaries."),
                 tags$li(strong("Family Size"), "- They are pregnant with their first child and plan to have a second one right after that."),
                 tags$li(strong("Housing"), "- They used to rent a big 3-room flat in the centre of Berlin (2000€ + 220€/month extra expenses for housing) but just recently bought a large 3-room flat in Berlin for 600.000€, not in the city centre but also not far. For that, they contracted a mortgage from the bank (3.7%/year interest rate, repayment rate of 2%) and used all their savings besides their emergency reserve, but did not need to borrow money from their families. They expect this property to increase in value over time (4%/year). They will use positive yearly cashflow to pay the mortgage as soon as possible and at the maximum allowed amortization rate (5%/year), whenever their emergency reserve is full."),
                 tags$li(strong("Passive Investments"), "- They invest all their savings in accumulating ETFs with 10%/year gross return and will change it into safer investment options over time, receiving 4%/year returns by retirement age. Any investment gains are taxed at 25% in Germany upon withdraw and after considering Vorabpauschale tax that is charged every year."),
                 tags$li(strong("Taxes"), "- They pay high income tax in Germany (42% + solidarity surcharge tax) and no voluntary taxes to the church."),
                 tags$li(strong("Debts"), "- Any time they go into debt, their only way of financial support is to contract a debt with a credit card while paying high interest rate (12%/year)."),
                 tags$li(strong("Lump Sums"), "- They expect to inherit some money from their parents (50.000€, around 2045), but also to have other large expenses at some point (25.000€, around 2050).")
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
               p(DISCLAIMER_FINANCIAL_ADVISE_SCENARIOS),

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
             ),

             "about_page" = div(
               h2("About - Financial Evolution Calculator"),
               p(DISCLAIMER_FINANCIAL_ADVISE),
               hr(),


               h2("Development Team and Collaborations"),
               HTML(
                 'This page and the calculator behind it have been developed by chvieira2. Open source code is available <a href="https://github.com/chvieira2/FinancialEvolutionCalculator" target="_blank">here</a>. Please give credit where credit is due.'
               ),
               p("Feel free to open an issue, create a branch and submit change requests, and contact me directly on GitHub. I am always open to feedback, suggestions, and collaboration opportunities."),
               p(em("I received feedback that users would like to adapt the calculator to other countries than Germany. I am open to this idea, but cannot do it myslef due to lack of expertise in other countries' tax systems. If you are interested in collaborating on this, please reach out to me.")),
               hr(),






               h2("Technical specifications"),
               p("The calculator was developed in R programming language and the web page was created using RShinny."),

               p(strong("Backend development")),
               p("The Calculator is an R6 Class object called DataProcessorClass that inherits from or instanciates other R6 Class objects containing the calculation steps for each section. It is designed to be modular, allowing for easy addition of new features and functionalities in the future."),
               p("Among its components, the BaseCalculator class contains the basic functions for data manipulation while the GeneralCalculator, RentalCalculator, PropertyCalculator, FinancialBalanceCalculator and PassiveInvestmentCalculator classes implement their own specific functions."),
               p("An RShiny app provides the interactive user interface and is hosted in shinyapps.io, an open access server provided by the Posit team. The app is designed to be user-friendly and intuitive, allowing users to easily navigate through the various features and functionalities of the calculator."),

               p(strong("Algorithmic logic and code compatibility")),
               p("Calculations for each year are execute sequentially, meaning that the results of one calculation are used as inputs for the next. This allows for a clear and logical flow of data, making it easier to understand how each calculation contributes to the overall financial picture."),
               p("Inputs for the calculator are organized into sections, each corresponding to a specific area of financial planning. Each section contains a set of parameters that can be adjusted by the user. The calculator receives these parameters as a config yaml file used to perform calculations and generate results."),
               p("As the main output, the calculator instance contains a results element, containing the detailed calculations as a data frame. This table is then used in the RShiny app to generate the plots and visualizations."),


               p(strong("User Interface")),
               p("The web page is created using RShiny to obtain an interactive interface to access the calculator. The app is modular, with specific modules coordinating the sidepane, the main panel, and individual modules producing each of the  visualizations. All plots are generated using ggplot2 plus several HTML modifications to achieve the final visuals."),
               p("Please contact me on GitHub with any questions, suggestions, or issues you may encounter while using the calculator."),
               hr(),
               hr()
             )

      )

    })
  })
}
