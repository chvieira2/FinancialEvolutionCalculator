welcomePageContent <- function(is_mobile) {
  div(
    style = if (is_mobile) "padding-left: 15px; padding-right: 15px; padding-bottom: 50px;" else "",
    h2("Welcome to the Financial Evolution Calculator!"),
    p(em(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE)),

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

  )
}
