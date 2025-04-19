welcomePageContent <- function(is_mobile) {
  div(
    style = if(is_mobile) "padding-left: 15px; padding-right: 15px;" else "padding-left: 15px",
    h2("Welcome to the Financial Evolution Calculator!"),
    p(em(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE)),

    p("Ever wondered", strong("how today's decisions shape your financial future?"), "The Financial Evolution Calculator helps answer this and more questions, such as:"),
    tags$ul(
      style = "list-style-type: none; padding-left: 0;",  # Remove default bullet points
      tags$li(icon("graduation-cap"), " Should I start working sooner or pursue more education?"),
      tags$li(icon("baby"), " Will I be able to afford having kids?"),
      tags$li(icon("home"), " Should I buy a property and move out of rent?"),
      tags$li(icon("money-bill-wave"), " How much money do I need for retirement?"),
      tags$li(icon("clock"), " Will I even retire one day?"),
      tags$li(icon("chart-line"), " Will I be able to build generational wealth?")
    ),

    p(strong("Ain't no time for reading?"), "Go ahead and get your hands dirty by switching the navigation tab on the top of the page."),
    hr(),




    h3("Simulate Your Financial Journey in 3 Steps"),

    p("The calculator is designed to be user-friendly, even for those without a financial background. Its interactive interface allows you to input your current financial situation and future assumptions with ease."),

    tags$ol(
      style = "list-style-type: none; padding-left: 0;",  # Remove default bullet points
      tags$li(icon("edit"), strong("Input Baseline"), "- Quickly set up your financial profile using smart defaults."),
      tags$li(icon("sliders-h"), strong("Experiment Freely"), "- Adjust over 40 parameters with ease or use our realistic defaults."),
      tags$li(icon("chart-bar"), strong("Discover Insights"), "- Explore interactive visualizations to uncover trends and non-linear effects.")
    ),

    p(em("Pro-tip: Do not start from scratch! Use one of the available templates with realistic default values.")),
    hr(),




    h3("Financial Timeline Scenario"),
    p("The calculator generates a financial timeline that covers four key areas:"),
    tags$ul(
      style = "list-style-type: none; padding-left: 0;",  # Remove default bullet points
      tags$li(icon("piggy-bank"), strong("Liquid and Illiquid Assets"), "- Cash, emergency funds, liquid investments, property equities."),
      tags$li(icon("user-graduate"), strong("Human Capital"), "- Education and skills, career trajectory models."),
      tags$li(icon("child"), strong("Life Variables"), "- Childcare costs, unexpected expenses like health shocks, retirement."),
      tags$li(icon("chart-pie"), strong("Macroeconomics Factors"), "* - Taxes, inflation, interest rates, market volatility.")
    ),

    p("Year-by-year, we calculate the financial evolution based on these inputs and assumptions. The logic is simple: how much money you have, how much you expect to spend and how much you expect to earn today will define how much money you'll have tomorrow."),

    p(em("*Some Macroeconomic Factors, specially the tax system, are based on Germany in 2025.")),
    hr(),




    h3("Visualising your Financial Journey"),
    p("The main value to track your financial journey is your total net worth - the sum of all assets (cash, investments, property) minus debts (like mortgages or credit card debt). Total net worth is represented in the calculator as the sum of all your assets at liquid value estimates, namely the", strong("Total Asset Value")),

    p("A granular view of", strong("Key Financial Metrics"), "including Total Expenses, Total Income, Cash Flow, Emergency Reserve, Investment Withdrawals, and Total Invested provides a clearer picture of your financial health and help you understand the impact of your decisions."),

    p("A detailed breakdown of", strong("Total Expenses and Total Income components"), "is provided for investigating causal links between parameters. This way, you can see where your money comes and goes!"),

    p("Finally, the", strong("Sensitivity Analysis feature"), "lets you identify the most impactful parameters in your financial journey, and the ones you should pay close attention."),
    hr(),




    h3("Start Now With Template Scenarios!"),

    p("It is easy to get started: click on the 'Calculator' tab in the navigation bar and choose one of our templates. The visualisations will dynamically reflect your changes as you explore each parameter"),

    p("Trying out the pre-built templates is like playing The Sims, but with finances instead of virtual homes ;)"),
    p("Check out our analysis of each scenario by selecting it on the sidetab. For each template, we tried to answer one of the most burning questions around personal finances:", strong("Should they buy or rent their home?")),
    p("Please meet:"),
    tags$ul(
      tags$li(strong("Gene and Jean"), "- A family with small income and expenses, focusing on basic needs while planning for children and retirement."),
      tags$li(strong("Alex and Max"), "- A family with average income and expenses, balancing basic needs, savings and exploring investment options for financial stability while planning for children and retirement."),
      tags$li(strong("Jack and Beck"), "- A family with high income and expenses, focusing on investments for financial stability, early retirement, and more options for charitable donations while also planning to have kids.")
    ),
    p(strong("Spoiler alert!"), "You might find the answer is not as straightforward as you might think. The goal for this Calculator it to help you understand the trade-offs involved in financial decisions like this!"),
    p(em("Pro-tip: These scenarios are based on typical financial profiles for low-, mid-, and high-income families in Berlin, Germany in 2025. In our calculator, you can adjust parameters to fit your own situation and compare how they affect your financial evolution.")),
    br(),
    hr()

  )
}
