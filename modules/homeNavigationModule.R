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
        "Welcome" = structure("welcome_page",stopened = TRUE),
        "About" = structure("about_page", stopened = TRUE),
        "Scenario 1: Gene and Jean, the poverty trap" = structure("scenario_1", stopened = TRUE),
        "Scenario 2: Alex and Max, the home-poor lifestyle" = structure("scenario_2", stopened = TRUE),
        "Scenario 3: Jack and Beck, wearing golden cuffs" = structure("scenario_3", stopened = TRUE)
      )
    })

    # Render the content based on the selected tree node
    output$home_content <- renderUI({
      selected <- unlist(shinyTree::get_selected(input$home_toc, format = "names"))

      label_to_handle <- list(
        "Welcome" = "welcome_page",
        "About" = "about_page",
        "Scenario 1: Gene and Jean, the poverty trap" = "scenario_1",
        "Scenario 2: Alex and Max, the home-poor lifestyle" = "scenario_2",
        "Scenario 3: Jack and Beck, wearing golden cuffs" = "scenario_3"
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
               p(em("The Financial Evolution Calculator is a free and we do not track your financial data. The tool was designed to help you gain insights into your personal finances. Please note that this tool does not provide financial advice, and we are not financial advisors. For professional guidance, consult a certified financial expert.")),

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
               hr()

             ),






             "scenario_1" = div(
               h2("Simulation scenario 1: Gene and Jean, the poverty trap"),
               p("Scenario number 1 (Gene and Jean) represents a poor family in Germany. Hard-working class, working jobs with low education requirements, forced to live away from city centres due to high living costs and the housing market."),
               tags$ul(
                 tags$li(strong("Income"), "- "),
                 tags$li(strong("Living standards"), "- "),
                 tags$li(strong("Retirement"), "- "),
                 tags$li(strong("Family Size"), "- "),
                 tags$li(strong("Housing"), "- "),
                 tags$li(strong("Passive Investments"), "- "),
                 tags$li(strong("Taxes"), "- "),
                 tags$li(strong("Debts"), "- "),
                 tags$li(strong("Lump Sums"), "- ")
               )
             ),

             "scenario_2" = div(
               h2("Simulation scenario 2: Alex and Max, the home-poor lifestyle"),
               p("Scenario number 2 (Alex and Max) represents a mid-income family in Germany. They likely work on technical jobs, services or other functions requiring some level of higher education. Their performance is directly linked to their salaries. Their salaries allow them to save and buy a property in the periphery of Berlin."),
               tags$ul(
                 tags$li(strong("Income"), "- "),
                 tags$li(strong("Living standards"), "- "),
                 tags$li(strong("Retirement"), "- "),
                 tags$li(strong("Family Size"), "- "),
                 tags$li(strong("Housing"), "- "),
                 tags$li(strong("Passive Investments"), "- "),
                 tags$li(strong("Taxes"), "- "),
                 tags$li(strong("Debts"), "- "),
                 tags$li(strong("Lump Sums"), "- ")
               )
             ),


             "scenario_3" = div(
               h2("Simulation scenario 3: Jack and Beck, wearing golden cuffs"),
               p("Scenario number 3 (Jack and Beck) represents a high-income family in Germany. They likely work on knowledge jobs requiring high level of education. Their salaries depend mostly on their education rather than their performance. Their income is among the highest of all Germany, but they are still part of the working class meaning they do not live off inherited assets. Their salaries allow them to save and buy a property in Berlin not far from the city centre."),
               tags$ul(
                 tags$li(strong("Income"), "- "),
                 tags$li(strong("Living standards"), "- "),
                 tags$li(strong("Retirement"), "- "),
                 tags$li(strong("Family Size"), "- "),
                 tags$li(strong("Housing"), "- "),
                 tags$li(strong("Passive Investments"), "- "),
                 tags$li(strong("Taxes"), "- "),
                 tags$li(strong("Debts"), "- "),
                 tags$li(strong("Lump Sums"), "- ")
               )
             ),


             "about_page" = div(
               h2("About - Financial Evolution Calculator"),
               p("The Financial Evolution Calculator is a free and we do not track your financial data. The tool was designed to help you gain insights into your personal finances. Please note that this tool does not provide financial advice, and we are not financial advisors. For professional guidance, consult a certified financial expert."),
               hr(),


               h2("Development Team and Collaborations"),
               HTML(
                 'This page and the calculator behind it have been developed by chvieira2. Open source code is available <a href="https://github.com/chvieira2/FinancialEvolutionCalculator" target="_blank">here</a>. Please give credit where credit is due.'
               ),
               p("Feel free to open an issue, create a branch and submit change requests, and contact me directly on GitHub. I am always open to feedback, suggestions, and collaboration opportunities."),
               p(em("I received feedback that users would like to adapt the calculator to other countries than Germany. I am open to this idea, but cannot do it myslef due to lack of expertise in other countrie's tax systems. If you are interested in collaborating on this, please reach out to me.")),
               hr(),






               h2("Technical specifications"),
               p("The calculator was developed in R programming language and the web page was created using RShinny."),

               p(strong("Backend development")),
               p("The Calculator is an R6 Class object called DataProcessorClass that inherits from or instanciates other R6 Class objects containing the calculation steps for each section. It is designed to be modular, allowing for easy addition of new features and functionalities in the future."),
               p("Among its components, the BaseCalculator class contains the basic functions for data manipulation while the GeneralCalculator, RentalCalculator, PropertyCalculator, FinancialBalanceCalculator and PassiveInvestmentCalculator classes implement their own specific functions."),

               p(strong("Algorithmic logic and code compatibility")),
               p("Calculations for each year are execute sequentially, meaning that the results of one calculation are used as inputs for the next. This allows for a clear and logical flow of data, making it easier to understand how each calculation contributes to the overall financial picture."),
               p("Inputs for the calculator are organized into sections, each corresponding to a specific area of financial planning. Each section contains a set of parameters that can be adjusted by the user. The calculator receives these parameters as a config yaml file used to perform calculations and generate results."),
               p("As the main output, the calculator instance contains a results element, containing the detailed calculations as a data frame. This table is then used in the RShiny app to generate the plots and visualizations."),


               p(strong("User Interface")),
               p("The web page is created using RShiny to obtain an interactive interface to access the calculator. The app is modular, with specific modules coordinating the sidepane, the main panel, and individual modules producing each of the  visualizations. All plots are generated using ggplot2 plus several HTML modifications to achieve the final visuals."),
               p("Please contact me on GitHub with any questions, suggestions, or issues you may encounter while using the calculator."),
               hr()
             )

      )

    })
  })
}
