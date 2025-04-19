aboutPageContent <- function(is_mobile) {
  div(
    style = if(is_mobile) "padding-left: 15px; padding-right: 15px;" else "padding-left: 15px",
    h2("About - Financial Evolution Calculator"),
    p(em(NAVTAB_CONTENT$DISCLAIMERS$NOT_FINANCIAL_ADVISE)),
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
}
