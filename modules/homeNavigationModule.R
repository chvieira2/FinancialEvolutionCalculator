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
        "Welcome" = structure(
          list(
            "Introduction" = structure("welcome_intro", stopened = TRUE),
            "Guidelines" = structure("welcome_guidelines", stopened = TRUE)
          ),
          stopened = TRUE
        ),
        "About the Calculator" = structure(
          list(
            "Purpose" = structure("about_purpose", stopened = TRUE),
            "Data Used" = structure("about_data", stopened = TRUE),
            "Outcomes" = structure("about_outcomes", stopened = TRUE),
            "Privacy" = structure("about_privacy", stopened = TRUE)
          ),
          stopened = TRUE
        ),
        "Key Features" = structure(
          list(
            "Financial Projections" = structure("features_projections", stopened = TRUE),
            "Visualizations" = structure("features_visualizations", stopened = TRUE),
            "Sensitivity Analysis" = structure("features_sensitivity", stopened = TRUE)
          ),
          stopened = TRUE
        )
      )
    })

    # Render the content based on the selected tree node
    output$home_content <- renderUI({
      selected <- unlist(shinyTree::get_selected(input$home_toc, format = "names"))

      label_to_handle <- list(
        "Welcome" = "welcome_intro",
        "Introduction" = "welcome_intro",
        "Guidelines" = "welcome_guidelines",
        "About the Calculator" = "about_purpose",
        "Purpose" = "about_purpose",
        "Data Used" = "about_data",
        "Outcomes" = "about_outcomes",
        "Privacy" = "about_privacy",
        "Key Features" = "features_projections",
        "Financial Projections" = "features_projections",
        "Visualizations" = "features_visualizations",
        "Sensitivity Analysis" = "features_sensitivity"
      )

      # Default selection if nothing is selected
      if (is.null(selected)) {
        selected_handle <- "welcome_intro"
      } else {
        # Translate the selected label to the corresponding handle
        selected_handle <- label_to_handle[[selected]]
      }

      switch(selected_handle,
             "welcome_intro" = div(
               h3("Welcome to the Financial Evolution Calculator"),
               p("This app helps you analyze your financial evolution over time."),
               p("Select the tabs on top to try out the calculator."),
               p("Read more about the calculator using the options on the left.")
             ),
             "welcome_guidelines" = div(
               h3("Guidelines"),
               p("1. Use the Calculator tab to input your data."),
               p("2. Explore the visualizations to understand your financial evolution."),
               p("3. Use the sensitivity analysis to test different scenarios.")
             ),
             "about_purpose" = div(
               h3("Purpose of the Calculator"),
               p("This calculator is designed to help users project their financial evolution.")
             ),
             "about_data" = div(
               h3("Data Used"),
               p("The calculator uses user-provided data such as income, expenses, and assets.")
             ),
             "about_outcomes" = div(
               h3("Main Outcomes"),
               p("The calculator provides key financial metrics and visualizations.")
             ),
             "about_privacy" = div(
               h3("Privacy"),
               p("No financial data from users is saved.")
             ),
             "features_projections" = div(
               h3("Financial Projections"),
               p("The calculator projects financial evolution over a customizable time range.")
             ),
             "features_visualizations" = div(
               h3("Visualizations"),
               p("Detailed visualizations of income, expenses, and assets are provided.")
             ),
             "features_sensitivity" = div(
               h3("Sensitivity Analysis"),
               p("Explore different scenarios using the sensitivity analysis feature.")
             )
      )
    })
  })
}