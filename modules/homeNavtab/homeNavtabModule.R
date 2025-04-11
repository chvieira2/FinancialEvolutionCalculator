source("modules/homeNavtab/welcomePageContent.R")
source("modules/homeNavtab/parametersPageContent.R")
source("modules/homeNavtab/scenario1Content.R")
source("modules/homeNavtab/scenario2Content.R")
source("modules/homeNavtab/scenario3Content.R")
source("modules/homeNavtab/aboutPageContent.R")
NAVTAB_CONTENT <- safelyLoadConfig("config/homeNavtabContent.yaml")


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

      selected_handle <- if (is.null(selected)) "welcome_page" else label_to_handle[[selected]]

      switch(selected_handle,
             "welcome_page" = welcomePageContent(),
             "parameters_page" = parametersPageContent(),
             "scenario_1" = scenario1Content(),
             "scenario_2" = scenario2Content(),
             "scenario_3" = scenario3Content(),
             "about_page" = aboutPageContent()
      )

    })
  })
}
