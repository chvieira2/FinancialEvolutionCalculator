parametersPageContent <- function() {
  div(
    h2("Calculator Parameters"),
    p("See below a detailed explanation of each parameter used by the Financial Evolution Calculator."),
    hr(),

    # Load and process the YAML configuration
    {
      default_template_file <- TEMPLATE_SCENARIOS[[DEFAULT_TEMPLATE]]
      config <- safelyLoadConfig(file.path("config", "templates", default_template_file))

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
  )
}
