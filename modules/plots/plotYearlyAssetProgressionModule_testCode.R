library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

source(file.path("modules", "plotYearlyAssetProgressionModule.R"))

# Create some sample data
# Load configuration
config <- yaml::read_yaml(file.path("config", "inputs.yaml"))

# Initialize the R6 object
data_processor <- DataProcessor$new(config = config, initial_year = 2025, final_year = 2075)
data_processor$calculate()
example_scenario <- data_processor$get_results()
example_scenario <- reactiveVal(example_scenario)

# Create a simple app to test the module
ui <- fluidPage(
  titlePanel("Plot Module Test"),
  plotYearlyAssetProgressionModuleUI("test_plot")
)

server <- function(input, output, session) {
  plotYearlyAssetProgressionModuleServer("test_plot", example_scenario)
}

shinyApp(ui, server)
