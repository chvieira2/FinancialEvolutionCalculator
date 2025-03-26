comparisonModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 4,
      card(
        id = ns("comparison_card_1"),
        card_header("Scenario 1"),
        DT::dataTableOutput(ns("comparison_table_1")),  # Ensure this is active
        actionButton(
          inputId = ns("delete_comparison_1"),
          label = "Delete Scenario 1",
          class = "btn-sm btn-danger"
        )
      )
    ),
    column(
      width = 4,
      card(
        id = ns("comparison_card_2"),
        card_header("Scenario 2"),
        DT::dataTableOutput(ns("comparison_table_2")),  # Ensure this is active
        actionButton(
          inputId = ns("delete_comparison_2"),
          label = "Delete Scenario 2",
          class = "btn-sm btn-danger"
        )
      )
    ),
    column(
      width = 4,
      card(
        id = ns("comparison_card_3"),
        card_header("Scenario 3"),
        DT::dataTableOutput(ns("comparison_table_3")),  # Ensure this is active
        actionButton(
          inputId = ns("delete_comparison_3"),
          label = "Delete Scenario 3",
          class = "btn-sm btn-danger"
        )
      )
    )
  )
}


comparisonModuleServer <- function(id, comparison_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize empty tables for all scenarios
    output$comparison_table_1 <- DT::renderDataTable({
      DT::datatable(data.frame(Message = "No scenario added for comparison"), options = list(scrollX = TRUE))
    })
    output$comparison_table_2 <- DT::renderDataTable({
      DT::datatable(data.frame(Message = "No scenario added for comparison"), options = list(scrollX = TRUE))
    })
    output$comparison_table_3 <- DT::renderDataTable({
      DT::datatable(data.frame(Message = "No scenario added for comparison"), options = list(scrollX = TRUE))
    })

    # Observe changes in comparison_list and update tables dynamically
    observe({
      current_list <- comparison_list()

      # Render table for Scenario 1
      if (length(current_list) >= 1) {
        output$comparison_table_1 <- DT::renderDataTable({
          DT::datatable(current_list[[1]], options = list(scrollX = TRUE))
        })
      }

      # Render table for Scenario 2
      if (length(current_list) >= 2) {
        output$comparison_table_2 <- DT::renderDataTable({
          DT::datatable(current_list[[2]], options = list(scrollX = TRUE))
        })
      }

      # Render table for Scenario 3
      if (length(current_list) >= 3) {
        output$comparison_table_3 <- DT::renderDataTable({
          DT::datatable(current_list[[3]], options = list(scrollX = TRUE))
        })
      }
    })

    # Handle deletion of comparison results
    observeEvent(input$delete_comparison_1, {
      current_list <- comparison_list()
      if (length(current_list) >= 1) {
        comparison_list(current_list[-1])
      }
      showNotification("Scenario 1 removed from comparison!",
                       type = "message", duration = 3)
    })

    observeEvent(input$delete_comparison_2, {
      current_list <- comparison_list()
      if (length(current_list) >= 2) {
        comparison_list(current_list[-2])
      }
      showNotification("Scenario 2 removed from comparison!",
                       type = "message", duration = 3)
    })

    observeEvent(input$delete_comparison_3, {
      current_list <- comparison_list()
      if (length(current_list) >= 3) {
        comparison_list(current_list[-3])
      }
      showNotification("Scenario 3 removed from comparison!",
                       type = "message", duration = 3)
    })
  })
}