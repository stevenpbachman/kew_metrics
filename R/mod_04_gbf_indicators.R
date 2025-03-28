#' GBF Indicators module
#'
#' Create the UI and Server components for the risk page.
#' @inheritParams conservation_ui
#' @return Either the HTML UI components, or the corresponding server code for the module.
#' @rdname gbf_indicators_ui
gbf_indicators_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "GBF Indicators",
    bslib::page_sidebar(
      sidebar = sidebar(
        accordion(
          accordion_panel(
            "Filters",
            selectInput(
              inputId = ns("goal_filter"),
              label = "Select Goal:",
              choices = c("All" = "", unique(metrics_gbf$Goal)),
              selected = ""
            ),
            selectInput(
              inputId = ns("target_filter"),
              label = "Select Target:",
              choices = c("All" = "", sort(unique(metrics_gbf$Target), na.last = TRUE)),
              selected = ""
            ),
            selectInput(
              inputId = ns("group_filter"),
              label = "Select Group:",
              choices = c("All" = "", sort(unique(metrics_gbf$Group), decreasing = TRUE)),
              selected = ""
            )
          )
        )
      ),
      # Main panel content
      tab_datatable_ui(ns("metrics_table"), as_nav_panel = FALSE)
    )
  )
}

#' @rdname gbf_indicators_ui
gbf_indicators_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Load GBF indicators ----
    metrics_gbf <- readr::read_csv(
      system.file("03_docs", "metrics_gbf.csv", package = "kew.metrics", mustWork = TRUE)
    ) %>%
      dplyr::mutate(
        Action = sprintf(
          '<button class="action-button" id="btn_%s" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority: \'event\'})">View Dataset</button>',
          .data$Dataset,
          shiny::NS(id, "selected_dataset"), # ensure ID has module namespace
          .data$Dataset # Using Dataset as a unique identifier
        )
      )

    filtered_metrics <- reactive({
      metrics_gbf %>%
        filter_if_truthy(.data$Goal, input$goal_filter) %>%
        filter_if_truthy(.data$Target, input$target_filter) %>%
        filter_if_truthy(.data$Group, input$group_filter)
    })

    # Datatable of filtered metrics ----
    tab_datatable_server(id = "metrics_table",
                         .data = filtered_metrics,
                         extensions = list(),
                         class = "display",
                         escape = 1:5,
                         selection = "single",
                         options = list(
                           pageLength = 5,
                           scrollY = FALSE,
                           scrollX = TRUE,
                           dom = 'Bftip',
                           lengthChange = FALSE,
                           columnDefs = list(
                             list(
                               targets = which(names(filtered_metrics()) == "Action") - 1,  # -1 because DT uses 0-based indexing
                               className = 'dt-center'
                             )
                           )
                         ))

    dataset_index <- tibble::tribble(
      ~dataset, ~navigation, ~dataset_input_id,
      "edgespecies", "Risk", "edge_layer",
      "globalsampled", "Risk", "red_list_layer",
      "tipas", "Conservation", "tipas_layer"
    )

    # This module returns the requested dataset when a user clicks "View Dataset" in the table.
    reactive({
      target <- dplyr::filter(dataset_index, .data$dataset == input$selected_dataset)
      if (nrow(target) == 0L) {
        return(NULL)
      }
      target
    }) %>%
      bindEvent(input$selected_dataset)
  })
}
