#' Diversity module
#'
#' Create the UI and Server components for the diversity page.
#' @inheritParams conservation_ui
#' @return Either the HTML UI components, or the corresponding server code for the module.
#' @rdname diversity_ui
diversity_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar_ns <- ns("layer_select")
  nav_panel(
    title = "Diversity",
    page_sidebar(
      sidebar = sidebar(
        layer_select_ui(
          id = sidebar_ns,
          spec_file = system.file(
            "layer_selections", "diversity.yaml",
            package = "kew.metrics", mustWork = TRUE
          )
        )
      ),
      uiOutput(ns("diversity_conditional"))
    )
  )
}

#' @rdname diversity_ui
diversity_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    layer_select_server(
      id = "layer_select",
      spec_file = system.file(
        "layer_selections", "diversity.yaml",
        package = "kew.metrics", mustWork = TRUE
      )
    )
  })
}
