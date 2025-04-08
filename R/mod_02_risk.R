#' Risk module
#'
#' Create the UI and Server components for the risk page.
#' @inheritParams conservation_ui
#' @return Either the HTML UI components, or the corresponding server code for the module.
#' @rdname risk_ui
risk_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar_ns <- ns("layer_select")
  nav_panel(
    title = "Risk",
    page_sidebar(
      sidebar = sidebar(
        layer_select_ui(
          id = sidebar_ns,
          spec_file = system.file(
            "layer_selections", "risk.yaml",
            package = "kew.metrics", mustWork = TRUE
          )
        )
      ),
      shiny::textOutput(ns("selected")),
      shiny::conditionalPanel(
        # NOTE: We technically don't need this outer layer, but without it we can end up with
        # both dataset 1 and dataset 2 boxes appearing at the same time for a split second,
        # because conditionalPanel will react faster than the server's observe operation.
        # This makes the old box disappear for a split second before the new one appears.
        "input.choice_accordion == 'edge_layer'",
        ns = shiny::NS(sidebar_ns),
        shiny::conditionalPanel(
          "input.edge_layer == 'edgespecies'",
          ns = shiny::NS(sidebar_ns),
          risk_edge_species_ui(id = ns("edge_species"))
        ),
        shiny::conditionalPanel(
          "input.edge_layer == 'edgecountries'",
          ns = shiny::NS(sidebar_ns),
          risk_edge_countries_ui(id = ns("edge_countries"))
        ),
      ),
      shiny::conditionalPanel(
        "input.choice_accordion == 'red_list_layer'",
        ns = shiny::NS(sidebar_ns),
        shiny::conditionalPanel(
          "input.red_list_layer == 'globalsampled'",
          ns = shiny::NS(sidebar_ns),
          risk_redlist_ui(id = ns("redlist_globalsampled"))
        )
      ),
      shiny::conditionalPanel(
        "input.choice_accordion == 'ecosystems_layer'",
        ns = shiny::NS(sidebar_ns),
        shiny::conditionalPanel(
          "input.ecosystems_layer == 'assessments'",
          ns = shiny::NS(sidebar_ns),
          risk_ecosystem_ui(id = ns("ecosystems_layer"))
        ),
      )

    )
  )
}

#' @rdname risk_ui
risk_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Load EDGE data files ----
    EDGEcountries <- readr::read_csv(
      system.file("01_data", "EDGE", "edge_ranges.csv", package = "kew.metrics", mustWork = TRUE)
    )

    layer_select_server(
      id = "layer_select",
      spec_file = system.file(
        "layer_selections", "risk.yaml",
        package = "kew.metrics", mustWork = TRUE
      )
    )

    # Import server layers ----
    ## EDGE server layers ----
    risk_edge_species_server(id = "edge_species", edge_countries = EDGEcountries)
    risk_edge_countries_server(id = "edge_countries", edge_countries = EDGEcountries)

    ## Red List Index server layers ----
    risk_redlist_server(id = "redlist_globalsampled")

    ## Red List ecosystems server layers ----
    risk_ecosystem_server(id = "ecosystems_layer")

  })
}
