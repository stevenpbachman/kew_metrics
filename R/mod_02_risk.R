#' Risk module
#'
#' Create the UI and Server components for the risk page.
#' @inheritParams conservation_ui
#' @return Either the HTML UI components, or the corresponding server code for the module.
#' @rdname risk_ui
risk_ui <- function(id) {
  ns <- shiny::NS(id)
  nav_panel(
    title = "Risk",
    page_sidebar(
      sidebar = sidebar(
        accordion(
          accordion_panel(
            "EDGE",
            selectInput(
              inputId = ns("edge_layer"),
              label = "Select layer:",
              choices = list(
                "None" = "",
                "Species" = "edgespecies",
                "Countries" = "edgecountries",
                "Index" = "edgeindex",
                "Zones" = "edgezones"
              ),
              selected = ""
            )
          ),
          accordion_panel(
            "Red List Index",
            selectInput(
              inputId = ns("red_list_layer"),
              label = "Select layer:",
              choices = list(
                "None" = "",
                "Global - Sampled" = "globalsampled",
                "Goldilocks clade I" = "goldilocksI",
                "Goldilocks clade II" = "goldilocksII"
                #"Legumes" = "legumes", # change to "Global - Sampled"
                #"Monocots" = "monocots" # change to "Global - Sampled"
              ),
              selected = ""
            )
          )
        )
      ),
      shiny::conditionalPanel(
        # NOTE: We technically don't need this outer layer, but without it we can end up with
        # both dataset 1 and dataset 2 boxes appearing at the same time for a split second,
        # because conditionalPanel will react faster than the server's observe operation.
        # This makes the old box disappear for a split second before the new one appears.
        "input.red_list_layer == ''", ns = ns,
        shiny::conditionalPanel(
          "input.edge_layer == 'edgespecies'",
          ns = ns,
          risk_edge_species_ui(id = ns("edge_species"))
        ),
        shiny::conditionalPanel(
          "input.edge_layer == 'edgecountries'",
          ns = ns,
          risk_edge_countries_ui(id = ns("edge_countries"))
        ),
      ),
      shiny::conditionalPanel(
        "input.edge_layer == ''", ns = ns,
        shiny::conditionalPanel(
          "input.red_list_layer == 'globalsampled'",
          ns = ns,
          risk_redlist_ui(id = ns("redlist_globalsampled"))
        )
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

    # Only allow one dataset input at a time ----
    observe({
      if (input$edge_layer != "") {
        updateSelectInput(session, "red_list_layer", selected = "")
      }
    }) %>%
      bindEvent(input$edge_layer)

    observe({
      if (input$red_list_layer != "") {
        updateSelectInput(session, "edge_layer", selected = "")
      }
    }) %>%
      bindEvent(input$red_list_layer)

    # Import server layers ----
    ## EDGE server layers ----
    risk_edge_species_server(id = "edge_species", edge_countries = EDGEcountries)
    risk_edge_countries_server(id = "edge_countries", edge_countries = EDGEcountries)

    ## Red List Index server layers ----
    risk_redlist_server(id = "redlist_globalsampled")
  })
}
