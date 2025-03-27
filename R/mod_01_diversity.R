#' Diversity module
#'
#' Create the UI and Server components for the diversity page.
#' @inheritParams conservation_ui
#' @return Either the HTML UI components, or the corresponding server code for the module.
#' @rdname diversity_ui
diversity_ui <- function(id) {
  ns <- shiny::NS(id)
  nav_panel(
    title = "Diversity",
    page_sidebar(
      sidebar = sidebar(
        accordion(
          accordion_panel(
            "Species Richness",
            selectInput(
              inputId = ns("species_layer"),
              label = "Select layer:",
              choices = list(
                "None" = "",
                "Gymnosperms" = "gymno",
                "Ferns" = "ferns",
                "Angiosperms" = "angio"
              ),
              selected = ""
            )
          ),
          accordion_panel(
            "Genetic",
            selectInput(
              inputId = ns("genetic_layer"),
              label = "Select layer:",
              choices = list(
                "None" = "",
                "PAFTOL" = "paftol",
                "Genome Size" = "genome"
              ),
              selected = ""
            )
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

    # Only allow one dataset input at a time ----
    observe({
      if (input$species_layer != "") {
        updateSelectInput(session, "genetic_layer", selected = "")
      }
    })

    observe({
      if (input$genetic_layer != "") {
        updateSelectInput(session, "species_layer", selected = "")
      }
    })
  })
}
