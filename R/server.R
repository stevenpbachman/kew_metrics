#' Main application server function
#'
#' @param input,output Shiny reactive values for the app's input and output objects.
#' @param session A shiny session object.
#' @return A Shiny server.
server <- function(input, output, session) {
  # Add module servers ----

  ## Diversity ----
  conservation_server(id = "conservation")

  ## Risk ----
  risk_server(id = "risk")

  ## GBF ----
  requested_dataset <- gbf_indicators_server(id = "gbf_indicators")

  # When the GBF Indicators module detects a user selected the dataset, the page should be
  # changed to display the requested page.
  # NOTE: We need to do this outside of the module so that shiny::update methods are able to
  # see HTML IDs outside of the module's namespace.
  observe({
    req(requested_dataset())

    shiny::updateTabsetPanel(session, "main_nav", selected = requested_dataset()$navigation)
    shiny::updateSelectInput(session,
                             shiny::NS(tolower(requested_dataset()$navigation),
                                       requested_dataset()$dataset_input_id),
                             selected = requested_dataset()$dataset)
  })
}
