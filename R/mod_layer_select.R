#' Create layer selection accordion
#'
#' Based on the spec in a yaml file, create an accordion for selecting different layers.
#' @param spec_file File path to a YAML file that specifies what options should be available in the
#'   accordion.
#' @return A [bslib::accordion()] with a panel for each top-level entry in the YAML file.
layer_select_ui <- function(id, spec_file) {
  ns <- shiny::NS(id)
  yaml::read_yaml(spec_file) %>%
    purrr::imap(
      function(x, idx) {
        bslib::accordion_panel(
          title = idx,
          shiny::selectInput(
            inputId = ns(x$id),
            label = x$label,
            choices = x$choices
          ),
          value = x$id
        )
      }
    ) %>%
    unname() %>%
    bslib::accordion(id = ns("choice_accordion"), open = FALSE, multiple = FALSE)
}

#' @rdname layer_select_ui
#' @return The server part of the module returns a reactive expression containing the selected
#'   layer.
layer_select_server <- function(id, spec_file) {
  shiny::moduleServer(id, function(input, output, session) {
    # Find all the ID values for each accordion panel
    ids <- yaml::read_yaml(spec_file) %>%
      purrr::map_depth(.depth = 1, .f = "id")

    # HACK: The accordion is supposed to only open the first panel when the open = NULL argument is
    # used but this isn't being obeyed by bslib::accordion, so we start it closed and then make a
    # single call to open the first panel.
    bslib::accordion_panel_open("choice_accordion", ids[[1]], session = session)

    if (length(ids) > 1L) {
      # Convert this to a vector.
      id_vec <- purrr::list_c(ids, ptype = character())

      # Where there is more than one accordion, create an observer on each select input, so that
      # setting any select input to a non-empty value makes all the other panels reset their
      # values to "".
      # We use purrr::walk to apply the side effect systematically for each accordion panel.
      purrr::walk(
        ids,
        function(x) {
          shiny::observe({
            if (input[[x]] != "") {
              purrr::walk(
                setdiff(id_vec, x),
                ~ shiny::updateSelectInput(session = session, inputId = .x, selected = "")
              )
            }
          })
        }
      )
    }

    selections <- shiny::reactive({
      shiny::req(input$choice_accordion)
      return(input[[input$choice_accordion]])
    })

    return(selections)
  })
}
