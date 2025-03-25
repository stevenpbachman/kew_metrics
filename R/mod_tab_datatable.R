#' Display table of data as a Nav Panel.
#'
#' Creates a [bslib::nav_panel()] page containing a [DT::datatable()] representation of some input
#' data.
#' @inheritParams conservation_ui
#' @inheritParams bslib::nav_panel
#' @param as_nav_panel Whether to return the table as a standalone [DT::datatable()] (when `FALSE`),
#'   or to wrap the table in a [bslib::nav_panel()] (when `TRUE`, by default).
#' @return A [bslib::nav_panel()] containing a [DT::datatable()] representation of some input
#' data.
#' @rdname tab_datatable
tab_datatable_ui <- function(id,
                             title = "Table",
                             ...,
                             value = title,
                             icon = NULL,
                             as_nav_panel = TRUE) {
  ns <- shiny::NS(id)
  tbl <- DT::DTOutput(ns("data_table"))
  if (as_nav_panel) {
    bslib::nav_panel(title = title, tbl)
  } else {
    tbl
  }
}

#' @rdname tab_datatable
#' @param .data Data to present in the table, as a reactive expression.
#' @inheritParams DT::datatable
#' @return From `tab_datatable_server()`, the [DT::datatable()] object as a reactive expression.
tab_datatable_server <- function(id,
                                 .data,
                                 options = list(
                                   searching = FALSE,
                                   pageLength = 5,
                                   scrollX = TRUE,
                                   scrollY = FALSE,
                                   dom = 'Bftip',
                                   lengthChange = FALSE,
                                   buttons = list(list(
                                     extend = 'csv',
                                     text = 'Download CSV',
                                     exportOptions = list(
                                       modifier = list(
                                         modifier = list(page = 'all')
                                       )
                                     )
                                   ))
                                 ),
                                 extensions = "Buttons",
                                 class = "compact stripe hover nowrap",
                                 escape = TRUE,
                                 selection = c("multiple", "single", "none")) {
  selection <- match.arg(selection)

  moduleServer(id, function(input, output, session) {
    data_table <- shiny::reactive({
      stopifnot(shiny::is.reactive(.data))
      req(.data())

      DT::datatable(
        .data(),
        options = options,
        filter = "none",
        extensions = extensions,
        class = class,
        escape = escape,
        selection = selection
      )
    })

    output$data_table <- DT::renderDT(data_table())

    return(data_table)
  })
}
