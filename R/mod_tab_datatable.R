#' Display table of data as a Nav Panel.
#'
#' Creates a [bslib::nav_panel()] page containing a [DT::datatable()] representation of some input
#' data.
#' @inheritParams conservation_ui
#' @inheritParams bslib::nav_panel
#' @return A [bslib::nav_panel()] containing a [DT::datatable()] representation of some input
#' data.
#' @rdname tab_datatable
tab_datatable_ui <- function(id, title = "Table", ..., value = title, icon = NULL) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = title,
    DT::DTOutput(ns("data_table"))
  )
}

#' @rdname tab_datatable
#' @param .data Data to present in the table, as a reactive expression.
#' @param options Additional Datatable options passed to [DT::datatable()].
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
                                 )) {
  moduleServer(id, function(input, output, session) {
    data_table <- shiny::reactive({
      stopifnot(shiny::is.reactive(.data))
      req(.data())

      DT::datatable(
        .data(),
        filter = "none",
        extensions = 'Buttons',
        options = options,
        class = "compact stripe hover nowrap"
      )
    })

    output$data_table <- DT::renderDT(data_table())

    return(data_table)
  })
}
