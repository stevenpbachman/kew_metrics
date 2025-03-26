#' Run
#'
#' Run application
#'
#' @param ... Additional parameters to pass to [shiny::shinyApp].
#'
#' @export
run <- function(...) {
  shiny::shinyApp(ui = ui, server = server, onStart = app_global, ...) # nolint: object_usage_linter
}

#' Run Development
#'
#' Runs the development version which includes
#' the build step.
#'
#' @keywords internal
run_dev <- function() {
  file <- system.file("run", "app.R", package = "kew.metrics")
  shiny::shinyAppFile(file)
}
