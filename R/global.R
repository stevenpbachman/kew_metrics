app_global <- function() {
  shiny::addResourcePath("logo", system.file("www", "logo", package = "kew.metrics"))

  metrics_gbf <<- readr::read_csv(
    system.file("03_docs", "metrics_gbf.csv", package = "kew.metrics", mustWork = TRUE)
  )
}
