app_global <- function() {
  shiny::addResourcePath("logo", system.file("www", "logo", package = "kew.metrics"))

  metrics_gbf <<- readr::read_csv(
    system.file("03_docs", "metrics_gbf.csv", package = "kew.metrics", mustWork = TRUE),
    col_types = readr::cols(
      Goal = readr::col_factor(levels = c("A", "B")),
      Target = readr::col_integer(),
      Group = readr::col_factor(levels = c("Headline", "Complimentary", "Component"))
    )
  )
}
