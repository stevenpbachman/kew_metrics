app_global <- function() {
  EDGEspecies <<- readr::read_csv(
    system.file("01_data", "EDGE", "EDGEspecies_matched.csv", package = "kew.metrics")
  )
  EDGEcountries <<- readr::read_csv(
    system.file("01_data", "EDGE", "edge_ranges.csv", package = "kew.metrics")
  )
  SampledGlobal <<- readr::read_csv(
    system.file("01_data", "RedList", "SRLI_2024.csv", package = "kew.metrics")
  )
  metrics_gbf <<- readr::read_csv(
    system.file("03_docs", "metrics_gbf.csv", package = "kew.metrics")
  )
  tipas <<- readr::read_csv(
    system.file("01_data", "TIPAS", "TIPAs.csv", package = "kew.metrics")
  )
  tipas_shp <<- sf::st_read(
    system.file(
      "01_data", "TIPAS", "TIPA_Composite_POLYGON", "TIPA_Composite_POLYGON.shp",
      package = "kew.metrics"
    )
  ) %>%
    sf::st_zm(drop = TRUE, what = "ZM")
}
