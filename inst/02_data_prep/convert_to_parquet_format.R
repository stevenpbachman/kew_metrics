library(readr)
library(arrow)

# EDGE ----
edge_dir <- system.file("01_data", "EDGE", package = "kew.metrics")
edge_ranges <- read_csv(
  file.path(edge_dir, "edge_ranges.csv"),
  col_types = cols(
    accepted_plant_name_id = col_integer(),
    plant_locality_id = col_integer(),
    continent_code_l1 = col_integer(),
    region_code_l2 = col_integer(),
    introduced = col_integer(),
    extinct = col_integer(),
    location_doubtful = col_integer()
  )
)

write_parquet(
  edge_ranges,
  file.path(edge_dir, "edge_ranges.parquet")
)


edge_species <- read_csv(
  file.path(edge_dir, "EDGEspecies_matched.csv"),
  col_types = cols(accepted_plant_name_id = col_integer())
)

write_parquet(
  edge_species,
  file.path(edge_dir, "EDGEspecies_matched.parquet")
)

# Redlist -----
redlist_dir <- system.file("01_data", "RedList", package = "kew.metrics")
redlist_srli <- read_csv(file.path(redlist_dir, "SRLI_2024.csv"))

write_parquet(
  redlist_srli,
  file.path(redlist_dir, "SRLI_2024.parquet")
)
#



bench::mark(
  readr::read_csv(file.path(redlist_dir, "SRLI_2024.csv")),
  arrow::read_parquet(file.path(redlist_dir, "SRLI_2024.parquet")),
  check = FALSE
)

bench::mark(
  readr::read_csv(
    file.path(edge_dir, "edge_ranges.csv"),
    col_types = readr::cols(
      accepted_plant_name_id = readr::col_integer(),
      plant_locality_id = readr::col_integer(),
      continent_code_l1 = readr::col_integer(),
      region_code_l2 = readr::col_integer(),
      introduced = readr::col_integer(),
      extinct = readr::col_integer(),
      location_doubtful = readr::col_integer()
    )
  ),
  arrow::read_parquet(file.path(edge_dir, "edge_ranges.parquet")),
  check = FALSE
)
