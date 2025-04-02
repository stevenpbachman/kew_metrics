library(arrow)
library(dplyr)
library(rWCVP)

tm <- rWCVP::taxonomic_mapping %>%
  mutate(across(everything(), factor))

# full list of names
names <- rWCVPdata::wcvp_names

# Convert data types of some columns
names <- dplyr::mutate(
  names,
  across(
    c(taxon_rank:infraspecies, lifeform_description, climate_description, reviewed),
    factor
  ),
  across(ends_with("plant_name_id"), as.integer)
) %>%
  left_join(tm, by = "family")

# but we only want Accepted species
acc_names <- dplyr::filter(names, taxon_status == "Accepted")

# for each species we have distribution data
distributions <- rWCVPdata::wcvp_distributions

get_levels <- function(.data, col) {
  dplyr::distinct(distributions, dplyr::across(dplyr::starts_with({{ col }}))) %>%
    dplyr::arrange(dplyr::across(dplyr::contains("_code_"))) %>%
    dplyr::pull()
}

continent_levels <- get_levels(distributions, "continent")
region_levels <- get_levels(distributions, "region")
area_levels <- get_levels(distributions, "area")

distributions <- dplyr::mutate(
  distributions,
  across(ends_with("_id"), as.integer),
  across(introduced:location_doubtful, as.logical),
  continent = factor(continent, levels = continent_levels),
  region = factor(region, levels = region_levels),
  area = factor(area, levels = area_levels)
)

# it should just be a left join to link the native ranges – same system in the EDGE maps
acc_names2 <- left_join(acc_names, rWCVPdata::wcvp_distributions,
                        by = c("accepted_plant_name_id" = "plant_name_id")) %>%
  filter(!introduced, !extinct, !location_doubtful)

# TODO: Use dplyr::select() to get rid of any columns we won't need in the app. This will make the
# stored data files smaller, and easier to deploy and host.

# Store this file to parquet
write_dataset(
  dataset = acc_names2,
  path = system.file("01_data", "Diversity", "species_richness", package = "kew.metrics"),
  format = "parquet",
  partitioning = "higher"
)

# To read a dataset back
this_data <- arrow::open_dataset(
  sources = system.file("01_data", "Diversity", "species_richness",
                        package = "kew.metrics", mustWork = TRUE),
  format = "parquet"
)

# Then use collect() to denote when to pull the data into R
# for example:
this_data %>%
  dplyr::group_by(.data$higher) %>%
  dplyr::count(.data$area) %>%
  dplyr::arrange(dplyr::desc(.data$n)) %>%
  dplyr::collect()
