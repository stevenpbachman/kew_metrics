
# preparing EDGE data for use in app

# 1. add POWO IDs to names
# 2. get TDWG ranges for names
# optional - get country names instead of TDWG
# 3. incorporate into leaflet map as choropleth
# where does the richness calcualtion happen? before the map?

# 1. add POWO IDs to names
Gymnosperms <- read.csv("01_data/EDGE/Gymnosperm_EDGE2_scores_2024.csv")
Angiosperms <- read.csv("01_data/EDGE/EDGE_angio.csv")

library(tidyverse)
library(rWCVP)

matches <- wcvp_match_names(Angiosperms,
                            name_col="Species",
                            #author_col="authority",
                            fuzzy=TRUE,
                            progress_bar=FALSE)

# resolve multimatch
auto_resolved <-
  matches %>%
  nest_by(Species) %>%
  mutate(data=list(resolve_multi(data))) %>%
  unnest(col=data) %>%
  ungroup()


# replace homotypic with accepted
accepted_matches <- auto_resolved %>%
  left_join(rWCVPdata::wcvp_names, by=c("wcvp_accepted_id"="plant_name_id")) %>%
  mutate(keep=case_when(
    taxon_status == "Accepted" & (wcvp_status != "Synonym" | wcvp_homotypic) ~
      "Matched to an accepted name",
    TRUE ~ "Not matched to an accepted name"
  ))


edge_gymno <- accepted_matches %>%
  select(powo_id, family, genus, species, taxon_name, taxon_authors, RL.cat, 
         EDGE.median, ED.median, lifeform_description, accepted_plant_name_id) %>%
  mutate(group = "Gymnosperms")

edge_angio <- accepted_matches %>%
  select(powo_id, family, genus, species, taxon_name, taxon_authors, RL.cat, 
         EDGE.median, ED.median, lifeform_description, accepted_plant_name_id) %>%
  mutate(group = "Angiosperms")


write.csv(edge_gymno, "EDGE_gymno_matched.csv", row.names = FALSE)
write.csv(edge_angio, "EDGE_angio_matched.csv", row.names = FALSE)

EDGEspecies <- dplyr::bind_rows(edge_gymno, edge_angio) %>% rename(EDGE = EDGE.median,
                                                                   ED = ED.median)

#EDGEspecies_matched <- EDGEspecies_matched %>% select(-X)


write.csv(EDGEspecies_matched, "EDGEspecies_matched.csv", row.names = FALSE)

edge_ranges <- EDGEspecies %>%
  left_join(rWCVPdata::wcvp_distributions, 
            by = c("accepted_plant_name_id" = "plant_name_id")) %>%
  filter(introduced == 0, extinct == 0, location_doubtful == 0)

# save down edge gymno ranges
write.csv(edge_ranges, "edge_ranges.csv", row.names = FALSE)


# this has to be done each time the filter is changed
# subset edge_gymno_ranges by the filtered data
edge_gymno_subset <- edge_gymno_ranges %>%
  filter(powo_id %in% selected_data()$powo_id)

# get the density of species per TDWG
edge_gymno_richness <- edge_gymno_subset %>%
  group_by(area_code_l3) %>%
  count()

# link to the sf geometry
edge_gymno_richness_sf <- rWCVPdata::wgsrpd3 %>% 
  #add the summary data, allowing for the different column names
  left_join(edge_gymno_richness, by=c("LEVEL3_COD"="area_code_l3"))

# palette
bins <- c(0, 10, 20, 50, 100, Inf)
pal <- colorBin("YlOrRd", domain = edge_gymno_richness_sf$n, bins = bins)

# labels
labels <- sprintf(
  "<strong>%s</strong><br/>%g species",
  edge_gymno_richness_sf$LEVEL3_NAM, edge_gymno_richness_sf$n
) %>% lapply(htmltools::HTML)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = edge_gymno_richness_sf,
    fillColor = ~pal(n),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = edge_gymno_richness_sf$n, opacity = 0.7, title = NULL,
            position = "bottomright")






# FUNCTIONS ----
# resolve multiple matches function ####
resolve_multi <- function(df) {
  if (nrow(df) == 1) {
    return(df)
  }
  
  # some fuzzy matches are rejected from the previous section
  valid_matches <- filter(df, !is.na(match_similarity))
  if (nrow(valid_matches) == 0) {
    return(head(df, 1))
  }
  
  # Removed the check for wcvp_author_edit_distance
  matching_authors <- valid_matches
  
  if (nrow(matching_authors) == 1) {
    return(matching_authors)
  }
  
  accepted_names <-
    matching_authors %>%
    filter(wcvp_status == "Accepted" | ! sum(wcvp_status == "Accepted"))
  if (nrow(accepted_names) == 1) {
    return(accepted_names)
  }
  
  synonym_codes <- c("Synonym", "Orthographic", "Artificial Hybrid", "Unplaced")
  synonyms <-
    accepted_names %>%
    filter(wcvp_status %in% synonym_codes | ! sum(wcvp_status %in% synonym_codes))
  if (nrow(synonyms) == 1)  {
    return(synonyms)
  }
  
  n_matches <- length(unique(synonyms$wcvp_accepted_id)) / nrow(synonyms)
  final <- head(synonyms, 1)
  if (n_matches != 1) {
    final <-
      final %>%
      mutate(
        across(wcvp_id:resolved_match_type & where(is.numeric), ~NA_real_),
        across(wcvp_id:resolved_match_type & where(is.character), ~NA_character_),
        resolved_match_type="Could not resolve multiple matches"
      )
  }
  final
}
