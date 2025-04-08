
# preparing the new TDWG map - to reduce file size
# rds is 7.1 mb

# simplify the polygons and then add centroid markers


tdwg_level3 <- readRDS("~/kew_metrics/inst/01_data/tdwg_level3.rds")

plot(tdwg_level3$geometry)


library(rWCVP)
library(sf)
install.packages("rmapshaper")
library(rmapshaper)

rWCVPdata::wcvp_version()

base_map <- rWCVPdata::wgsrpd3
st_write(base_map, "wgsrpd_raw.shp") # file size 7.1 mb

simplified_basemap <- ms_simplify(
  base_map,
  keep = 0.3,  # Adjust between 0 (most simplified) and 1 (full detail)
  keep_shapes = TRUE  # Keeps small polygons from disappearing
)
st_write(simplified_basemap, "wgsrpd_simple.shp") # file size 7.1 mb
saveRDS(simplified_basemap, file = "wgsrpd_simple.rds")
readRDS(file = "inst/01_data/wgsrpd_simple.rds")


leaflet::leaflet() %>%
  leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri imagery") %>%
  leaflet::addProviderTiles("CartoDB.Positron", group = "Carto map") %>%
  leaflet::setView(lng = 0, lat = 0, zoom = 2) %>%
  leaflet::addPolygons(
    data = simplified_basemap,
    weight = 1,
    opacity = 1,
    color = "grey",
    dashArray = "3",
    fillOpacity = 0.8,
    group = 'Species Richness',
    highlightOptions = leaflet::highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  leaflet::addLayersControl(
    baseGroups = c("Esri imagery", "Carto map"),
    overlayGroups = c('Species Richness'),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )



getwd()

