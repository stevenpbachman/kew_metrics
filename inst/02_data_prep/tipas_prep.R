
# tipas prep

library(rWCVP)
#wgsrpd_mapping

library(sf)
library(leaflet)
library(dplyr)

# merge the shapefiles (hopefully get this in one go later)

# Load multiple shapefiles (or combine them into a list)
Bobole <- st_read(system.file("01_data/TIPAS/Bobole/Bobole.shp", package = "kew.metrics"))
'Bilene-Calanga' <- st_read(system.file("01_data/TIPAS/Bilene_Calanga/Bilene_Calanga.shp", package = "kew.metrics"))
Mount_Morrumbala <- st_read(system.file("01_data/TIPAS/Mount_Morrumbala/Mount_Morrumbala.shp", package = "kew.metrics"))
bvi <- st_read(system.file("01_data/TIPAS/TIPAs_network_boundaries_OUTLINES/TIPAs_network_boundaries_OUTLINES.shp", package = "kew.metrics"))

# Combine them using st_union
tipas_shp <- bind_rows(Bobole, Mount_Morrumbala,`Bilene-Calanga`, bvi)
tipas_shp <- st_zm(tipas_shp, drop = TRUE, what = "ZM")
#bvi <- st_zm(bvi, drop = TRUE, what = "ZM")


TIPAs <- read.csv(system.file("01_data/TIPAS/TIPAs.csv", package = "kew.metrics"))
TIPAs$tipas_nam <- TIPAs$Name
#tipas_names <- data.frame(tipas_names = c("Bobole", "Bilene-Calanga", "Mount Morrumbala"))

tipas_shp <- bind_cols(tipas_shp, TIPAs)

st_write(tipas_shp, file.path(system.file("01_data/TIPAS", package = "kew.metrics"), "tipas_shp.shp"))

