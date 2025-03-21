
# tipas prep

library(rWCVP)
#wgsrpd_mapping

library(sf)
library(leaflet)
library(dplyr)

# merge the shapefiles (hopefully get this in one go later)

# Load multiple shapefiles (or combine them into a list)
Bobole <- st_read("01_data/Bobole/Bobole.shp")
'Bilene-Calanga' <- st_read("01_data/Bilene_Calanga/Bilene_Calanga.shp")
Mount_Morrumbala <- st_read("01_data/Mount_Morrumbala/Mount_Morrumbala.shp")
bvi <- st_read("01_data/TIPAs_network_boundaries_OUTLINES/TIPAs_network_boundaries_OUTLINES.shp")

# Combine them using st_union
tipas_shp <- bind_rows(Bobole, Mount_Morrumbala,`Bilene-Calanga`, bvi)
tipas_shp <- st_zm(tipas_shp, drop = TRUE, what = "ZM")
#bvi <- st_zm(bvi, drop = TRUE, what = "ZM")


TIPAs <- read.csv("~/kew_metrics/01_data/TIPAs.csv")
TIPAs$tipas_nam <- TIPAs$Name
#tipas_names <- data.frame(tipas_names = c("Bobole", "Bilene-Calanga", "Mount Morrumbala"))

tipas_shp <- bind_cols(tipas_shp, TIPAs)

st_write(tipas_shp, "01_data/tipas_shp.shp")

