library(sf)
tipas_shp <- st_read(system.file("01_data/TIPAS/TIPA_Composite_POLYGON/TIPA_Composite_POLYGON.shp",
                                 package = "kew.metrics"))

Encoding(tipas_shp$TIPA_Name)


library(rgdal)

tipas_shp <- st_zm(tipas_shp, drop = TRUE, what = "ZM")


leaflet() %>%
  addTiles() %>%
  #addPolygons(data = tipas_shp$geometry,
  addPolygons(data = edge_gymno_richness_sf)
              #color = "red",    # Outline color
              #weight = 2,           # Outline thickness
              #fillColor = "red",# Fill color
              #


#fillOpacity = 0.75)    # Transparency)

# First, let's see which rows have which encoding
row_encodings <- data.frame(
  row = 1:length(dbf_data$TIPA_Name),
  text = dbf_data$TIPA_Name,
  encoding = Encoding(dbf_data$TIPA_Name)
)

# View rows with unknown encoding
unknown_rows <- row_encodings[row_encodings$encoding == "unknown", ]


library(foreign)
dbf_data <- read.dbf(system.file("01_data/TIPAS/TIPA_Composite_POLYGON/TIPA_Composite_POLYGON.dbf",
                                 package = "kew.metrics"))

# Bolivia
dbf_data$TIPA_Name <- gsub("LomerÃ­o", "Lomerío", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("SÃ£o SebastiÃ£o Peninsula", "São Sebastião Pennisula", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Cerro ManomÃ³", "Cerro Manomó", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Lajas del Carmen Rivero TÃ³rrez", "Lajas del Carmen Rivero Tórrez", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Laguna ConcepciÃ³n", "Laguna Concepción", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("San MatÃ­as, ANMI San MatÃ­as", "San Matías, ANMI San Matías", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("SerranÃ­a de Chiquitos", "Serranía de Chiquitos", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("ConcepciÃ³n", "Concepción", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("El Carmen (Sendero ecolÃ³gico)", "El Carmen (Sendero ecologico)", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Cerro MutÃºn", "Cerro Mutún", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("JardÃ­n BotÃ¡nico Municipal de Santa Cruz de la Sierra", "Jardín Botánico Municipal de Santa Cruz de la Sierra", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("SerranÃ­a de Ipias-Abayoy", "Serranía de Ipías-Abayoy", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Santa Cruz La Vieja y bloque norte de San JosÃ© de Chiquitos", "Santa Cruz La Vieja y bloque norte de San José de Chiquitos
", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Reserva Forestal Bajo ParaguÃ¡", "Reserva Forestal Bajo Paraguá", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Ãrea Protegida Municipal OrquÃ­deas del Encanto", "Área Protegida Municipal Orquídeas del Encanto", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Reserva RÃ­os Blanco y Negro (Perseverancia and Oquiriquia)", "Reserva Ríos Blanco y Negro (Perseverancia and Oquiriquia)
", dbf_data$TIPA_Name)

# Cameroon
dbf_data$TIPA_Name <- gsub("Mount VokrÃ©", "Mount Vokré", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("AkokndouÃ© hill", "Mont Akok Ndoé", dbf_data$TIPA_Name)

# Mozambique
dbf_data$TIPA_Name <- gsub("Ã", "à", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Ã¡", "á", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("gá¡mula", "gámula", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Muá ", "Muà", dbf_data$TIPA_Name)

problem_rows <- grep("Muà", dbf_data$TIPA_Name)

# Replace those specific rows with the correct text
dbf_data$TIPA_Name[problem_rows] <- "Muàgámula River"


dbf_data$TIPA_Name <- gsub("RibÃ¡uÃ¨-Mâpaluwe", "Ribáuè-M’paluwe", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Mount NÃ¡llume", "Monte Nállume", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("LÃºrio Waterfalls, ChiÃºre", "Lúrio Waterfalls, Chiúre", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Inharrime-ZÃ¡vora", "Inharrime-Závora", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("ErÃ¡ti", "Eráti", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("CatapÃº", "Catapú", dbf_data$TIPA_Name)

# Guinea
dbf_data$TIPA_Name <- gsub("Ã©", "é", dbf_data$TIPA_Name)
dbf_data$TIPA_Name <- gsub("Ã­", "í", dbf_data$TIPA_Name)



write.dbf(dbf_data, "corrected_file.dbf")




