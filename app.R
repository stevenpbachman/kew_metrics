
# libraries
library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sysfonts)
library(scales)
library(sf)
library(httr)
library(plotly)
library(mapview)
library(ggiraph)
library(readr)

# load all data
# load_data <- function() {
#   list(
EDGEspecies <- read_csv("01_data/EDGE/EDGEspecies_matched.csv")
EDGEcountries <- read_csv("01_data/EDGE/edge_ranges.csv")
SampledGlobal <- read_csv("01_data/RedList/SRLI_2024.csv")
metrics_gbf <- read_csv("03_docs/metrics_gbf.csv")
tipas <- read_csv("01_data/TIPAS/TIPAs.csv")
tipas_shp <- st_read("01_data/TIPAS/TIPA_Composite_POLYGON/TIPA_Composite_POLYGON.shp") %>% 
st_zm(drop = TRUE, what = "ZM")
#   )
# }

# Source the UI and server files
source("ui.R")
source("server.R")

# Run the Shiny app
shinyApp(ui = ui, server = server)


