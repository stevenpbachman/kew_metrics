
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
#   )
# }

# Source the UI and server files
source("ui.R")
source("server.R")

# Run the Shiny app
shinyApp(ui = ui, server = server)


