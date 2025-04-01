#' Display a choropleth using leaflet
#'
#' The choropleth contains one floating [shiny::selectInput] that allows for switching between
#' layers displayed on the plot. On the server side, this switch determines which column of data
#' that is joined to the shapefile is used to determine the fill colour.
#'
#' @note Currently the colours used in the palette are hard-coded in the server code for this
#' module, with a default palette of [YlGn](https://colorbrewer2.org/#type=sequential&scheme=YlGn)
#' used if a match is not found.
#'
#' @inheritParams shiny::selectInput
#' @inheritParams conservation_ui
#' @return Shiny module part of a UI containing leaflet map and floating [shiny::selectInput()], or
#'   corresponding server code.
leaflet_choropleth_ui <- function(id, label, choices) {
  ns <- shiny::NS(id)
  shiny::tagList(
    leaflet::leafletOutput(ns("map")),
    shiny::absolutePanel(
      id = ns("controls"),
      top = 20,
      left = "auto",
      right = 20,
      bottom = "auto",
      class = "card card-body",
      draggable = TRUE,
      width = 330,
      height = "auto",
      shiny::selectInput(inputId = ns("choropleth_layer"),
                         label = label,
                         choices = choices,
                         selectize = FALSE)
    )
  )
}

#' @rdname leaflet_choropleth_ui
#' @param data Summary data to map onto the shapefile. The summary data should include a
#'   `area_code_l3` column containing the area ID labels.
#' @param edge_countries The EDGE countries dataset
#' @param shapefile Path to shapefile.
leaflet_choropleth_server <- function(id,
                                      data,
                                      edge_countries,
                                      shapefile = system.file("01_data", "wgsrpd_simple.rds",
                                                              package = "kew.metrics",
                                                              mustWork = TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri imagery") %>%
        leaflet::addProviderTiles("CartoDB.Positron", group = "Carto map") %>%
        leaflet::setView(lng = 0, lat = 0, zoom = 2) %>%
        leaflet::addLayersControl(
          baseGroups = c("Esri imagery", "Carto map"),
          position = "topleft",
          options = leaflet::layersControlOptions(collapsed = TRUE)
        )
    })

    wgsrpd <- readRDS(shapefile)

    map_data <- shiny::reactive({
      shiny::req(data())

      edge_gymno <- edge_countries %>%
        dplyr::filter(.data$powo_id %in% data()$powo_id) %>% # get the ranges
        dplyr::summarise(richness = dplyr::n(),
                         threat = sum(.data$ED, na.rm = TRUE),
                         .by = "area_code_l3")

      dplyr::left_join(wgsrpd, edge_gymno, by = c("LEVEL3_COD" = "area_code_l3"))
    })

    shiny::observe({
      shiny::req(map_data())

      palette_name <- switch(input$choropleth_layer,
        threat = "Blues",
        richness = "YlOrRd",
        "YlGn" #default
      )

      bins <- pretty(range(map_data()[[input$choropleth_layer]], na.rm = TRUE))
      pal <- leaflet::colorBin(palette = palette_name,
                               domain = map_data()[[input$choropleth_layer]],
                               bins = bins,
                               na.color = "lightgray")
      labels <- lapply(X = sprintf("<strong>%s</strong><br/>%g species",
                                   map_data()$LEVEL3_NAM,
                                   map_data()[[input$choropleth_layer]]),
                       FUN = shiny::HTML)

      leaflet::leafletProxy(mapId = "map", session = session, data = map_data()) %>%
        leaflet::clearShapes() %>%
        leaflet::addPolygons(
          fillColor = ~ pal(map_data()[[input$choropleth_layer]]),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = leaflet::highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet::removeControl(layerId = "legend") %>%
        leaflet::addLegend(
          pal = pal,
          values = map_data()[[input$choropleth_layer]],
          opacity = 0.7,
          title = NULL,
          layerId = "legend",
          position = "bottomright"
        )
    })
  })
}
