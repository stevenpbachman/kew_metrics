#' Conservation module
#'
#' Create the UI and Server components for the conservation page.
#' @param id Module ID, should match across each UI and Server pairing.
#' @return Either the HTML UI components, or the corresponding server code for the module.
#' @rdname conservation_module
conservation_ui <- function(id) {
  ns <- shiny::NS(id)
  nav_panel(
    title = "Conservation",
    page_sidebar(
      sidebar = sidebar(
        accordion(
          accordion_panel(
            title = "TIPAs",
            selectInput(
              inputId = ns("tipas_layer"), # FIXME: This is going to be tricky to match up later.
              label = "Select layer:",
              choices = list(
                "None" = "",
                "TIPAs" = "tipas"
              ),
              selected = ""
            )
          )
        )
      ),
      shiny::conditionalPanel(
        "input.tipas_layer == 'tipas'",
        ns = ns,
        page_fillable(
          navset_card_tab(
            title = "TIPAs",
            full_screen = TRUE,
            sidebar = sidebar(
              selectizeInput(
                inputId = ns("tipas_country_select"),
                label = "Select country",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                inputId = ns("tipas_name_select"),
                label = "Select TIPA",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              ),
              input_task_button(
                id = ns("apply_tipa_filter"),
                label = "Apply Filter"
              )
            ),
            tab_datatable_ui(id = ns("filtered_table")),
            nav_panel(
              "Map",
              leaflet::leafletOutput(ns("tipas_map"))
            ),
            nav_panel(
              "Summary stats",
              layout_column_wrap(
                width = "100%",
                plotly::plotlyOutput(ns("cumulative_area_plot"), height = "280px")
              ),
              layout_column_wrap(
                width = "250px",
                heights_equal = "row",
                value_box(
                  title = "Number of selected TIPAs",
                  full_screen = FALSE,
                  value = textOutput(ns("tipa_count"))
                ),
                value_box(
                  title = shiny::tags$span(shiny::HTML("Sum of TIPAs areas (km<sup>2</sup>)")),
                  full_screen = FALSE,
                  value = htmlOutput(ns("tipa_area"), inline = TRUE)
                )
              )
            ),
            nav_panel(
              "About",
              includeMarkdown(system.file("about", "about_tipas.Rmd", package = "kew.metrics"))
            )
          )
        )
      )
    )
  )
}

#' @rdname conservation_module
conservation_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Load TIPAS data files
    tipas <- readr::read_csv(
      system.file("01_data", "TIPAS", "TIPAs.csv", package = "kew.metrics", mustWork = TRUE),
      col_types = readr::cols(Country = readr::col_character(),
                              Name = readr::col_character(),
                              Area = readr::col_double(),
                              year_identified = readr::col_integer())
    )

    tipas_shp <- sf::st_read(
      system.file(
        "01_data", "TIPAS", "TIPA_Composite_POLYGON", "TIPA_Composite_POLYGON.shp",
        package = "kew.metrics", mustWork = TRUE
      )
    ) %>%
      sf::st_zm(drop = TRUE, what = "ZM")

    # selectize input for the tipas country  filter
    observe({
      updateSelectizeInput(
        session,
        "tipas_country_select",
        choices = sort(unique(tipas$Country), decreasing = FALSE),
        selected = character(0),
        server = TRUE
      )
    })

    # selectize input for the tipas namefilter
    observe({
      updateSelectizeInput(
        session,
        "tipas_name_select",
        choices = sort(unique(tipas$Name), decreasing = FALSE),
        server = TRUE
      )
    })

    # Make the TIPA name selection dependent on country selection
    observe({
      filtered_by_country <- filter_if_truthy(tipas, .data$Country, input$tipas_country_select)

      updateSelectizeInput(
        session,
        "tipas_name_select",
        choices = sort(unique(filtered_by_country$Name)),
        #selected = NULL
        selected = character(0)
      )
    }) %>%
      bindEvent(input$tipas_country_select)

    # Reactive that responds to the apply tipas filter button
    filtered_tipa_data <- reactive({
      tipas %>%
        filter_if_truthy(.data$Country, input$tipas_country_select) %>%
        filter_if_truthy(.data$Name, input$tipas_name_select) %>%
        dplyr::arrange(.data$Country)
    }) %>%
      bindEvent(input$apply_tipa_filter, ignoreNULL = FALSE)

    # Conservation datatable ----
    tab_datatable_server(id = "filtered_table", .data = filtered_tipa_data)

    # Summary statistics
    output$tipa_count <- renderText({
      req(filtered_tipa_data())
      nrow(filtered_tipa_data())
    })

    output$tipa_area <- renderUI({
      req(nrow(filtered_tipa_data()) > 0L)
      filtered_tipa_data() %>%
        dplyr::summarise(total_area = sum(.data$Area)) %>%
        dplyr::pull("total_area") %>%
        ceiling() %>%
        prettyNum(big.mark = ",") %>%
        shiny::HTML("km<sup>2</sup>")
    })

    output$cumulative_area_plot <- plotly::renderPlotly({
      req(filtered_tipa_data())  # Ensure data is available

      # Process data: Sort by year and calculate cumulative sum of area
      cumulative_data <- filtered_tipa_data() %>%
        dplyr::arrange(.data$year_identified) %>%
        dplyr::group_by(.data$year_identified) %>%
        dplyr::summarise(total_area = sum(.data$Area, na.rm = TRUE)) %>%
        dplyr::mutate(cumulative_area = cumsum(.data$total_area))  # Compute cumulative sum

      # Create the Plotly chart
      plotly::plot_ly(
        cumulative_data,
        x = ~ year_identified,
        y = ~ cumulative_area,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'blue'),
        marker = list(size = 6, color = 'red')
      ) %>%
        plotly::layout(
          title = "Cumulative Growth in TIPAs Over Time",
          xaxis = list(title = "Year Identified"),
          yaxis = list(title = "Cumulative TIPA (sq km)"),
          hovermode = "x unified"
        )
      #add_lines(y = ~cumulative_area, name = "Trend")

    })

    prot_planet_url <- "https://data-gis.unep-wcmc.org/server/rest/services/ProtectedSites/The_World_Database_of_Protected_Areas/MapServer/tile/{z}/{y}/{x}"

    output$tipas_map <- leaflet::renderLeaflet({
      withProgress(message = 'Loading map...', {
        req(filtered_tipa_data())
        filtered_shp <- tipas_shp[tipas_shp$TIPA_Name %in% filtered_tipa_data()$Name, ]

        leaflet::leaflet() %>%
          leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri imagery") %>%
          leaflet::addProviderTiles("CartoDB.Positron", group = "Carto map") %>%
          leaflet::addTiles(
            urlTemplate = prot_planet_url,
            group = "Protected Planet"
          ) %>%
          leaflet::addAwesomeMarkers(
            data = filtered_shp,
            lng = ~sf::st_coordinates(sf::st_centroid(geometry))[, 1],
            lat = ~sf::st_coordinates(sf::st_centroid(geometry))[, 2],
            label = ~TIPA_Name,
            group = "TIPAs pins",
            clusterOptions = leaflet::markerClusterOptions(),
            icon = leaflet::awesomeIcons(
              icon = 'home',
              markerColor = "darkgreen",
              iconColor = 'white'
            )
          ) %>%
          leaflet::addPolygons(
            data = filtered_shp,
            color = "red",
            weight = 2,
            fillColor = "red",
            fillOpacity = 0.75,
            group = "TIPAs polygons",
            label = filtered_shp$TIPA_Name
          ) %>%
          leaflet::addLayersControl(
            baseGroups = c("Esri imagery", "Carto map"),
            overlayGroups = c("TIPAs pins", "TIPAs polygons", "Protected Planet"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
      })
    })
  })
}
