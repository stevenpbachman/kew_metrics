#' Risk EDGE module
#'
#' Create the UI and Server components for the risk page.
#' @inheritParams conservation_ui
#' @return Either the HTML UI components, or the corresponding server code for the module.
#' @rdname risk_edge_ui
risk_edge_species_ui <- function(id) {
  ns <- shiny::NS(id)
  page_fillable(
    navset_card_tab(
      sidebar = sidebar(
        selectizeInput(
          inputId = ns("edge_group_select"),
          label = "Select Group",
          choices = NULL,
          multiple = FALSE
        ),
        selectizeInput(
          inputId = ns("edge_family_select"),
          label = "Select Family",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = ns("edge_genus_select"),
          label = "Select Genus",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = ns("edge_species_select"),
          label = "Select Species",
          choices = NULL,
          multiple = TRUE
        ),
        input_task_button(
          id = ns("apply_edge_filter"),
          label = "Apply Filter"#,
          #class = "btn-primary"
        )
      ),
      full_screen = TRUE,
      title = "EDGE species",
      nav_panel("Table", fillable = TRUE, DT::DTOutput(ns("data_table_edgespecies"))),
      nav_panel("Maps", leaflet::leafletOutput(ns("EDGE_map1"))),
      nav_panel("About", includeMarkdown(system.file("about", "about_edge_species.Rmd",
                                                     package = "kew.metrics")))
    )
  )
}

#' @rdname risk_edge_ui
risk_edge_species_server <- function(id, edge_countries) {
  shiny::moduleServer(id, function(input, output, session) {
    # Load EDGE data files ----
    edge_species <- readr::read_csv(
      system.file("01_data", "EDGE", "EDGEspecies_matched.csv", package = "kew.metrics")
    )

    # Set filters for EDGE species ----
    ## EDGE group - Angiosperms or Gymnosperms ----
    observe({
      updateSelectizeInput(
        session,
        "edge_group_select",
        choices = sort(unique(edge_species$group), decreasing = FALSE),
        server = TRUE
      )
    })

    ## EDGE family ----
    observe({
      updateSelectizeInput(
        session,
        "edge_family_select",
        choices = sort(unique(edge_species$family), decreasing = FALSE),
        server = TRUE
      )
    })

    ## EDGE genus ----
    observe({
      updateSelectizeInput(
        session,
        "edge_genus_select",
        choices = sort(unique(edge_species$genus), decreasing = FALSE),
        server = TRUE
      )
    })

    ## EDGE species ----
    observe({
      updateSelectizeInput(
        session,
        "edge_species_select",
        choices = sort(unique(edge_species$taxon_name), decreasing = FALSE),
        server = TRUE
      )
    })

    # Refine filters based on the other filters ----
    ## Update family options based on group ----
    filtered_by_group <- reactive({
      filter_if_truthy(.data = edge_species, col = .data$group, ifTruthy = input$edge_group_select)
    })

    observe({
      updateSelectizeInput(
        session,
        "edge_family_select",
        choices = sort(unique(filtered_by_group()$family)),
        selected = character(0)
      )
    }) %>%
      bindEvent(input$edge_group_select)

    ## Update genus options based on group and family ----
    filtered_by_family <- reactive({
      filter_if_truthy(filtered_by_group(), .data$family, input$edge_family_select)
    })

    observe({
      updateSelectizeInput(
        session,
        "edge_genus_select",
        choices = sort(unique(filtered_by_family()$genus)),
        selected = character(0)
      )
    }) %>%
      bindEvent(c(input$edge_group_select, input$edge_family_select))

    ## Update species/taxon based on group, family and genus ----
    filtered_by_genus <- reactive({
      filter_if_truthy(filtered_by_family(), .data$genus, input$edge_genus_select)
    })

    observe({
      updateSelectizeInput(
        session,
        "edge_species_select",
        choices = sort(unique(filtered_by_genus()$taxon_name)),
        selected = character(0)
      )
    }) %>%
      bindEvent(c(input$edge_group_select, input$edge_family_select, input$edge_genus_select))

    # Create filtered_edge_data reactive that responds to the apply_edge_filter button ----
    filtered_edge_data <- reactive({
      filter_if_truthy(filtered_by_genus(), .data$taxon_name, input$edge_species_select)
    }) %>%
      bindEvent(input$apply_edge_filter, ignoreNULL = FALSE)

    # Output for the EDGE species datatable ----
    output$data_table_edgespecies <- DT::renderDT({
      req(filtered_edge_data())

      DT::datatable(
        filtered_edge_data(),
        filter = "none",
        extensions = 'Buttons',
        options = list(
          searching = FALSE,
          pageLength = 5,
          scrollX = TRUE,
          scrollY = FALSE,#"calc(100vh - 300px)",
          autoWidth = TRUE,
          paging = TRUE,
          dom = 'Bftip',
          buttons = list("csv"),
          lengthChange = FALSE
        ),
        class = "compact stripe hover nowrap"
      )
    })

    # Leaflet Map output ----
    output$EDGE_map1 <- leaflet::renderLeaflet({
      req(filtered_edge_data())

      # debugging
      # print(paste0("edge_gymno_subset = ", filtered_edge_data$powo_id))

      # prep data for map - maybe a helper function needed here?
      edge_gymno_subset <- edge_countries %>%
        dplyr::filter(.data$powo_id %in% filtered_edge_data()$powo_id) # get the ranges

      # debugging
      # print(paste0("edge_gymno_subset = ", edge_gymno_subset))

      # 1. generate first poly layer for map - edge richness
      edge_gymno_richness <- edge_gymno_subset %>%
        dplyr::group_by(.data$area_code_l3) %>%
        dplyr::count() # get the richness
      edge_gymno_richness_sf <- rWCVPdata::wgsrpd3 %>%
        dplyr::left_join(edge_gymno_richness, by = c("LEVEL3_COD" = "area_code_l3")) # add the sf geom
      bins <- pretty(range(edge_gymno_richness_sf$n, na.rm = TRUE))
      pal_rich <- leaflet::colorBin("YlOrRd", domain = edge_gymno_richness_sf$n, bins = bins, na.color = "lightgray") # palette
      labels_rich <- sprintf(
        "<strong>%s</strong><br/>%g species",
        edge_gymno_richness_sf$LEVEL3_NAM, edge_gymno_richness_sf$n
      ) %>% lapply(shiny::HTML) # labels

      # 2. generate second poly - threatened evolutionary history
      edge_gymno_threat <- edge_gymno_subset %>%
        dplyr::group_by(.data$area_code_l3) %>%
        dplyr::summarise(n = sum(.data$ED, na.rm = TRUE)) # Sum EDGE values
      #print(edge_gymno_threat)

      edge_gymno_threat_sf <- rWCVPdata::wgsrpd3 %>%
        dplyr::left_join(edge_gymno_threat, by = c("LEVEL3_COD" = "area_code_l3")) # add the sf geom
      bins <- pretty(range(edge_gymno_threat_sf$n, na.rm = TRUE))
      pal_threat <- leaflet::colorBin("Blues", domain = edge_gymno_threat_sf$n, bins = bins, na.color = "lightgray") # palette
      labels_threat <- sprintf(
        "<strong>%s</strong><br/>%g species",
        edge_gymno_threat_sf$LEVEL3_NAM, edge_gymno_threat_sf$n
      ) %>% lapply(shiny::HTML)     # labels

      # map it
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri imagery") %>%
        leaflet::addProviderTiles("CartoDB.Positron", group = "Carto map") %>%
        leaflet::setView(lng = 0, lat = 0, zoom = 2) %>%
        leaflet::addPolygons(
          data = edge_gymno_threat_sf,
          fillColor = ~ pal_threat(n),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          group = 'Threatened Evolutionary History',
          highlightOptions = leaflet::highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = labels_threat,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet::addPolygons(
          data = edge_gymno_richness_sf,
          fillColor = ~ pal_rich(n),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.8,
          group = 'Species Richness',
          highlightOptions = leaflet::highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = labels_rich,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet::addLegend(
          pal = pal_rich,
          values = edge_gymno_richness_sf$n,
          opacity = 0.7,
          title = NULL,
          position = "bottomright"
        ) %>%
        leaflet::addLegend(
          pal = pal_threat,
          values = edge_gymno_threat_sf$n,
          opacity = 0.7,
          title = NULL,
          position = "bottomleft"
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c("Esri imagery", "Carto map"),
          overlayGroups = c('Species Richness', 'Threatened Evolutionary History'),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )

    })

  })
}
