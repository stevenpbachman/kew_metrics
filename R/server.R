#' Main application server function
#'
#' @param input,output Shiny reactive values for the app's input and output objects.
#' @param session A shiny session object.
#' @return A Shiny server.
server <- function(input, output, session) {

  # Dataset manipulation ----

  # Reactive to control which dataset is being used
  base_data <- reactive({
    if (!is.null(input$dataset1) && input$dataset1 != "") {
      switch(
        input$dataset1,
        "edgespecies" = EDGEspecies,
        "edgecountries" = EDGEcountries,
        "edgeindex" = "",
        "edgezones" = ""
      )
    } else if (!is.null(input$dataset2) && input$dataset2 != "") {
      switch(input$dataset2,
             "globalsampled" = SampledGlobal)
    } else if (!is.null(input$dataset3) &&
               input$dataset3 != "") {
      switch(input$dataset3, "tipas" = tipas)
    } else {
      NULL
    }
  })

  # Reactive expression to determine which dataset type is selected
  dataset_type <- reactive({
    if (!is.null(input$dataset1) && input$dataset1 != "") {
      return("edge")
    }
    if (!is.null(input$dataset2) && input$dataset2 != "") {
      return("redlist")
    }
    if (!is.null(input$dataset3) && input$dataset3 != "") {
      return("tipas")
    }
    return(NULL)
  })

  # Reactive to control filtered data in the data tables
  selected_data <- reactive({
    req(base_data())
    req(dataset_type())

    current_data <- base_data()

    switch(
      dataset_type(),
      "edge" = {
        if (input$dataset1 == "edgespecies") {
          if (!is.null(input$data_table_edgespecies_rows_all)) {
            current_data[input$data_table_edgespecies_rows_all, ]
          } else {
            current_data
          }
        } else if (input$dataset1 == "edgecountries") {
          if (!is.null(input$data_table_edgeranges_rows_all)) {
            current_data[input$data_table_edgeranges_rows_all, ]
          } else {
            current_data
          }
        } else {
          current_data
        }
      },
      "redlist" = {
        if (!is.null(input$data_table_redlist_rows_all)) {
          current_data[input$data_table_redlist_rows_all, ]
        } else {
          current_data
        }
      },
      "tipas" = {
        if (!is.null(input$data_table_tipas_rows_all)) {
          current_data[input$data_table_tipas_rows_all, ]
        } else {
          current_data
        }
      },
      current_data  # default case
    )
  })

  # Automatically reset other selection to "None" when one dataset is selected
  observeEvent(input$dataset1, {
    if (input$dataset1 != "") {
      updateSelectInput(session, "dataset2", selected = "")
      updateSelectInput(session, "dataset3", selected = "")
    }
  })

  observeEvent(input$dataset2, {
    if (input$dataset2 != "") {
      updateSelectInput(session, "dataset1", selected = "")
      updateSelectInput(session, "dataset3", selected = "")
    }
  })

  observeEvent(input$dataset3, {
    if (input$dataset3 != "") {
      updateSelectInput(session, "dataset1", selected = "")
      updateSelectInput(session, "dataset2", selected = "")
    }
  })

  # Diveristy UI ----

  # Risk UI ----
  # Set the UI for the Risk tab
  output$Risk_conditional <- renderUI({
    req(dataset_type())

    if (dataset_type() == "edge") {
      if (input$dataset1 == "edgespecies") {
        page_fillable(
          navset_card_tab(
            sidebar = sidebar(
              selectizeInput(
                inputId = "edge_group_select",
                label = "Select group",
                choices = NULL,
                multiple = FALSE
              ),
              selectizeInput(
                inputId = "edge_family_select",
                label = "Select Family",
                choices = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                inputId = "edge_genus_select",
                label = "Select Genus",
                choices = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                inputId = "edge_species_select",
                label = "Select Species",
                choices = NULL,
                multiple = TRUE
              ),
              input_task_button(
                id = "apply_edge_filter",
                label = "Apply Filter"#,
                #class = "btn-primary"
              )
            ),
            full_screen = TRUE,
            title = "EDGE species",
            nav_panel("Table", fillable = TRUE, DT::DTOutput("data_table_edgespecies")),
            nav_panel("Maps", leaflet::leafletOutput("EDGE_map1")),
            nav_panel("About", includeMarkdown(system.file("about", "about_edge_species.Rmd",
                                                           package = "kew.metrics")))
          )
        )
      } else if (input$dataset1 == "edgecountries") {
        page_fillable(
          navset_card_tab(
            sidebar = sidebar(
              selectizeInput(
                inputId = "edge_region_group_select",
                label = "Select group",
                choices = NULL,
                multiple = FALSE
              ),
              selectizeInput(
                inputId = "edge_region_select",
                label = "Select region",
                choices = NULL,
                multiple = TRUE
              ),
              input_task_button(
                id = "apply_edge_region_filter",
                label = "Apply Filter"
              )
            ),
            full_screen = TRUE,
            title = "EDGE Countries",
            nav_panel("Countries Table", DT::DTOutput("data_table_edgeranges")),
            nav_panel("Summary stats",
                      layout_column_wrap(
                        width = "250px",
                        heights_equal = "row",
                        value_box(
                          title = "Number of EDGE species",
                          full_screen = TRUE,
                          value = textOutput("stat_e1")
                        ),
                        value_box(
                          title = "Highest ranked EDGE species",
                          full_screen = TRUE,
                          value = textOutput("stat_e2")
                        ),
                        value_box(
                          title = "Highest ranked ED species",
                          full_screen = TRUE,
                          value = textOutput("stat_e3")
                        ),
                        value_box(
                          title = "Sum of threatened ED",
                          full_screen = TRUE,
                          value = textOutput("stat_e4")
                        )
                      )
            ),
            nav_panel("About", includeMarkdown(system.file("about", "about_edge_countries.Rmd",
                                                           package = "kew.metrics")))
          )
        )
      }
    }

    else if (dataset_type() == "redlist"){
      if (input$dataset2 == "globalsampled") {
        page_fillable(
          navset_card_tab(
            sidebar = sidebar(selectizeInput(
              inputId = "srli_group_select",
              label = "Select group",
              choices = NULL,
              multiple = FALSE
            ),
            input_task_button(
              id = "apply_srli_filter",
              label = "Apply Filter"
            )
            ),
            full_screen = TRUE,
            title = "Red List Index species",
            nav_panel("Table", fillable = TRUE, DT::DTOutput("data_table_redlist")),
            nav_panel("Maps", leaflet::leafletOutput("SRLI_map1")),
            nav_panel("About",
                      includeMarkdown(system.file("about", "about_global_sampled_RLI.Rmd",
                                                  package = "kew.metrics")))
          )
        )
      }
    }

  })

  # Response UI ----
  # Set the UI for the Response tab
  output$conservation_conditional <- renderUI({
    req(dataset_type() == "tipas")

    page_fillable(
      navset_card_tab(
        sidebar = sidebar(
          selectizeInput(
            inputId = "tipas_country_select",
            label = "Select country",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          selectizeInput(
            inputId = "tipas_name_select",
            label = "Select TIPA",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          input_task_button(
            id = "apply_tipa_filter",
            label = "Apply Filter"
          )
        ),
        full_screen = TRUE,
        title = "TIPAs",
        nav_panel("Table",
                  fillable = TRUE,
                  DT::DTOutput("data_table_tipas")
        ),
        nav_panel("Map",
                  leaflet::leafletOutput("tipas_map")
        ),
        nav_panel("Summary stats",
                  layout_column_wrap(
                    width = "100%",
                    plotly::plotlyOutput("cumulative_area_plot", height = "280px")
                  ),
                  layout_column_wrap(
                    width = "250px",
                    heights_equal = "row",
                    value_box(
                      title = "Number of selected TIPAs",
                      full_screen = TRUE,
                      value = textOutput("stat4")
                    ),
                    value_box(
                      title = "Sum of TIPAs areas (km sq)",
                      full_screen = TRUE,
                      value = textOutput("stat5")
                    )
                  )
        ),
        nav_panel("About",
                  includeMarkdown(system.file("about", "about_tipas.Rmd", package = "kew.metrics")))
      )
    )


  })

  # GBF controls ----

  # Filtering options for GBF - goal, target, group
  filtered_metrics <- reactive({
    metrics <- metrics_gbf

    if (input$goal_filter != "All") {
      metrics <- metrics %>% dplyr::filter(.data$Goal == input$goal_filter)

    }
    if (input$target_filter != "All") {
      metrics <- metrics %>% dplyr::filter(.data$Target == input$target_filter)

    }
    if (input$group_filter != "All") {
      metrics <- metrics %>% dplyr::filter(.data$Group == input$group_filter)
    }

    metrics
  })

  # Render the GBF metrics table with action buttons for all rows
  output$gbf_metrics_table <- DT::renderDT({
    df <- filtered_metrics()

    # Add action buttons for all rows - allows link back to the reelvatn dataset page
    df <- df %>%
      dplyr::mutate(
        Action = sprintf(
          '<button class="action-button" id="btn_%s" onclick="Shiny.setInputValue(\'selected_dataset\', \'%s\', {priority: \'event\'})">View Dataset</button>',
          .data$Dataset,  # Using Dataset as a unique identifier
          .data$Dataset
        )
      )

    # datatable parameters
    DT::datatable(
      df,
      escape = FALSE,
      filter = "none",
      #extensions = 'Buttons',
      #class = "compact stripe hover nowrap",
      options = list(
        pageLength = 5,
        scrollY = FALSE,
        scrollX = TRUE,
        dom = 'Bftip',
        lengthChange = FALSE,
        columnDefs = list(
          list(
            targets = which(names(df) == "Action") - 1,  # -1 because DT uses 0-based indexing
            className = 'dt-center'
          )
        )
      ),
      selection = 'single'
    )
  })

  # Observer for the button clicks
  observeEvent(input$selected_dataset, {
    dataset <- input$selected_dataset

    # Navigate based on the selected dataset
    switch(dataset,
           "edgespecies" = {
             updateSelectInput(session, "dataset1", selected = "edgespecies")
             updateTabsetPanel(session, "main_nav", selected = "Risk")
           },
           "globalsampled" = {
             updateSelectInput(session, "dataset2", selected = "globalsampled")
             updateTabsetPanel(session, "main_nav", selected = "Risk")
           },
           "tipas" = {
             updateSelectInput(session, "dataset3", selected = "tipas")
             updateTabsetPanel(session, "main_nav", selected = "Conservation")
           }
    )
  })


  # EDGE controls ----
  # Input controls for EDGE datasets

  # Observe selectize input for EDGE group - Angiosperms or Gymnosperms
  observe({
    req(base_data(), dataset_type() == "edge")

    updateSelectizeInput(
      session,
      "edge_group_select",
      choices = sort(unique(base_data()$group), decreasing = FALSE),
      server = TRUE
    )
  })

  # Observe selectize input for EDGE family
  observe({
    req(base_data(), dataset_type() == "edge")

    updateSelectizeInput(
      session,
      "edge_family_select",
      choices = sort(unique(base_data()$family), decreasing = FALSE),
      server = TRUE
    )
  })

  # Observe selectize input for EDGE genus
  observe({
    req(base_data(), dataset_type() == "edge")

    updateSelectizeInput(
      session,
      "edge_genus_select",
      choices = sort(unique(base_data()$genus), decreasing = FALSE),
      server = TRUE
    )
  })

  # Observe selectize input for EDGE species
  observe({
    req(base_data(), dataset_type() == "edge")

    updateSelectizeInput(
      session,
      "edge_species_select",
      choices = sort(unique(base_data()$taxon_name), decreasing = FALSE),
      server = TRUE
    )
  })

  # Observe selectize input for EDGE countries - group
  # this is basically same as for EDGE species - can we reuse?
  observe({
    req(base_data(), dataset_type() == "edge")

    updateSelectizeInput(
      session,
      "edge_region_group_select",
      choices = sort(unique(base_data()$group), decreasing = FALSE),
      server = TRUE
    )
  })

  # Observe selectize input for EDGE countries
  observe({
    req(base_data(), dataset_type() == "edge")

    updateSelectizeInput(
      session,
      "edge_region_select",
      choices = sort(unique(base_data()$area), decreasing = FALSE),
      server = TRUE
    )
  })

  # Reactives to provide the data for selectize inputs
  filtered_by_group <- reactive({
    base_data() %>%
      dplyr::filter(if(length(input$edge_group_select) > 0) .data$group %in% input$edge_group_select else TRUE)
  })

  filtered_by_family <- reactive({
    filtered_by_group() %>%
      dplyr::filter(if(length(input$edge_family_select) > 0) .data$family %in% input$edge_family_select else TRUE)
  })

  filtered_by_genus <- reactive({
    filtered_by_family() %>%
      dplyr::filter(if(length(input$edge_genus_select) > 0) .data$genus %in% input$edge_genus_select else TRUE)
  })

  # Update dropdowns based on filtered data
  observeEvent(input$edge_group_select, {
    updateSelectizeInput(
      session,
      "edge_family_select",
      choices = sort(unique(filtered_by_group()$family)),
      selected = character(0)
    )
  })

  observeEvent(c(input$edge_group_select, input$edge_family_select), {
    updateSelectizeInput(
      session,
      "edge_genus_select",
      choices = sort(unique(filtered_by_family()$genus)),
      selected = character(0)
    )
  })

  observeEvent(c(input$edge_group_select, input$edge_family_select, input$edge_genus_select), {
    updateSelectizeInput(
      session,
      "edge_species_select",
      choices = sort(unique(filtered_by_genus()$taxon_name)),
      selected = character(0)
    )
  })

  # Create filtered_edge_data reactive that responds to the apply_edge_filter button
  filtered_edge_data <- eventReactive(input$apply_edge_filter, {
    filtered_by_genus() %>%
      dplyr::filter(if(length(input$edge_species_select) > 0) .data$taxon_name %in% input$edge_species_select else TRUE)
  }, ignoreNULL = FALSE)

  # Output for the EDGE species datatable
  output$data_table_edgespecies <- DT::renderDT({
    req(base_data(), dataset_type() == "edge")

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

  # Reactive for filtered EDGE countries data
  filtered_edge_region_data <- eventReactive(input$apply_edge_region_filter, {
    filtered_data <- base_data()

    if (length(input$edge_region_group_select) > 0) {
      filtered_data <- filtered_data %>%
        dplyr::filter(.data$group %in% input$edge_region_group_select)
    }

    if (length(input$edge_region_select) > 0) {
      filtered_data <- filtered_data %>%
        dplyr::filter(.data$area %in% input$edge_region_select)
    }

    filtered_data
  }, ignoreNULL = FALSE)


  # Output for the EDGE countries(regions) datatable
  output$data_table_edgeranges <- DT::renderDT({
    req(base_data(), dataset_type() == "edge")
    DT::datatable(
      filtered_edge_region_data(),
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
      class = "compact stripe hover nowrap"  # Optional styling for better readability
    )
  })

  # EDGE value box statistics -

  # stat_e1 - number of EDGE species in region/countries data
  output$stat_e1 <- renderText({
    req(selected_data(), dataset_type() == "edge")
    req(filtered_edge_region_data())
    paste(length((unique(filtered_edge_region_data()$taxon_name))) ) # Count rows of the dataset
  })

  # stat_e2 - highest ranked EDGE species region/countries data
  output$stat_e2 <- renderText({
    req(selected_data(), dataset_type() == "edge")
    req(filtered_edge_region_data())
    paste(filtered_edge_region_data() %>%
            dplyr::slice_min(order_by = .data$EDGE, with_ties = FALSE) %>%
            dplyr::pull("taxon_name"))
  })

  # stat_e3 - highest ranked ED species from region/countries data
  output$stat_e3 <- renderText({
    req(selected_data(), dataset_type() == "edge") # Ensure data is available and it's EDGE type
    req(filtered_edge_region_data())
    paste(filtered_edge_region_data() %>%
            dplyr::slice_min(order_by = .data$ED, with_ties = FALSE) %>%
            dplyr::pull("taxon_name"))
  })

  # stat_e4 - sum of threatened ED
  output$stat_e4 <- renderText({
    req(selected_data(), dataset_type() == "edge") # Ensure data is available and it's EDGE type
    req(filtered_edge_region_data())
    total_ED <- filtered_edge_region_data() %>%
      dplyr::distinct(.data$taxon_name, .keep_all = TRUE) %>%  # Keep only one row per species
      dplyr::summarise(total_ED = ceiling(sum(.data$ED, na.rm = TRUE))) # Sum ED values
    paste(total_ED)
  })

  # Leaflet Map outputs - EDGE species
  output$EDGE_map1 <- leaflet::renderLeaflet({
    req(selected_data())
    req(dataset_type() == "edge")
    req(filtered_edge_data())

    # debugging
    # print(paste0("edge_gymno_subset = ", filtered_edge_data$powo_id))

    # prep data for map - maybe a helper function needed here?
    edge_gymno_subset <- EDGEcountries %>%
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

  # RedList controls ----

  # selectize input for the SRLI group filter
  observe({
    req(base_data(), dataset_type() == "redlist")

    updateSelectizeInput(
      session,
      "srli_group_select",
      choices = sort(unique(base_data()$group), decreasing = FALSE),
      server = TRUE
    )
  })

  # srli reactive for filter button
  filtered_srli_data <- eventReactive(input$apply_srli_filter, {
    filtered_data <- base_data() %>%
      dplyr::filter(.data$group %in% input$srli_group_select)
  })


  output$data_table_redlist <- DT::renderDT({
    #req(selected_data(), dataset_type() == "redlist")
    req(base_data(), dataset_type() == "redlist")

    filtered_srli_data()

    DT::datatable(
      #base_data(),
      filtered_srli_data(),
      filter = "none",
      extensions = 'Buttons',
      options = list(
        searching = FALSE,
        #pageLength = 5,
        scrollX = TRUE,
        scrollY = "300px",#"calc(100vh - 300px)", FALSE
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        paging = FALSE,
        dom = 'Bftip',
        buttons = list("csv"),
        lengthChange = FALSE
        # buttons = list(
        #   list(
        #     extend = 'csv',
        #     text = 'Download CSV',
        #     exportOptions = list(modifier = list(modifier = list(page = 'all')))
        #   )
        # )
      ),
      class = "compact stripe hover nowrap"  # Optional styling for better readability
    )
  })

  output$SRLI_map1 <- leaflet::renderLeaflet({

    leaflet::leaflet() %>%
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri imagery") %>%
      leaflet::addProviderTiles("CartoDB.Positron", group = "Carto map") %>%
      leaflet::setView(lng = 0,
                       lat = 0,
                       zoom = 2) %>%
      #addProviderTiles(providers$Stadia.StamenTonerLite, group = "stamen") %>%
      #addProviderTiles(providers$Esri.WorldImagery, group = "esri") %>%
      leaflet::addLayersControl(
        baseGroups = c("Esri imagery", "Carto map"),
        #overlayGroups = c('Species Richness', 'Threatened Evolutionary History'),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )

  })

  output$redlist_plot <- ggiraph::renderGirafe({
    req(selected_data())
    req(dataset_type() == "redlist")

    # Create a summary table with counts
    summary_data <- selected_data() %>%
      dplyr::count(.data$RL_2020)

    # Create the interactive plot
    p <- ggplot2::ggplot(summary_data, ggplot2::aes(
      x = .data$RL_2020,
      y = .data$n,
      fill = .data$RL_2020,
      tooltip = paste("Category:", .data$RL_2020, "\nCount:", .data$n)  # Add tooltips
    )) +
      ggiraph::geom_bar_interactive(stat = "identity") +
      ggplot2::scale_fill_brewer(palette = "Set3") +
      ggplot2::labs(
        title = "Red List Categories Count",
        x = "Red List Category",
        y = "Count"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")

    # Return interactive plot
    ggiraph::girafe(ggobj = p, options = list(
      ggiraph::opts_hover(css = "fill-opacity:0.8;stroke:gray;cursor:pointer;"),
      ggiraph::opts_tooltip(css = "background-color:white;color:black;border-radius:5px;padding:5px;")
    ))
  })

  # # Plot output (only for Red List datasets)
  # output$redlist_plot <- renderPlot({
  #   req(selected_data())
  #   req(dataset_type() == "redlist")
  #
  #   # Replace with your actual Red List plotting code
  #   # Create a summary table with counts
  #   summary_data <- selected_data() %>%
  #     count(RL_2020)
  #
  #   ggplot(summary_data, aes(x = RL_2020, y = n, fill = RL_2020)) +
  #     geom_bar(stat = "identity") +
  #     scale_fill_brewer(palette = "Set3") +
  #     labs(title = "Red List Categories Count", x = "Red List Category", y = "Count") +
  #     theme_minimal() +
  #     theme(legend.position = "none")
  #
  #
  # })

  # TIPAs controls ----

  # selectize input for the tipas country  filter
  observe({
    req(base_data(), dataset_type() == "tipas")

    updateSelectizeInput(
      session,
      "tipas_country_select",
      choices = sort(unique(base_data()$Country), decreasing = FALSE),
      selected = character(0),
      server = TRUE
    )
  })

  # selectize input for the tipas namefilter
  observe({
    req(base_data(), dataset_type() == "tipas")

    updateSelectizeInput(
      session,
      "tipas_name_select",
      choices = sort(unique(base_data()$Name), decreasing = FALSE),
      server = TRUE
    )
  })

  # Make the TIPA name selection dependent on country selection
  observeEvent(input$tipas_country_select, {
    filtered_by_country <- base_data() %>%
      dplyr::filter(if(!is.null(input$tipas_country_select)) .data$Country %in% input$tipas_country_select else TRUE)

    updateSelectizeInput(
      session,
      "tipas_name_select",
      choices = sort(unique(filtered_by_country$Name)),
      #selected = NULL
      selected = character(0)
    )
  })

  # Reactive that responds to the apply tipas filter button
  filtered_tipa_data <- eventReactive(input$apply_tipa_filter, {
    filtered_data <- base_data()

    # Ensure input$tipas_country_select is not empty before filtering
    if (!is.null(input$tipas_country_select) && length(input$tipas_country_select) > 0) {
      filtered_data <- filtered_data %>%
        dplyr::filter(.data$Country %in% input$tipas_country_select)
    }

    if (!is.null(input$tipas_name_select) && length(input$tipas_name_select) > 0) {
      filtered_data <- filtered_data %>%
        dplyr::filter(.data$Name %in% input$tipas_name_select)
    }

    filtered_data
  }, ignoreNULL = FALSE)


  output$data_table_tipas <- DT::renderDT({
    #isolate({
    req(filtered_tipa_data(), dataset_type() == "tipas")

    sorted_data <- filtered_tipa_data() %>%
      dplyr::arrange(.data$Country)

    DT::datatable(
      sorted_data,  # Use selected_data consistently
      filter = "none",
      extensions = 'Buttons',
      class = "compact stripe hover nowrap",
      options = list(
        searching = FALSE,
        pageLength = 5,
        scrollY = FALSE,
        scrollX = TRUE,
        dom = 'Bftip',
        lengthChange = FALSE,
        buttons = list(
          list(
            extend = 'csv',
            text = 'Download CSV',
            exportOptions = list(modifier = list(modifier = list(page = 'all')))
          )
        )
      )
    )
    #})
  })


  output$stat4 <- renderText({
    #isolate({
    req(filtered_tipa_data(), dataset_type() == "tipas")
    #print(filtered_tipa_data())
    nrow(filtered_tipa_data())
    #})
  })

  output$stat5 <- renderText({
    #isolate({
    req(nrow(filtered_tipa_data()), dataset_type() == "tipas")
    paste(ceiling(filtered_tipa_data() %>%
                    dplyr::select("Area") %>% sum()))
    #})
    #scales::unit_format(unit = "km")(stat5_value)
    #HTML(paste0(scales::comma(stat5_value), " km", tags$sup("2")))
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
      req(filtered_tipa_data(), dataset_type() == "tipas")
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

}


# EXTRAS
# number of EDGE species
# output$stat1 <- renderText({
#   req(selected_data(), dataset_type() == "edge")
#   nrow(selected_data()) # Count rows of the dataset
# })

# output$stat2 <- renderText({
#   req(selected_data(), dataset_type() == "edge")
#   paste(selected_data() %>%
#           filter(ED_rank == min(ED_rank)) %>%
#           arrange(ED_rank) %>%  # Ensure ordering (even if there are ties)
#           pull(taxon_name))
# })
