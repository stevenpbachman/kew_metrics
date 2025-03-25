#' Risk redlist module
#'
#' Create the UI and Server components for a risk redlist page.
#' @inheritParams conservation_ui
#' @return Either the HTML UI components, or the corresponding server code for the module.
#' @rdname risk_redlist_ui
risk_redlist_ui <- function(id) {
  ns <- shiny::NS(id)
  page_fillable(
    navset_card_tab(
      full_screen = TRUE,
      title = "Red List Index species",
      sidebar = sidebar(
        selectizeInput(
          inputId = ns("srli_group_select"),
          label = "Select group",
          choices = NULL,
          multiple = FALSE
        ),
        input_task_button(id = ns("apply_srli_filter"), label = "Apply Filter")
      ),
      nav_panel("Table", fillable = TRUE, DT::DTOutput(ns("data_table_redlist"))),
      nav_panel("Status distribution", ggiraph::girafeOutput(ns("redlist_plot"))),
      nav_panel("Maps", leaflet::leafletOutput(ns("SRLI_map1"))),
      nav_panel(
        "About",
        includeMarkdown(system.file("about", "about_global_sampled_RLI.Rmd",
                                    package = "kew.metrics"))
      )
    )
  )
}

#' @rdname risk_redlist_ui
#' @param edge_countries Dataset for EDGE countries
risk_redlist_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    sampled_global <- readr::read_csv(
      system.file("01_data", "RedList", "SRLI_2024.csv", package = "kew.metrics")
    )

    # Populate filters with choices ----
    observe({
      updateSelectizeInput(
        session,
        "srli_group_select",
        choices = sort(unique(sampled_global$group), decreasing = FALSE),
        server = TRUE
      )
    })

    # srli reactive for filter button ----
    filtered_srli_data <- reactive({
      filtered_data <- sampled_global %>%
        dplyr::filter(.data$group %in% input$srli_group_select)
    }) %>%
      bindEvent(input$apply_srli_filter)

    output$data_table_redlist <- DT::renderDT({
      req(filtered_srli_data())

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
      req(filtered_srli_data())

      # Create a summary table with counts
      summary_data <- filtered_srli_data() %>%
        dplyr::count(.data$RL_2020)

      # Create the interactive plot
      p <- ggplot2::ggplot(
        summary_data,
        ggplot2::aes(
          x = .data$RL_2020,
          y = .data$n,
          fill = .data$RL_2020,
          tooltip = paste("Category:", .data$RL_2020, "\nCount:", .data$n)  # Add tooltips
        )
      ) +
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
      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_hover(css = "fill-opacity:0.8;stroke:gray;cursor:pointer;"),
          ggiraph::opts_tooltip(css = "background-color:white;color:black;border-radius:5px;padding:5px;")
        )
      )
    })
  })
}
