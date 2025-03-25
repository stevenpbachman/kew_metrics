#' @rdname risk_edge_ui
risk_edge_countries_ui <- function(id) {
  ns <- shiny::NS(id)
  page_fillable(
    navset_card_tab(
      sidebar = sidebar(
        selectizeInput(
          inputId = ns("edge_region_group_select"),
          label = "Select group",
          choices = NULL,
          multiple = FALSE
        ),
        selectizeInput(
          inputId = ns("edge_region_select"),
          label = "Select region",
          choices = NULL,
          multiple = TRUE
        ),
        input_task_button(
          id = ns("apply_edge_region_filter"),
          label = "Apply Filter"
        )
      ),
      full_screen = TRUE,
      title = "EDGE Countries",
      nav_panel("Countries Table", DT::DTOutput(ns("data_table_edgeranges"))),
      nav_panel(
        "Summary stats",
        layout_column_wrap(
          width = "250px",
          heights_equal = "row",
          value_box(
            title = "Number of EDGE species",
            full_screen = TRUE,
            value = textOutput(ns("edge_species_count"))
          ),
          value_box(
            title = "Highest ranked EDGE species",
            full_screen = TRUE,
            value = textOutput(ns("highest_ranked_edge"))
          ),
          value_box(
            title = "Highest ranked ED species",
            full_screen = TRUE,
            value = textOutput(ns("highest_ranked_ed"))
          ),
          value_box(
            title = "Sum of threatened ED",
            full_screen = TRUE,
            value = textOutput(ns("sum_of_threatened"))
          )
        )
      ),
      nav_panel("About", includeMarkdown(system.file("about", "about_edge_countries.Rmd",
        package = "kew.metrics"
      )))
    )
  )
}

#' @rdname risk_edge_ui
#' @param edge_countries Dataset for EDGE countries
risk_edge_countries_server <- function(id, edge_countries) {
  shiny::moduleServer(id, function(input, output, session) {

    # Populate filters with choices ----
    # NOTE: this is basically same as for EDGE species - can we reuse?
    observe({
      updateSelectizeInput(
        session,
        "edge_region_group_select",
        choices = sort(unique(edge_countries$group), decreasing = FALSE),
        server = TRUE
      )
    })

    # Observe selectize input for EDGE countries
    observe({
      updateSelectizeInput(
        session,
        "edge_region_select",
        choices = sort(unique(edge_countries$area), decreasing = FALSE),
        server = TRUE
      )
    })

    # Set filters for EDGE countries ----
    filtered_edge_region_data <- reactive({
      edge_countries %>%
        filter_if_truthy(.data$group, input$edge_region_group_select) %>%
        filter_if_truthy(.data$area, input$edge_region_select)
    }) %>%
      bindEvent(input$apply_edge_region_filter, ignoreNULL = FALSE)

    # Output for the EDGE countries(regions) datatable ----
    output$data_table_edgeranges <- DT::renderDT({
      req(filtered_edge_region_data())
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

    # Value boxes ----

    ## edge_species_count - number of EDGE species in region/countries data ----
    output$edge_species_count <- renderText({
      req(filtered_edge_region_data())
      length(unique(filtered_edge_region_data()$taxon_name)) # Count rows of the dataset
    })

    ## highest_ranked_edge - highest ranked EDGE species region/countries data ----
    output$highest_ranked_edge <- renderText({
      req(filtered_edge_region_data())
      filtered_edge_region_data() %>%
        dplyr::slice_min(order_by = .data$EDGE, with_ties = FALSE) %>%
        dplyr::pull("taxon_name")
    })

    ## highest_ranked_ed - highest ranked ED species from region/countries data ----
    output$highest_ranked_ed <- renderText({
      req(filtered_edge_region_data()) # Ensure data is available and it's EDGE type
      paste(filtered_edge_region_data() %>%
              dplyr::slice_min(order_by = .data$ED, with_ties = FALSE) %>%
              dplyr::pull("taxon_name"))
    })

    ## sum_of_threatened - sum of threatened ED ----
    output$sum_of_threatened <- renderText({
      req(filtered_edge_region_data()) # Ensure data is available and it's EDGE type
      filtered_edge_region_data() %>%
        dplyr::distinct(.data$taxon_name, .keep_all = TRUE) %>%  # Keep only one row per species
        dplyr::summarise(total_ED = ceiling(sum(.data$ED, na.rm = TRUE))) %>%  # Sum ED values
        dplyr::pull("total_ED")
    })
  })
}
