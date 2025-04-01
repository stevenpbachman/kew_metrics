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
      tab_datatable_ui(id = ns("filtered_table")),
      nav_panel(
        "Maps",
        leaflet_choropleth_ui(
          id = ns("choropleth"),
          label = "Choose a metric to display",
          choices = c(
            "Species Richness" = "richness",
            "Threatened Evolutionary History" = "threat"
          )
        )
      ),
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
      system.file("01_data", "EDGE", "EDGEspecies_matched.csv",
                  package = "kew.metrics", mustWork = TRUE)
    )

    # Set filters for EDGE species ----
    ## EDGE group - Angiosperms or Gymnosperms ----
    observe({
      updateSelectizeInput(
        session,
        "edge_group_select",
        choices = sort(unique(edge_species$group), decreasing = FALSE),
        server = FALSE
      )
    })

    ## EDGE family ----
    observe({
      updateSelectizeInput(
        session,
        "edge_family_select",
        choices = sort(unique(edge_species$family), decreasing = FALSE),
        server = FALSE
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
        selected = character(0),
        server = FALSE
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
        selected = character(0),
        server = TRUE
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
        selected = character(0),
        server = TRUE
      )
    }) %>%
      bindEvent(c(input$edge_group_select, input$edge_family_select, input$edge_genus_select))

    # Create filtered_edge_data reactive that responds to the apply_edge_filter button ----
    filtered_edge_data <- reactive({
      filter_if_truthy(filtered_by_genus(), .data$taxon_name, input$edge_species_select)
    }) %>%
      bindEvent(input$apply_edge_filter, ignoreNULL = FALSE)

    # EDGE species datatable ----
    tab_datatable_server(id = "filtered_table", .data = filtered_edge_data)

    # Leaflet Map output ----
    leaflet_choropleth_server(
      id = "choropleth",
      data = filtered_edge_data,
      edge_countries = edge_countries,
      shapefile = system.file("01_data", "wgsrpd_simple.rds",
                              package = "kew.metrics", mustWork = TRUE)
    )

  })
}
