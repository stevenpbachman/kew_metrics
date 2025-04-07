species_richness_ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    navset_card_tab(
      sidebar = sidebar(
        selectizeInput(
          inputId = ns("family"),
          label = "Select family",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = ns("genus"),
          label = "Select genus",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = ns("taxon_name"),
          label = "Select taxon name",
          choices = NULL,
          multiple = TRUE
        ),
        input_task_button(
          id = ns("set_filter"),
          label = "Apply Filter"
        )
      ),
      full_screen = TRUE,
      title = "Species Diversity",
      #tab_datatable_ui(id = ns("filtered_table"), title = "Table"),
      tab_datatable_ui(id = ns("taxon_by_country_count"), title = "Taxon count"),
      nav_panel("About", includeMarkdown(system.file("about", "about_species_richness.Rmd",
                                                      package = "kew.metrics")))
    )
  )
}

species_richness_server <- function(id, species) {
  moduleServer(id, function(input, output, session) {
    stopifnot(shiny::is.reactive(species))

    # Load WCVP data files ----
    all_species_data <- arrow::open_dataset(
      sources = system.file("01_data", "Diversity", "species_richness",
                            package = "kew.metrics", mustWork = TRUE),
      format = "parquet"
    )

    species_data <- reactive({
      req(species())
      dplyr::filter(all_species_data, .data$higher == .env$species())
    })

    get_unique_column_values <- function(.data, column) {
      .data %>%
        dplyr::distinct({{ column }}) %>%
        dplyr::collect() %>%
        dplyr::pull() %>%
        as.character() %>%
        sort(decreasing = FALSE)
    }

    # Set filters for diversity ----
    observe({
      shiny::updateSelectizeInput(
        session = session,
        inputId = "family",
        choices = get_unique_column_values(species_data(), .data$family),
        server = FALSE
      )
      updateSelectizeInput(
        session = session,
        inputId = "genus",
        choices = get_unique_column_values(species_data(), .data$genus),
        server = TRUE
      )
      updateSelectizeInput(
        session = session,
        inputId = "taxon_name",
        choices = get_unique_column_values(species_data(), .data$taxon_name),
        server = TRUE
      )
    })

    # Refine filters based on the other filters ----
    # Update genus options based on family ----
    filtered_by_family <- reactive({
      filter_if_truthy(.data = species_data(), col = .data$family, ifTruthy = input$family)
    })

    observe({
      updateSelectizeInput(
        session,
        "genus",
        choices = get_unique_column_values(filtered_by_family(), .data$genus),
        selected = character(0),
        server = TRUE
      )
    }) %>%
      bindEvent(input$family, ignoreNULL = TRUE, ignoreInit = TRUE)

    ## Update genus options based on group and family ----
    filtered_by_genus <- reactive({
      filter_if_truthy(filtered_by_family(), .data$genus, input$genus)
    })

    observe({
      updateSelectizeInput(
        session,
        "taxon_name",
        choices = get_unique_column_values(filtered_by_genus(), .data$taxon_name),
        selected = character(0),
        server = TRUE
      )
    }) %>%
      bindEvent(c(input$family, input$genus), ignoreNULL = TRUE, ignoreInit = TRUE)

    # Create filtered_data reactive that responds to the set_filter button ----
    filtered_data <- reactive({
      filter_if_truthy(filtered_by_genus(), .data$taxon_name, input$taxon_name)
    }) %>%
      bindEvent(input$set_filter)

    # species datatable ----
    table_columns <- c(
      "powo_id",
      "family",
      "genus",
      "species",
      "taxon_name",
      "taxon_authors",
      "geographic_area",
      "area",
      "accepted_plant_name_id",
      "higher"
    )

    table_data <- shiny::reactive({
      filtered_data() %>%
        dplyr::select(!!!table_columns) %>%
        dplyr::distinct(.data$species, .keep_all = TRUE) %>%
        dplyr::collect()
    })

    # Alternative approach below - trying to reduce size of table for display
    # De-duplicate by species by going in and out of duckdb before collect()
    # Even so, the species table crashes on shiny live version

    # table_data <- shiny::reactive({
    #   filtered_data() %>%
    #     arrow::to_duckdb() %>%
    #     dplyr::select(dplyr::all_of(table_columns)) %>%
    #     dplyr::distinct(species, .keep_all = TRUE) %>%
    #     arrow::to_arrow() %>%
    #     dplyr::collect()
    # })

    tab_datatable_server(id = "filtered_table", .data = table_data)

    # Example: Diversity by country ----
    # Number of distinct taxon names that appear for each country.
    taxon_by_country_count <- shiny::reactive({
      filtered_data() %>%
        dplyr::group_by(.data$continent, .data$region, .data$area) %>%
        dplyr::summarise(taxon_count = dplyr::n_distinct(.data$taxon_name)) %>%
        dplyr::arrange(dplyr::desc(.data$taxon_count)) %>%
        dplyr::collect()
    })

    tab_datatable_server(id = "taxon_by_country_count", .data = taxon_by_country_count)
  })
}
