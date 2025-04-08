#' Risk ecosystems module
#'
#' Create the UI and Server components for a risk ecosystems page.
#' @inheritParams conservation_ui
#' @return Either the HTML UI components, or the corresponding server code for the module.
#' @rdname risk_ecosystem_ui
risk_ecosystem_ui <- function(id) {
  ns <- shiny::NS(id)
  page_fillable(
    navset_card_tab(
      full_screen = TRUE,
      title = "Red List of Ecosystems",
      sidebar = sidebar(
        selectizeInput(
          inputId = ns("ecosystem_cat_select"),
          label = "Select Category",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = ns("ecosystem_realm_select"),
          label = "Select Realm",
          choices = NULL,
          multiple = TRUE
        ),
        input_task_button(id = ns("apply_eco_filter"), label = "Apply Filter")
      ),
      tab_datatable_ui(id = ns("filtered_table")),
      nav_panel(
        "Summary stats",
        # layout_column_wrap(
        #   width = "100%",
        #   plotly::plotlyOutput(ns("cumulative_area_plot"), height = "280px")
        # ),
        layout_column_wrap(
          width = "250px",
          heights_equal = "row",
          value_box(
            title = "Number of selected assessments",
            full_screen = FALSE,
            value = textOutput(ns("eco_count"))
          ),
          value_box(
            title = shiny::tags$span(shiny::HTML("Sum of Ecosystem areas (km<sup>2</sup>)")),
            full_screen = FALSE,
            value = htmlOutput(ns("eco_area"), inline = TRUE)
          )
        )
      ),

      #nav_panel("Status distribution", ggiraph::girafeOutput(ns("redlist_plot"))),
      #nav_panel("Maps", leaflet::leafletOutput(ns("SRLI_map1"))),
      nav_panel(
        "About",
        includeMarkdown(system.file("about", "about_ecosystem_redlist.Rmd",
                                    package = "kew.metrics"))
      )
    )
  )
}

#' @rdname risk_ecosystem_ui
risk_ecosystem_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    eco_global <- readr::read_csv(
      system.file("01_data", "EcosystemRedList", "eco_assessments.csv", package = "kew.metrics", mustWork = TRUE)
    )

    # Populate filters with choices ----
    observe({
      updateSelectizeInput(
        session,
        "ecosystem_cat_select",
        choices = sort(unique(eco_global$Category), decreasing = FALSE),
        server = TRUE
      )
    })

    ## Ecosystem realm ----
    observe({
      updateSelectizeInput(
        session,
        "ecosystem_realm_select",
        choices = sort(unique(eco_global$Realm), decreasing = FALSE),
        server = FALSE
      )
    })

    # # Ecosystem reactive for filter button ----
    filtered_eco_data <- reactive({
      filtered_data <- eco_global

      filtered_data <- filter_if_truthy(filtered_data, Category, input$ecosystem_cat_select)
      filtered_data <- filter_if_truthy(filtered_data, Realm, input$ecosystem_realm_select)

      filtered_data
    }) %>%
      bindEvent(input$apply_eco_filter, ignoreNULL = FALSE)

    # Ecosystem datatable
    tab_datatable_server(id = "filtered_table", .data = filtered_eco_data)

    # Summary statistics
    output$eco_count <- renderText({
      req(filtered_eco_data())
      nrow(filtered_eco_data())
    })

    output$eco_area <- renderUI({
      req(nrow(filtered_eco_data()) > 0L)
      filtered_eco_data() %>%
        dplyr::summarise(total_area = sum(.data$EOO)) %>%
        dplyr::pull("total_area") %>%
        ceiling() %>%
        prettyNum(big.mark = ",") %>%
        shiny::HTML("km<sup>2</sup>")
    })


})
}


