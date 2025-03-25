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
  risk_server(id = "risk")

  # Set the UI for the Risk tab
  output$Risk_conditional <- renderUI({
    req(dataset_type())

    if (dataset_type() == "edge") {

      if (input$dataset1 == "edgecountries") {

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

  # Leaflet map outputs

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

  conservation_server(id = "conservation")

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
