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
}
