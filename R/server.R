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

  ## GBF ----
  requested_dataset <- gbf_indicators_server(id = "gbf_indicators")

  # When the GBF Indicators module detects a user selected the dataset, the page should be
  # changed to display the requested page.
  # NOTE: We need to do this outside of the module so that shiny::update methods are able to
  # see HTML IDs outside of the module's namespace.
  observe({
    req(requested_dataset())

    shiny::updateTabsetPanel(session, "main_nav", selected = requested_dataset()$navigation)
    shiny::updateSelectInput(session,
                             shiny::NS(tolower(requested_dataset()$navigation),
                                       requested_dataset()$dataset_input_id),
                             selected = requested_dataset()$dataset)
  })
}
