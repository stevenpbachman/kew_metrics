
# add powo ID to EDGE data
# move to github and version control
# publish test version
# add TDWG density map for EDGE gymno as an example - leaflet
## try to make maps respond to the filtered data - may be slow
## use static powo distributions
# nav bar updates - add page to explain the site, metrics used etc. targets vs summary information


# road map for new features, data - go back to old notes
# underlying data to explain each dashboard page (ai assist?)
# add a map - should it react to the selection? yes I think. 
# Add the distributions first, then join and then map the selection
# map could have two tabs -
#  1) count of EDGE species per TDWG
#  2) count of EDGE species
# how heavy is the TDWG layer to display - maybe not bad if you only show selected regions?
# add download option to get selected or full datatable? link to DOI and original publication
# go back to bslib video and add more functionality e.g. action button? https://www.youtube.com/watch?v=vzXTFbnKAqc
# color:#008285 (kew colour for apps)
# filter doesn't work that well for things like taxus that could be Amentotaxus
# add kew logo - seems difficult!
# add series of checks to make sure each dataset works before adding



library(shiny)

library(bslib)
library(bsicons)
library(DT)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sysfonts)

# Raw data
# add TDWG ranges for each layer - join every time there is a selection?

Gymnosperms <- read.csv("~/kew_metrics/01_data/EDGE_gymno.csv")
Angiosperms <- read.csv("~/kew_metrics/01_data/EDGE_angio.csv")
Monocots <- read.csv("01_data/SRLI_2024.csv") %>% filter(group == "Monocots") %>% slice_head(n = 50)
Legumes <- read.csv("01_data/SRLI_2024.csv") %>% filter(group == "Legumes") %>% slice_head(n = 50)

# UI Definition
ui <- page_sidebar(
  title = "Kew Biodiveristy Metrics",

    theme = bs_theme(
    bootswatch = "zephyr",
    base_font = font_google("Inter"),
    navbar_bg = "#008285"
  ),
  
  # title = tags$span(
  #   tags$img(src = "kew.jpg", height = "30px", style = "margin-right: 10px;"),
  #   tags$span(style = "color: white;", "Kew Metrics")
  # ),
  # theme = bs_theme(
  #   bootswatch = "zephyr",
  #   base_font = font_google("Inter"),
  #   navbar_bg = "#008285",
  #   navbar_light_brand_color = "#ffffff",
  #   navbar_light_brand_hover_color = "#ffffff"
  # ),
  
  sidebar = sidebar(
    accordion(
      accordion_panel(
        "EDGE",
        selectInput(
          inputId = "dataset1",
          label = "Select Dataset:",
          choices = list("None" = "", "Gymnosperms" = "gymno", "Angiosperms" = "angio"),
          selected = ""
        )
      ),
      accordion_panel(
        "Red List",
        selectInput(
          inputId = "dataset2",
          label = "Select Dataset:",
          choices = list("None" = "", "Legumes" = "legumes", "Monocots" = "monocots"),
          selected = ""
        )
      )
    )
  ),
  
  # Main content area with conditional UI
  uiOutput("conditional_content")
  
)

# Server logic
server <- function(input, output, session) {
  
  # Keep your original data reactive but rename it
  base_data <- reactive({
    if (!is.null(input$dataset1) && input$dataset1 != "") {
      switch(input$dataset1,
             "gymno" = Gymnosperms,
             "angio" = Angiosperms
      )
    } else if (!is.null(input$dataset2) && input$dataset2 != "") {
      switch(input$dataset2,
             "legumes" = Legumes,
             "monocots" = Monocots
      )
    } else {
      NULL
    }
  })
  
  
  # Add new reactive for filtered data
  selected_data <- reactive({
    req(base_data())
    if (dataset_type() == "edge") {
      if (!is.null(input$data_table_edge_rows_all)) {
        base_data()[input$data_table_edge_rows_all, ]
      } else {
        base_data()
      }
    } else if (dataset_type() == "redlist") {
      if (!is.null(input$data_table_redlist_rows_all)) {
        base_data()[input$data_table_redlist_rows_all, ]
      } else {
        base_data()
      }
    }
  })
  
  # Reactive expression to determine which dataset type is selected
  dataset_type <- reactive({
    if (!is.null(input$dataset1) && input$dataset1 != "") {
      "edge"
    } else if (!is.null(input$dataset2) && input$dataset2 != "") {
      "redlist"
    } else {
      NULL
    }
  })
  
  # Automatically reset other selection to "None" when one dataset is selected
  observeEvent(input$dataset1, {
    if (input$dataset1 != "") {
      updateSelectInput(session, "dataset2", selected = "")
    }
  })
  
  observeEvent(input$dataset2, {
    if (input$dataset2 != "") {
      updateSelectInput(session, "dataset1", selected = "")
    }
  })
  
  # Conditional UI based on dataset selection
  # In the conditional UI section, update the renderUI code:
  output$conditional_content <- renderUI({
    req(dataset_type())
    
    if (dataset_type() == "edge") {
      # UI elements for EDGE datasets
      layout_column_wrap(
        width = "500px",
        heights_equal = "row",
        value_box(
          title = "Number of selected species",
          value = textOutput("stat1"),
          showcase = bs_icon("hash"),
          full_screen = TRUE,
          theme = "purple"
        ),
        value_box(
          title = "Highest ranking EDGE species",
          value = textOutput("stat2"),
          showcase = bs_icon("tree"),
          full_screen = TRUE,
          theme = "teal"
        ),
        value_box(
          title = "Stat 3",
          value = textOutput("stat3"),
          showcase = bs_icon("flower1"),
          full_screen = TRUE,
          theme = "pink"
        ),
        card(
          height = "600px",
          full_screen = TRUE,
          card_header("EDGE Species Table"),
          DTOutput("data_table_edge")
        ),
        navset_card_underline(
          height = "600px",
          full_screen = TRUE,
          title = "EDGE Maps",
          nav_panel("EDGE species richness", leafletOutput("EDGE_map1")),
          nav_panel("Threatened evolutionary history", leafletOutput("EDGE_map2"))
        )
      )
    } else if (dataset_type() == "redlist") {
      # UI elements for Red List datasets
      layout_column_wrap(
        width = "500px",
        heights_equal = "row",
        card(
          height = "600px",
          full_screen = TRUE,
          card_header("Red List Species Table"),
          DTOutput("data_table_redlist")
        ),
        card(
          height = "600px",
          full_screen = TRUE,
          card_header("Red List Status Distribution"),
          plotOutput("redlist_plot")
        )
      )
    }
  })
  
  output$data_table_edge <- renderDT({
    #req(selected_data(), dataset_type() == "edge")
    req(base_data(), dataset_type() == "edge")
    datatable(
      base_data(),
      filter = "top", 
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'csv',
            text = 'Download CSV',
            exportOptions = list(
              modifier = list(
                modifier = list(page = 'all')
                        )
            )
          )
        )
      )
    )
  })
  
  output$data_table_redlist <- renderDT({
    #req(selected_data(), dataset_type() == "redlist")
    req(base_data(), dataset_type() == "redlist")
    datatable(
      base_data(),
      filter = "top",
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'csv',
            text = 'Download CSV',
            exportOptions = list(
              modifier = list(
                modifier = list(page = 'all')
              )
            )
          )
        )
      )
    )
  })
  
  # Value box statistics
  output$stat1 <- renderText({
    req(selected_data(), dataset_type() == "edge") # Ensure data is available and it's EDGE type
    nrow(selected_data()) # Count rows of the dataset
  })
  
  output$stat2 <- renderText({
    req(selected_data(), dataset_type() == "edge") # Ensure data is available and it's EDGE type
    paste(selected_data() %>%
            filter(ED_rank == min(ED_rank)) %>%
            arrange(ED_rank) %>%  # Ensure ordering (even if there are ties)
            pull(Taxon))
  })
  
  output$stat3 <- renderText({
    req(selected_data(), dataset_type() == "edge") # Ensure data is available and it's EDGE type
    nrow(selected_data()) # Count rows of the dataset
  })
  
  # Map outputs (only for EDGE datasets)
  output$EDGE_map1 <- renderLeaflet({
    req(selected_data())
    req(dataset_type() == "edge")
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  output$EDGE_map2 <- renderLeaflet({
    req(selected_data())
    req(dataset_type() == "edge")
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  # Plot output (only for Red List datasets)
  output$redlist_plot <- renderPlot({
    req(selected_data())
    req(dataset_type() == "redlist")
    
    # Replace with your actual Red List plotting code
    # Create a summary table with counts
    summary_data <- selected_data() %>%
      count(RL_2020)
    
    ggplot(summary_data, aes(x = RL_2020, y = n, fill = RL_2020)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Red List Categories Count", x = "Red List Category", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)


