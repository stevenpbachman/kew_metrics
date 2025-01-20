
# reduce to 2 value boxes, #selected and stat
# add full screen to all cards


# add powo ID to EDGE data
# add TDWG density map for EDGE gymno as an example - leaflet
## try to make maps respond to the filtered data - may be slow
## use static powo distributions
# nav bar updates - add page to explain the site, metrics used etc. targets vs summary information
# add extinction risk? from predictions? - check column headers for filtering
# change values to yes no to make it easier to filter. Confident? yes, no etc.
# e.g. predictions, EDGE, use?


# add issues to github and remove from code here
# folder for each layer - include raw data and any transformation code
# road map for new features, data - go back to old notes
# underlying data to explain each dashboard page (ai assist?)
# consider sparklines for value boxes - which data to show?
# consider gauge to show progress against a target
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
library(scales)

# Raw data
# add TDWG ranges for each layer - join every time there is a selection?

Gymnosperms <- read.csv("01_data/EDGE_gymno.csv")
Angiosperms <- read.csv("01_data/EDGE_angio.csv")
Monocots <- read.csv("01_data/SRLI_2024.csv") %>% filter(group == "Monocots") %>% slice_head(n = 50)
Legumes <- read.csv("01_data/SRLI_2024.csv") %>% filter(group == "Legumes") %>% slice_head(n = 50)
tipas <- read.csv("01_data/TIPAs.csv")
tipas_shp <- st_read("01_data/tipas_shp.shp")
#predictions <- read.csv("01_data/predictions.csv")

# UI ----
ui <- page_navbar(
  title = "Kew Biodiversity Metrics",
  underline = TRUE,
  bg = "#008285",
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Inter")
  ),
  
  # first nav item - Diversity metrics page
  nav_panel(
    title = "Diversity",
    page_sidebar(
      sidebar = sidebar(
        accordion(
          accordion_panel(
            "Species Richness",
            selectInput(
              inputId = "datasetx",
              label = "Select Dataset:",
              choices = list(
                "None" = "",
                "Gymnosperms" = "gymno",
                "Ferns" = "ferns",
                "Angiosperms" = "angio"
              ),
              selected = ""
            )
          ),
          accordion_panel(  # Removed extra accordion() wrapper
            "Genetic",
            selectInput(
              inputId = "datasety",
              label = "Select Dataset:",
              choices = list(
                "None" = "",
                "PAFTOL" = "paftol",
                "Genome Size" = "genome"
              ),
              selected = ""
            )
          )
        )
      ),
      uiOutput("diversity_conditional") # Note: typo in "diversity"
    )
  ),
  
  # second nav item - Threats metrics page
  nav_panel(
    title = "Threats",
    page_sidebar(
      sidebar = sidebar(
        accordion(
          accordion_panel(
            "EDGE",
            selectInput(
              inputId = "dataset1",
              label = "Select Dataset:",
              choices = list(
                "None" = "",
                "Gymnosperms" = "gymno",
                "Angiosperms" = "angio"
              ),
              selected = ""
            )
          ),
          accordion_panel(
            "Red List",
            selectInput(
              inputId = "dataset2",
              label = "Select Dataset:",
              choices = list(
                "None" = "",
                "Legumes" = "legumes",
                "Monocots" = "monocots"
              ),
              selected = ""
            )
          )
        )
      ),
      uiOutput("threats_conditional") #Threats
    )
  ),
  
  # Third nav item - Response metrics
  nav_panel(
    title = "Conservation",
    page_sidebar(
      sidebar = sidebar(
        accordion(
          accordion_panel(
            "TIPAs",
            selectInput(
              inputId = "dataset3",
              label = "Select Dataset:",
              choices = list("None" = "", "TIPAs" = "tipas"),
              selected = ""
            )
          )
        )
      ),
      uiOutput("conservation_conditional")
    )
  ),
  
  # logo
  nav_spacer(),
  
  nav_panel(
    title = "About",
    page_sidebar(
      sidebar = sidebar(),
      layout_column_wrap(
        width = "800px",
        card(
          height = "600px",
          full_screen = TRUE,
          card_header("Predictions table"),
          DTOutput("data_table_predictions")
        )
      )
    )
  ),
  
  nav_item(
    tags$a(
      href = "https://powo.science.kew.org/",
      target = "_blank",  # This makes the link open in a new tab
      tags$img(
        src = "kew_logo_2015_small_w.png", 
        height = "30px", 
        style = "margin: 0 15px;"
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # organise by theme 
  # 1. Diversity ----
  # 2. Threats ----
  # Keep your original data reactive but rename it
  base_data <- reactive({
    if (!is.null(input$dataset1) && input$dataset1 != "") {
      switch(
        input$dataset1,
        "gymno" = Gymnosperms,
        "angio" = Angiosperms,
        "edge_index" = EDGE_Index
      )
    } else if (!is.null(input$dataset2) && input$dataset2 != "") {
      switch(input$dataset2,
             "legumes" = Legumes,
             "monocots" = Monocots)
    } else if (!is.null(input$dataset3) &&
               input$dataset3 != "") {
      switch(input$dataset3, "tipas" = tipas)
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
    else if (dataset_type() == "tipas") {
      if (!is.null(input$data_table_tipas_rows_all)) {
        base_data()[input$data_table_tipas_rows_all, ]
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
    } else if (!is.null(input$dataset3) && input$dataset3 != "") {
      "tipas"
    } else {
      NULL
    }
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
  
  # Conditional UI based on dataset selection
  output$threats_conditional <- renderUI({
    req(dataset_type())
    
    if (dataset_type() == "edge") {
      # UI elements for EDGE datasets
      page_fillable(
        card(
          height = "180px",
          class = "mb-3",  # Add margin bottom for spacing
          layout_column_wrap(
            width = 1/3,  # This makes each value box take up 1/3 of the width
            heights_equal = "row",
            value_box(
              title = "Number of selected species",
              value = textOutput("stat1"),
              #showcase = bs_icon("hash"),
              theme = "purple",
              full_screen = TRUE,
              height = "100px"
            ),
            value_box(
              title = "Highest ranking EDGE species",
              value = textOutput("stat2"),
              #showcase = bs_icon("tree"),
              theme = "teal",
              full_screen = TRUE,
              height = "100px"
            ),
            value_box(
              title = "Stat 3",
              value = textOutput("stat3"),
              #showcase = bs_icon("flower1"),
              theme = "pink",
              full_screen = TRUE,
              height = "100px"
            )
          )
        ),
        card(
          full_screen = TRUE,
          height = "600px",
          class = "mb-3",  # Add margin bottom for spacing
          card_header("EDGE Species Table"),
          DTOutput("data_table_edge"),
          card_footer("link or reference here?")
        ),
        navset_card_underline(
          height = "600px",
          full_screen = TRUE,
          title = "EDGE Maps",
          nav_panel("EDGE species richness", leafletOutput("EDGE_map1")),
          nav_panel(
            "Threatened evolutionary history",
            leafletOutput("EDGE_map2")
          )
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
          card_footer("link or reference here?"),
          plotOutput("redlist_plot")
        )
      )
    } 
  })
  
  # Response panel conditional UI
  output$conservation_conditional <- renderUI({
    req(dataset_type() == "tipas")
    
    layout_column_wrap(
      width = "500px",
      heights_equal = "row",
      card(
        height = "180px",
        layout_column_wrap(
          width = 1/2,
          heights_equal = "row",
          value_box(
            title = "Number of selected TIPAs",
            full_screen = TRUE,
            value = textOutput("stat4"),
            theme = "pink",
            height = "50px"
          ),
          value_box(
            title = "Sum of TIPAs areas (km sq)",
            full_screen = TRUE,
            value = textOutput("stat5"),
            theme = "teal",
            height = "50px"
          )
        )
      ),
      
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        card(
          height = "600px",
          full_screen = TRUE,
          card_header("TIPAs table"),
          DTOutput("data_table_tipas")
        ),
        card(
          height = "600px",
          full_screen = TRUE,
          card_header("TIPAs map"),
          leafletOutput("tipas_map")
        )
      )
    )
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
            exportOptions = list(modifier = list(modifier = list(page = 'all')))
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
            exportOptions = list(modifier = list(modifier = list(page = 'all')))
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
      setView(lng = 0,
              lat = 0,
              zoom = 2)
  })
  
  output$EDGE_map2 <- renderLeaflet({
    req(selected_data())
    req(dataset_type() == "edge")
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 0,
              zoom = 2)
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
  
  # 3. Response ----
  # add the conditional here
  output$data_table_tipas <- renderDT({
    #req(selected_data(), dataset_type() == "redlist")
    req(base_data(), dataset_type() == "tipas")
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
            exportOptions = list(modifier = list(modifier = list(page = 'all')))
          )
        )
      )
    )
  })
  
  output$data_table_predictions <- renderDT({
    datatable(predictions,
              filter = "top",
              extensions = 'Buttons',
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip')
    )

  })
  
  output$stat4 <- renderText({
    req(selected_data(), dataset_type() == "tipas") # Ensure data is available and it's EDGE type
    nrow(selected_data())
  })
  
  output$stat5 <- renderText({
    req(selected_data(), dataset_type() == "tipas") # Ensure data is available and it's EDGE type
    paste(selected_data() %>%
            select(Area) %>% sum())
    #scales::unit_format(unit = "km")(stat5_value)  
    #HTML(paste0(scales::comma(stat5_value), " km", tags$sup("2")))
  })
  
  
  output$tipas_map <- renderLeaflet({
    req(selected_data())
    req(dataset_type() == "tipas")
    #print(selected_data()) # just for debugging
    filtered_shp <- tipas_shp[tipas_shp$tips_nm %in% selected_data()$Name, ]
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = filtered_shp,
                  color = "red",    # Outline color
                  weight = 2,           # Outline thickness
                  fillColor = "red",# Fill color
                  fillOpacity = 0.75,
                  label = filtered_shp$tips_nm)    # Transparency) 
  })
  

}

# Run the application
shinyApp(ui = ui, server = server)
