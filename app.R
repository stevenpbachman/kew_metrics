
library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sysfonts)
library(scales)
library(sf)
library(httr)
library(plotly)

# Raw data
# add TDWG ranges for each layer - join every time there is a selection?

EDGEspecies <- read.csv("01_data/EDGE/EDGEspecies_matched.csv")
EDGEcountries <- read.csv("01_data/EDGE/edge_ranges.csv")
#Angiosperms <- read.csv("01_data/EDGE/EDGE_angio.csv")
Monocots <- read.csv("01_data/RedList/SRLI_2024.csv") %>% filter(group == "Monocots") %>% slice_head(n = 50)
Legumes <- read.csv("01_data/RedList/SRLI_2024.csv") %>% filter(group == "Legumes") %>% slice_head(n = 50)
tipas <- read.csv("01_data/TIPAS/TIPAs.csv")
tipas_shp <- st_read("01_data/TIPAS/TIPA_Composite_POLYGON/TIPA_Composite_POLYGON.shp")
tipas_shp <- st_zm(tipas_shp, drop = TRUE, what = "ZM")

 

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
              label = "Select layer:",
              choices = list(
                "None" = "",
                "Gymnosperms" = "gymno",
                "Ferns" = "ferns",
                "Angiosperms" = "angio"
              ),
              selected = ""
            )
          ),
          accordion_panel(  
            "Genetic",
            selectInput(
              inputId = "datasety",
              label = "Select layer:",
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
      uiOutput("diversity_conditional") 
    )
  ),
  
  # second nav item - Risk metrics page
  nav_panel(
    title = "Risk",
    page_sidebar(
      sidebar = sidebar(
        accordion(
          accordion_panel(
            "EDGE",
            selectInput(
              inputId = "dataset1",
              label = "Select layer:",
              choices = list(
                "None" = "",
                "Species" = "edgespecies",
                "Countries" = "edgecountries",
                "Index" = "edgeindex",
                "Zones" = "edgezones"
              ),
              selected = ""
            )
          ),
          accordion_panel(
            "Red List",
            selectInput(
              inputId = "dataset2",
              label = "Select layer:",
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
      uiOutput("Risk_conditional") 
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
              label = "Select layer:",
              choices = list("None" = "", "TIPAs" = "tipas"),
              selected = ""
            )
          )
        ),
      ),
      uiOutput("conservation_conditional")
    )
  ),
  
  # Add JavaScript here, before other UI elements
  tags$head(
    tags$script("
      $(document).on('bslib.card', function(event) {
        if (event.detail.fullScreen) {
          Plotly.relayout(event.target.querySelector('.plotly'), {'xaxis.visible': true});
        } else {
          Plotly.relayout(event.target.querySelector('.plotly'), {'xaxis.visible': false});
        }
      });
    ")
  ),
  
  # shifting it to the right of navbar
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
          card_header("What this website is all about..."),
          DTOutput("data_table_predictions")
        )
      )
    )
  ),
  
  # logo etc.
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
  # 2. Risk ----
  # Keep your original data reactive but rename it
  base_data <- reactive({
    #req(input$dataset1)
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
             "legumes" = Legumes,
             "monocots" = Monocots)
    } else if (!is.null(input$dataset3) &&
               input$dataset3 != "") {
      switch(input$dataset3, "tipas" = tipas)
    } else {
      NULL
    }
  })
  

  selected_data <- reactive({
    req(base_data())
    req(dataset_type())
    
    current_data <- base_data()
    
    switch(dataset_type(),
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
  
  output$Risk_conditional <- renderUI({
    req(dataset_type())
    
    if (dataset_type() == "edge") {
      if (input$dataset1 == "edgespecies") {
        page_fillable(
          navset_card_tab(
            sidebar = sidebar(
              selectizeInput(
                inputId = "edge_group_select", 
                label = "Select group",
                choices = NULL,
                multiple = FALSE
              ),
              selectizeInput(
                inputId = "edge_family_select", 
                label = "Select Family",
                choices = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                inputId = "edge_genus_select", 
                label = "Select Genus",
                choices = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                inputId = "edge_species_select", 
                label = "Select Species",
                choices = NULL,
                multiple = TRUE
              ),
              actionButton(
                inputId = "apply_edge_filter", 
                label = "Apply Filter", 
                class = "btn-primary"
              )
            ),
            full_screen = TRUE,
            title = "EDGE species",
            nav_panel("Table", fillable = TRUE, DTOutput("data_table_edgespecies")), 
            nav_panel("Maps", leafletOutput("EDGE_map1"))
          )
        )
      } else if (input$dataset1 == "edgecountries") {
        page_fillable(
          navset_card_tab(
            sidebar = sidebar(
              selectizeInput(
                inputId = "edge_region_group_select", 
                label = "Select group",
                choices = NULL,
                multiple = FALSE
              ),
              selectizeInput(
                inputId = "edge_region_select", 
                label = "Select region",
                choices = NULL,
                multiple = TRUE
              ),
              actionButton(
                inputId = "apply_edge_region_filter", 
                label = "Apply Filter", 
                class = "btn-primary"
              )
              ),
            full_screen = TRUE,
            title = "EDGE Countries",
            nav_panel("Countries Table", DTOutput("data_table_edgeranges")),
            nav_panel("Summary stats",
                      layout_column_wrap(
                        width = "250px",
                        heights_equal = "row",
                            value_box(
                              title = "Number of EDGE species",
                              full_screen = TRUE,
                              value = textOutput("stat_e1")
                        ),
                        value_box(
                          title = "Highest ranked EDGE species",
                          full_screen = TRUE,
                          value = textOutput("stat_e2") 
                        ),
                        value_box(
                          title = "Highest ranked ED species",
                          full_screen = TRUE,
                          value = textOutput("stat_e3")
                        ),
                        value_box(
                          title = "Sum of threatened ED",
                          full_screen = TRUE,
                          value = textOutput("stat_e4")
                        )
                        
                      )
        )
      )
        )
      }
    } else if (dataset_type() == "redlist") {
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
    
    page_fillable(
      navset_card_tab(
        sidebar = sidebar(
          selectizeInput(
            inputId = "tipas_country_select", 
            label = "Select country",
            choices = NULL,
            multiple = FALSE
          ),
          selectizeInput(
            inputId = "tipas_name_select", 
            label = "Select TIPA",
            choices = NULL,
            multiple = FALSE
          ),

          actionButton(
            inputId = "apply_tipa_filter", 
            label = "Apply Filter", 
            class = "btn-primary"
          )
        ),
        full_screen = TRUE,
        title = "TIPAs",
        nav_panel("Table", fillable = TRUE, DTOutput("data_table_tipas")), 
        nav_panel("Maps", leafletOutput("tipas_map")),
        nav_panel("Summary stats",
                  layout_column_wrap(
                    width = "250px",
                    heights_equal = "row",
                    value_box(
                      title = "Number of EDGE selected TIPAs",
                      full_screen = TRUE,
                      value = textOutput("stat4")
                    ),
                    value_box(
                      title = "Sum of TIPAs areas (km sq)",
                      full_screen = TRUE,
                      value = textOutput("stat5") 
                    )
                    
                  )
        )
      )
    )
    
    
  })
  
  
  # selectize input for the edge species filter
  observe({
    req(base_data(), dataset_type() == "edge")
    
    updateSelectizeInput(
      session, 
      "edge_group_select", 
      choices = sort(unique(base_data()$group), decreasing = FALSE),
      server = TRUE
    )
  })
  
  observe({
    req(base_data(), dataset_type() == "edge")
    
    updateSelectizeInput(
      session, 
      "edge_family_select", 
      choices = sort(unique(base_data()$family), decreasing = FALSE),
      server = TRUE
    )
  })
  
  observe({
    req(base_data(), dataset_type() == "edge")
    
    updateSelectizeInput(
      session, 
      "edge_genus_select", 
      choices = sort(unique(base_data()$genus), decreasing = FALSE),
      server = TRUE
    )
  })
  
  observe({
    req(base_data(), dataset_type() == "edge")
    
    updateSelectizeInput(
      session, 
      "edge_species_select", 
      choices = sort(unique(base_data()$taxon_name), decreasing = FALSE),
      server = TRUE
    )
  })
  
  # selectize input for the edge countries filter
  observe({
    req(base_data(), dataset_type() == "edge")
    
    updateSelectizeInput(
      session, 
      "edge_region_select", 
      #choices = unique(base_data()$area),
      choices = sort(unique(base_data()$area), decreasing = FALSE),
      server = TRUE
    )
  })
  
  # selectize input for the edge countries filter - groups
  observe({
    req(base_data(), dataset_type() == "edge")
    
    updateSelectizeInput(
      session, 
      "edge_region_group_select", 
      #choices = unique(base_data()$area),
      choices = sort(unique(base_data()$group), decreasing = FALSE),
      server = TRUE
    )
  })
  
  # selectize input for the tipas country  filter
  observe({
    req(base_data(), dataset_type() == "tipas")
    
    updateSelectizeInput(
      session, 
      "tipas_country_select", 
      choices = sort(unique(base_data()$Country), decreasing = FALSE),
      server = TRUE
    )
  })
  
  # selectize input for the tipas namefilter
  observe({
    req(base_data(), dataset_type() == "tipas")
    
    updateSelectizeInput(
      session, 
      "tipas_name_select", 
      choices = sort(unique(base_data()$Name), decreasing = FALSE),
      server = TRUE
    )
  })
  
  # Make the TIPA name selection dependent on country selection
  observeEvent(input$tipas_country_select, {
    filtered_by_country <- base_data() %>%
      filter(if(!is.null(input$tipas_country_select)) Country %in% input$tipas_country_select else TRUE)
    
    updateSelectizeInput(
      session,
      "tipas_name_select",
      choices = sort(unique(filtered_by_country$Name)),
      selected = character(0)
    )
  })
  
  
  # Create reactive for filtered data at each level
  filtered_by_group <- reactive({
    base_data() %>%
      filter(if(length(input$edge_group_select) > 0) group %in% input$edge_group_select else TRUE)
  })
  
  filtered_by_family <- reactive({
    filtered_by_group() %>%
      filter(if(length(input$edge_family_select) > 0) family %in% input$edge_family_select else TRUE)
  })
  
  filtered_by_genus <- reactive({
    filtered_by_family() %>%
      filter(if(length(input$edge_genus_select) > 0) genus %in% input$edge_genus_select else TRUE)
  })
  
  # Update dropdowns based on filtered data
  observeEvent(input$edge_group_select, {
    updateSelectizeInput(
      session,
      "edge_family_select",
      choices = sort(unique(filtered_by_group()$family)),
      selected = character(0)
    )
  })
  
  observeEvent(c(input$edge_group_select, input$edge_family_select), {
    updateSelectizeInput(
      session,
      "edge_genus_select",
      choices = sort(unique(filtered_by_family()$genus)),
      selected = character(0)
    )
  })
  
  observeEvent(c(input$edge_group_select, input$edge_family_select, input$edge_genus_select), {
    updateSelectizeInput(
      session,
      "edge_species_select",
      choices = sort(unique(filtered_by_genus()$taxon_name)),
      selected = character(0)
    )
  })
  
  # Final filtered_edge_data reactive that responds to the apply filter button
  filtered_edge_data <- eventReactive(input$apply_edge_filter, {
    filtered_by_genus() %>%
      filter(if(length(input$edge_species_select) > 0) taxon_name %in% input$edge_species_select else TRUE)
  }, ignoreNULL = FALSE)
  
  # Reactive that responds to the apply tipas filter button
  filtered_tipa_data <- eventReactive(input$apply_tipa_filter, {
    filtered_data <- base_data()
    
    if (!is.null(input$tipas_country_select) && input$tipas_country_select != "") {
      filtered_data <- filtered_data %>% 
        filter(Country %in% input$tipas_country_select)
    }
    
    if (!is.null(input$tipas_name_select) && input$tipas_name_select != "") {
      filtered_data <- filtered_data %>% 
        filter(Name %in% input$tipas_name_select)
    }
    
    filtered_data
  }, ignoreNULL = FALSE)
 
  
  output$data_table_edgespecies <- renderDT({
    req(base_data(), dataset_type() == "edge")
    
    datatable(
      filtered_edge_data(),
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
  
  # Create a reactive for filtered edge countries data
  filtered_edge_region_data <- eventReactive(input$apply_edge_region_filter, {
    filtered_data <- base_data()
  
    if (length(input$edge_region_group_select) > 0) {
      filtered_data <- filtered_data %>% 
        filter(group %in% input$edge_region_group_select)
    }
    
    if (length(input$edge_region_select) > 0) {
      filtered_data <- filtered_data %>% 
        filter(area %in% input$edge_region_select)
    }
    
    filtered_data
  }, ignoreNULL = FALSE)
  
  output$data_table_edgeranges <- renderDT({
    req(base_data(), dataset_type() == "edge")
    datatable(
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
            pull(taxon_name))
  })
  
  output$stat_e1 <- renderText({
    req(selected_data(), dataset_type() == "edge") # Ensure data is available and it's EDGE type
    req(filtered_edge_region_data())
    paste(length((unique(filtered_edge_region_data()$taxon_name))) ) # Count rows of the dataset
  })

  output$stat_e2 <- renderText({
    req(selected_data(), dataset_type() == "edge") # Ensure data is available and it's EDGE type
    req(filtered_edge_region_data())
    paste(filtered_edge_region_data() %>%
            slice_min(order_by = EDGE, with_ties = FALSE) %>%
            pull(taxon_name))
  })
  
  output$stat_e3 <- renderText({
    req(selected_data(), dataset_type() == "edge") # Ensure data is available and it's EDGE type
    req(filtered_edge_region_data())
    paste(filtered_edge_region_data() %>%
            slice_min(order_by = ED, with_ties = FALSE) %>%
            pull(taxon_name))
  })
  
  output$stat_e4 <- renderText({
    req(selected_data(), dataset_type() == "edge") # Ensure data is available and it's EDGE type
    req(filtered_edge_region_data())
    total_ED <- filtered_edge_region_data() %>%
      distinct(taxon_name, .keep_all = TRUE) %>%  # Keep only one row per species
      summarise(total_ED = ceiling(sum(ED, na.rm = TRUE))) # Sum ED values
    paste(total_ED)
    })
  
  # Map outputs (only for EDGE datasets)
  output$EDGE_map1 <- renderLeaflet({
    req(selected_data())
    req(dataset_type() == "edge")
    req(filtered_edge_data())
    #print(paste0("edge_gymno_subset = ", filtered_edge_data$powo_id))
    # prep data for map - maybe a helper function is called here
    edge_gymno_subset <- EDGEcountries %>%
      filter(powo_id %in% filtered_edge_data()$powo_id) # get the ranges
    #print(paste0("edge_gymno_subset = ", edge_gymno_subset))
    
    # 1. first poly layer for map - edge richness
    edge_gymno_richness <- edge_gymno_subset %>%
      group_by(area_code_l3) %>% 
      count() # get the richness
    edge_gymno_richness_sf <- rWCVPdata::wgsrpd3 %>% 
      left_join(edge_gymno_richness, by=c("LEVEL3_COD"="area_code_l3")) # add the sf geom
    # palette
    bins <- pretty(range(edge_gymno_richness_sf$n, na.rm = TRUE))
    pal_rich <- colorBin("YlOrRd", domain = edge_gymno_richness_sf$n, bins = bins, na.color = "lightgray")
        # labels
    labels_rich <- sprintf(
      "<strong>%s</strong><br/>%g species",
      edge_gymno_richness_sf$LEVEL3_NAM, edge_gymno_richness_sf$n
    ) %>% lapply(htmltools::HTML)
    
    # 2. second poly - threatened evo history
    edge_gymno_threat <- edge_gymno_subset %>%
      group_by(area_code_l3) %>% 
      summarise(n = sum(ED, na.rm = TRUE)) # Sum EDGE values
    print(edge_gymno_threat)
    
    edge_gymno_threat_sf <- rWCVPdata::wgsrpd3 %>% 
      left_join(edge_gymno_threat, by=c("LEVEL3_COD"="area_code_l3")) # add the sf geom
    # palette
    bins <- pretty(range(edge_gymno_threat_sf$n, na.rm = TRUE))
    pal_threat <- colorBin("Blues", domain = edge_gymno_threat_sf$n, bins = bins, na.color = "lightgray")
    # labels
    labels_threat <- sprintf(
      "<strong>%s</strong><br/>%g species",
      edge_gymno_threat_sf$LEVEL3_NAM, edge_gymno_threat_sf$n
    ) %>% lapply(htmltools::HTML)
    
    # map it
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 0,
              zoom = 2) %>%
      #addProviderTiles(providers$Stadia.StamenTonerLite, group = "stamen") %>%
      #addProviderTiles(providers$Esri.WorldImagery, group = "esri") %>%
      addPolygons(data = edge_gymno_threat_sf,
                  fillColor = ~pal_threat(n),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.8,
                  group = 'Threatened Evolutionary History',
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.8,
                    bringToFront = TRUE),
                  label = labels_threat,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addPolygons(data = edge_gymno_richness_sf,
                  fillColor = ~pal_rich(n),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.8,
                  group = 'Species Richness',
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels_rich,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addLegend(pal = pal_rich, values = edge_gymno_richness_sf$n, opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      addLegend(pal = pal_threat, values = edge_gymno_threat_sf$n, opacity = 0.7, title = NULL,
                position = "bottomleft") %>%
      addLayersControl(
        #baseGroups = c("stamen", "esri"),
        overlayGroups = c('Species Richness', 'Threatened Evolutionary History'),
        options = layersControlOptions(collapsed = FALSE)
        )
    
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
    #isolate({
      req(filtered_tipa_data(), dataset_type() == "tipas")
      datatable(
        filtered_tipa_data(),  # Use selected_data consistently
        filter = "none",
        extensions = 'Buttons',
        class = "compact stripe hover nowrap",
        options = list(
          searching = FALSE,
          pageLength = 5,
          scrollY = FALSE,
          scrollX = TRUE,
          dom = 'Bftip',
          lengthChange = FALSE,
          buttons = list(
            list(
              extend = 'csv',
              text = 'Download CSV',
              exportOptions = list(modifier = list(modifier = list(page = 'all')))
            )
          )
        )
      )
    #})
  }) 
  
  output$stat4 <- renderText({
    #isolate({
      req(filtered_tipa_data(), dataset_type() == "tipas")
      nrow(filtered_tipa_data())
    #})
  })
  
  output$stat5 <- renderText({
    #isolate({
    req(nrow(filtered_tipa_data()), dataset_type() == "tipas") 
    paste(ceiling(filtered_tipa_data() %>%
            select(Area) %>% sum()))
    #})
    #scales::unit_format(unit = "km")(stat5_value)  
    #HTML(paste0(scales::comma(stat5_value), " km", tags$sup("2")))
  })
  
  
  
  output$tipas_map <- renderLeaflet({
    #isolate({
      req(filtered_tipa_data(), dataset_type() == "tipas")
      filtered_shp <- tipas_shp[tipas_shp$TIPA_Name %in% filtered_tipa_data()$Name, ]
      leaflet() %>%
        addTiles() %>%
        addAwesomeMarkers(
          data = filtered_shp,
          lng = ~st_coordinates(st_centroid(geometry))[, 1],
          lat = ~st_coordinates(st_centroid(geometry))[, 2],
          label = ~TIPA_Name,
          clusterOptions = markerClusterOptions(),
          icon = awesomeIcons(
            icon = 'home',    
            markerColor = "darkgreen", 
            iconColor = 'white'
          )
        ) %>%
        addPolygons(
          data = filtered_shp,
          color = "red",
          weight = 2,
          fillColor = "red",
          fillOpacity = 0.75,
          label = filtered_shp$TIPA_Name
        )
    #})
  })
  

}

# Run the application
shinyApp(ui = ui, server = server)


