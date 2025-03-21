

# testing kew metrics dashboard app

# to do
# use kew font and add logo
# road map for new features, data - go back to old notes
# how to create a new page every time a new dataset is selected?
# underlying data to explain each dashboard page (ai assist?)
# move to github and update
# add a map - should it react to the selection? yes I think. 
# Add the distributions first, then join and then map the selection
# map could have two tabs -
#  1) count of EDGE species per TDWG
#  2) count of EDGE species
# how heavy is the TDWG layer to display - maybe not bad if you only show selected regions?
# add download option to get selected or full datatable? link to DOI and original publication
# go back to bslib video and add more functionality e.g. action button? https://www.youtube.com/watch?v=vzXTFbnKAqc
# color:#008285 (kew colours for apps)
# filter doesn't work that well for things like taxus that could be Amentotaxus


library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(leaflet)
library(ggplot2)
library(dplyr)

# Raw data
gymnosperms_data <- read.csv("01_data/EDGE_gymno.csv")
angiosperms_data <- read.csv("01_data/EDGE_angio.csv")
SRLI_data <- read.csv("01_data/SRLI_2024.csv")

# UI
ui <- page_sidebar(
  title = "Kew Metrics",

  # sidebar = sidebar(
  #   selectInput(
  #     inputId = "dataset",
  #     label = "Select Dataset",
  #     choices = c("Gymnosperms", "Angiosperms"),
  #     selected = "Gymnosperms"
  #   )
  # ),
  
  sidebar = sidebar(bg = "white",
                    accordion(
                      accordion_panel(
                        "EDGE",
                        selectInput(
                          inputId = "dataset",
                          label = "Select Dataset",
                          choices = c("Gymnosperms", "Angiosperms"),
                          selected = "Gymnosperms"
                        )
                      ),
                      
                      # accordion_panel(
                      #   "EDGE",
                      #   selectInput(
                      #     inputId = "main_dataset_selector",
                      #     label = "Select Main Dataset",
                      #     choices = list(
                      #       "Gymnosperms" = "gymnosperms",
                      #       "Angiosperms" = "angiosperms"
                      #     ),
                      #     selected = "gymnosperms"
                      #   )
                      # ),
                      # 
                      # accordion_panel(
                      #   "SRLI Groups",
                      #   selectInput(
                      #     inputId = "srl_dataset_selector",
                      #     label = "Select SRLI Group",
                      #     choices = list(
                      #       "Legumes" = "legumes",
                      #       "Monocots" = "monocots"
                      #     ),
                      #     selected = "legumes"
                      #   )
                      # ),
                      
                      accordion_panel(
                        "Red List",
                        selectInput(
                          inputId = "redlist_group",
                          label = "Select Dataset",
                          choices = unique(SRLI_data$group),
                          selected = unique(SRLI_data$group)[1]  # Default to the first group
                        )
                      ),
                      accordion_panel("Important Plant Areas")
                    )), 
  
  theme = bs_theme(
    bootswatch = "zephyr",
    base_font = font_google("Inter"),
    navbar_bg = "#008285"
  ),
  
  layout_column_wrap(
    width = "250px",
    value_box(
      title = "Number of selected species",
      value = textOutput("headline1"),
      showcase = bs_icon("hash"),
      full_screen = TRUE,
      theme = "purple"
    ),
    value_box(
      title = "Highest Ranked ED species:",
      value = textOutput("headline2"),
      showcase = bs_icon("tree"),
      full_screen = TRUE,
      theme = "teal"
    ),
    value_box(
      title = "Highest Ranked EDGE species: ",
      value = textOutput("headline3"),
      showcase = bs_icon("flower1"),
      full_screen = TRUE,
      theme = "pink"
    )
  ),
  
  card(full_screen = TRUE, card_header("EDGE_Table"), DTOutput("filtered_EDGE_table")),
  
  #card(card_header("Red List Table"),DT::DTOutput("filtered_redlist_table")),
  
  navset_card_underline(
   full_screen = TRUE,
   title = "Map",
   nav_panel("EDGE species richness", leafletOutput("map1")),
   nav_panel("Threatened evolutionary history", leafletOutput("map2"))
  )
)

# Server
server <- function(input, output, session) {
  
  # # Reactive to select the active dataset
  # selected_dataset <- reactive({
  #   if (input$main_dataset_selector == "gymnosperms") {
  #     gymnosperms_data
  #   } else if (input$main_dataset_selector == "angiosperms") {
  #     angiosperms_data
  #   } else if (input$srl_dataset_selector == "legumes") {
  #     SRLI_data %>% filter(group == "Legumes")
  #   } else if (input$srl_dataset_selector == "monocots") {
  #     SRLI_data %>% filter(group == "Monocots")
  #   } else {
  #     NULL
  #   }
  # })

  
  
  # Reactive value to determine the selected dataset
  selected_EDGE_taxa <- reactive({
    if (input$dataset == "Gymnosperms") {
      gymnosperms_data
    } else if (input$dataset == "Angiosperms") {
      angiosperms_data
    } else {
      NULL
    }
  })
  # 
  # # Reactive dataset for the selected group in Red List
  # selected_redlist_group <- reactive({
  #   req(input$redlist_group)  # Ensure a group is selected
  #   SRLI_data %>%
  #     filter(group == input$redlist_group)  # Filter by selected group
  # })
  # 
  # Reactive value to capture the filtered rows
  filtered_EDGE_data <- reactive({
    req(selected_EDGE_taxa())
    data <- selected_EDGE_taxa()
    # Use input from DT search filter to subset the data
    if (!is.null(input$filtered_table_rows_all)) {
      data <- data[input$filtered_table_rows_all, ]
    }
    data
  })
  
  # Headline statistics
  output$headline1 <- renderText({
    paste(nrow(filtered_EDGE_data()))
  })
  
  output$headline2 <- renderText({
    paste(filtered_EDGE_data() %>%
            filter(ED_rank == min(ED_rank)) %>%
            arrange(ED_rank) %>%  # Ensure ordering (even if there are ties)
            pull(Taxon))
  })
  
  output$headline3 <- renderText({
    paste(filtered_EDGE_data() %>%
            filter(EDGE_rank == min(EDGE_rank)) %>%
            arrange(EDGE_rank) %>%  # Ensure ordering (even if there are ties)
            pull(Taxon))
  })
  
  #Data table
  output$filtered_EDGE_table <- renderDT({
    req(selected_EDGE_taxa())
    datatable(selected_EDGE_taxa(),
              filter = "top",
              options = list(dom = 't'),
              selection = "multiple" 
              )
  })
  
  # Display the filtered table
  # output$filtered_redlist_table <- DT::renderDT({
  #   req(selected_redlist_group())  # Ensure the reactive dataset is ready
  #   DT::datatable(selected_redlist_group())
  # })

  # Bar chart
  # output$chart <- renderPlot({
  #   req(selected_taxa())
  #   ggplot(selected_taxa(), aes(x = reorder(taxon, edge_rank), y = edge_rank)) +
  #     geom_bar(stat = "identity", fill = "steelblue") +
  #     labs(title = "EDGE Rankings", x = "Taxon", y = "EDGE Rank") +
  #     theme_minimal()
  # })
  
  #Interactive map
  output$map1 <- renderLeaflet({
    req(selected_EDGE_taxa())
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 0,
              zoom = 2)
  })
  
  output$map2 <- renderLeaflet({
    req(selected_EDGE_taxa())
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 0,
              zoom = 5)
  })
}  


# Run the app
shinyApp(ui, server)