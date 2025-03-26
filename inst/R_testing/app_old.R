

# testing kew metrics dashboard app

# to do
# use kew font and add logo
# road map for new features, data - go back to old notes
# adjust the logic so that the dashboard template changes for each input dataset
## e.g. you may want different value boxes or a map vs a chart
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
gymnosperms_data <- read.csv(system.file("01_data/EDGE/EDGE_gymno.csv", package = "kew.metrics"))
angiosperms_data <- read.csv(system.file("01_data/EDGE/EDGE_angio.csv", package = "kew.metrics"))
SRLI_data <- read.csv(system.file("01_data/SRLI_2024.csv", package = "kew.metrics"))

# UI
ui <- page_sidebar(
  title = "Kew Metrics",

  sidebar = sidebar(bg = "white",
                    accordion(
                      accordion_panel(
                        "Gymnosperms and Angiosperms",
                        selectInput(
                          inputId = "EDGE_select",
                          label = "Select EDGE Dataset",
                          choices = list(
                            "Gymnosperms" = "gymnosperms",
                            "Angiosperms" = "angiosperms"
                          ),
                          selected = "gymnosperms"
                        )
                      ),
                      accordion_panel(
                        "SRLI Groups",
                        selectInput(
                          inputId = "RL_select",
                          label = "Select Red List Dataset",
                          choices = list(
                            "Legumes" = "legumes",
                            "Monocots" = "monocots"
                          ),
                          selected = "legumes"
                        )
                      ),
                      accordion_panel("Important Plant Areas")
                    ),
                    actionButton(inputId = "apply_dataset", label = "Apply Dataset")
                    ),

  theme = bs_theme(
    bootswatch = "zephyr",
    base_font = font_google("Inter"),
    navbar_bg = "#008285"
  ),

  layout_column_wrap(
    width = "250px",
    value_box(
      title = "Number of selected species",
      value = textOutput("stat1"),
      showcase = bs_icon("hash"),
      full_screen = TRUE,
      theme = "purple"
    ),
    value_box(
      title = "Stat 2",
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
    )
  ),

  card(full_screen = TRUE, card_header("Filtered Table"), DTOutput("filtered_table")),

  #card(card_header("Red List Table"),DT::DTOutput("filtered_redlist_table")),

  navset_card_underline(
   full_screen = TRUE,
   title = "EDGE Map",
   nav_panel("EDGE species richness", leafletOutput("EDGE_map1")),
   nav_panel("Threatened evolutionary history", leafletOutput("EDGE_map2"))
  )
)

# Server
server <- function(input, output, session) {

  # Reset inputs when switching between main and SRLI datasets
  observeEvent(input$EDGE_select, {
    updateSelectInput(session, "RL_select", selected = NULL)
  })

  observeEvent(input$RL_select, {
    updateSelectInput(session, "EDGE_select", selected = NULL)
  })

  # Reactive to select the active dataset
  selected_dataset <- eventReactive(input$apply_dataset, {
    if (!is.null(input$RL_select)) {
      if (input$RL_select == "Legumes") {
        dataset <- SRLI_data %>% filter(group == "Legumes")
        list(data = dataset, type = "RL")
      } else if (input$RL_select == "Monocots") {
        dataset <- SRLI_data %>% filter(group == "Monocots")
        list(data = dataset, type = "RL")
      } else {
        NULL
      }
    } else if (!is.null(input$EDGE_select)) {
      if (input$EDGE_select == "Gymnosperms") {
        list(data = gymnosperms_data, type = "EDGE")
      } else if (input$EDGE_select == "Angiosperms") {
        list(data = angiosperms_data, type = "EDGE")
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  observeEvent(input$apply_dataset, {
    if (!is.null(input$RL_select)) {
      updateSelectInput(session, "EDGE_select", selected = NULL)
    } else if (!is.null(input$EDGE_select)) {
      updateSelectInput(session, "RL_select", selected = NULL)
    }
  })

  # Value box statistics
  output$stat1 <- renderText({
    req(selected_dataset())
    nrow(selected_dataset()$data)
    #paste(nrow(selected_dataset()))
  })

  output$headline2 <- renderText({
    paste(filtered_EDGE_data() %>%
            filter(ED_rank == min(ED_rank)) %>%
            arrange(ED_rank) %>%  # Ensure ordering (even if there are ties)
            pull(Taxon))
  })

  output$stat2 <- renderText({
    req(selected_dataset())
    dataset <- selected_dataset()

    if (dataset$type == "EDGE") {
      dataset$data %>%
        arrange(ED_rank) %>%
        slice(1) %>%
        pull(Taxon)
    } else if (dataset$type == "RL") {
      paste("Most Common Status:", dataset$data %>%
              count(RL_2000) %>%
              arrange(desc(n)) %>%
              slice(1) %>%
              pull(RL_2000))
    }
  })


  # output$headline3 <- renderText({
  #   paste(filtered_EDGE_data() %>%
  #           filter(EDGE_rank == min(EDGE_rank)) %>%
  #           arrange(EDGE_rank) %>%  # Ensure ordering (even if there are ties)
  #           pull(Taxon))
  # })

  #Data table
  output$filtered_table <- renderDT({
    req(selected_dataset())
    datatable(selected_dataset()$data)
    #,
     #         filter = "top",
      #        options = list(dom = 't'),
       #       selection = "multiple"
        #      )
  })

  # output$filtered_table <- renderDT({
  #   req(selected_dataset())
  #   datatable(selected_dataset()$data)
  # })

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
  output$EDGEmap1 <- renderLeaflet({
    req(selected_EDGE_taxa())
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 0,
              zoom = 2)
  })

  output$EDGEmap2 <- renderLeaflet({
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
