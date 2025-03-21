# extras ----

# # Create reactive sparkline data
# tipas_spark_data <- tipas %>%
#   na.omit() %>%
#   arrange(year_identified) %>%
#   mutate(Cumulative_Area = cumsum(Area))
# 
# # Create the sparkline plot
# sparkline <- plot_ly(tipas_spark_data) %>%
#   add_lines(
#     x = ~year_identified, 
#     y = ~Cumulative_Area,
#     color = I("white"), 
#     span = I(1),
#     fill = 'tozeroy', 
#     alpha = 0.2
#   ) %>%
#   layout(
#     xaxis = list(visible = FALSE, showgrid = FALSE, title = ""),
#     yaxis = list(visible = FALSE, showgrid = FALSE, title = ""),
#     hovermode = "x",
#     margin = list(t = 0, r = 0, l = 0, b = 0),
#     font = list(color = "white"),
#     paper_bgcolor = "transparent",
#     plot_bgcolor = "transparent"
#   ) %>%
#   config(displayModeBar = FALSE)
# 

# observe({
#   print(paste("Base data type:", dataset_type()))
#   print("Base data preview:")
#   print(head(base_data()))
# })

# observe({
#   print(selected_data())  # Check if TIPAS is returning data
# })
# card(
#   height = "180px",
#   class = "mb-3",  # Add margin bottom for spacing
#   layout_column_wrap(
#     width = 1/3,  # This makes each value box take up 1/3 of the width
#     heights_equal = "row",
#     value_box(
#       title = "Number of selected species",
#       value = textOutput("stat1"),
#       #showcase = bs_icon("hash"),
#       theme = "purple",
#       full_screen = TRUE,
#       height = "100px"
#     ),
#     value_box(
#       title = "Highest ranking EDGE species",
#       value = textOutput("stat2"),
#       #showcase = bs_icon("tree"),
#       theme = "teal",
#       full_screen = TRUE,
#       height = "100px"
#     ),
#     value_box(
#       title = "Stat 3",
#       value = textOutput("stat_e1"),
#       #showcase = bs_icon("flower1"),
#       theme = "pink",
#       full_screen = TRUE,
#       height = "100px"
#     )
#   )
# ),
#card(

# # Conditional UI based on dataset selection
# output$Risk_conditional <- renderUI({
#   req(dataset_type())
#   
#   if (dataset_type() == "edge") {
#     # UI elements for EDGE datasets
#     page_fillable(
#       navset_card_tab(
#         sidebar = sidebar(
#           selectizeInput(
#             inputId = "edge_family_select", 
#             label = "Select Family",
#             choices = NULL,
#             multiple = TRUE
#           ),
#           selectizeInput(
#             inputId = "edge_genus_select", 
#             label = "Select Genus",
#             choices = NULL,
#             multiple = TRUE
#           ),
#           selectizeInput(
#             inputId = "edge_species_select", 
#             label = "Select Species",
#             choices = NULL,
#             multiple = TRUE
#           ),
#           input_task_button(
#             id = "apply_edge_filter", 
#             label = "Apply Filter"
#           ),
#         ),
# 
#         full_screen = TRUE,
#         title = "EDGE species",
#         nav_panel("Table", DTOutput("data_table_edge")),
#         nav_panel("Maps", leafletOutput("EDGE_map1"))
#         #,card_footer("link or reference here?")
#         
#       )#,
#       # navset_card_tab(
#       #   height = "600px",
#       #   full_screen = TRUE,
#       #   title = "EDGE Maps",
#       #   nav_panel("EDGE species richness", leafletOutput("EDGE_map1")),
#       #   ##nav_panel("Threatened evolutionary history",leafletOutput("EDGE_map2"))
#       # )
#     )
#   } else if (dataset_type() == "redlist") {
#     # UI elements for Red List datasets
#     layout_column_wrap(
#       width = "500px",
#       heights_equal = "row",
#       card(
#         height = "600px",
#         full_screen = TRUE,
#         card_header("Red List Species Table"),
#         DTOutput("data_table_redlist")
#       ),
#       card(
#         height = "600px",
#         full_screen = TRUE,
#         card_header("Red List Status Distribution"),
#         card_footer("link or reference here?"),
#         plotOutput("redlist_plot")
#       )
#     )
#   } 
# })

# Add new reactive for filtered data
# selected_data <- reactive({
#   req(base_data())
#   if (dataset_type() == "edge") {
#     if (!is.null(input$data_table_edge_rows_all)) {
#       base_data()[input$data_table_edge_rows_all, ]
#     } else {
#       base_data()
#     }
#   } else if (dataset_type() == "redlist") {
#     if (!is.null(input$data_table_redlist_rows_all)) {
#       base_data()[input$data_table_redlist_rows_all, ]
#     } else {
#       base_data()
#     }
#   } 
#   else if (dataset_type() == "tipas") {
#     if (!is.null(input$data_table_tipas_rows_all)) {
#       base_data()[input$data_table_tipas_rows_all, ]
#     } else {
#       base_data()
#     }
#   }
# })

#filtered_species <- base_data() %>%
#  filter(genus %in% input$edge_genus_select) %>%
#  pull(species) %>%
#  unique()