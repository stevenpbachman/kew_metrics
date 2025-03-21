
#' Main application user interface
#'
#' @return A [bslib::page_navbar()] object of the application user interface.
#' @import shiny
#' @import bslib
ui <- function() {
  page_navbar(
    title = "Kew Biodiversity Metrics",
    id = "main_nav",
    underline = TRUE,
    bg = "#008285",
    theme = bs_theme(
      bootswatch = "flatly",
      base_font = font_google("Inter")
    ),


    # Diversity metrics ----
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

    # Risk metrics ----
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
              "Red List Index",
              selectInput(
                inputId = "dataset2",
                label = "Select layer:",
                choices = list(
                  "None" = "",
                  "Global - Sampled" = "globalsampled",
                  "Goldilocks clade I" = "goldilocksI",
                  "Goldilocks clade II" = "goldilocksII"
                  #"Legumes" = "legumes", # change to "Global - Sampled"
                  #"Monocots" = "monocots" # change to "Global - Sampled"
                ),
                selected = ""
              )
            )
          )
        ),
        uiOutput("Risk_conditional")
      )
    ),

    # Response metrics ----
    # Third nav item -
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

    # GBF Indicators ----
    nav_panel(
      title = "GBF Indicators",
      page_sidebar(
        sidebar = sidebar(
          accordion(
            accordion_panel(
              "Filters",
              selectInput(
                inputId = "goal_filter",
                label = "Select Goal:",
                choices = c("All", unique(metrics_gbf$Goal)),
                selected = "All"
              ),
              selectInput(
                inputId = "target_filter",
                label = "Select Target:",
                choices = c("All", sort(unique(metrics_gbf$Target), na.last = TRUE)),
                selected = "All"
              ),
              selectInput(
                inputId = "group_filter",
                label = "Select Group:",
                choices = c("All", sort(unique(metrics_gbf$Group), decreasing = TRUE)),
                selected = "All"
              )
            )
          )
        ),
        # Main panel content
        DTOutput("gbf_metrics_table")
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
            card_header("What this website is all about...")
            #DTOutput("data_table_predictions")
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
}
