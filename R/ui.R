
#' Main application user interface
#'
#' @return A [bslib::page_navbar()] object of the application user interface.
#' @import shiny
#' @import bslib
ui <- function() {
  page_navbar(
    title = "Kew Biodiversity Metrics",
    id = "main_nav",
    navbar_options = navbar_options(underline = TRUE, bg = "#008285"),
    lang = "en",
    theme = bs_theme(
      bootswatch = "flatly",
      base_font = font_google("Inter")
    ),

    # Add JavaScript here, before other UI elements
    header = tags$head(
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
    risk_ui(id = "risk"),

    # Response metrics ----
    # Third nav item -
    conservation_ui(id = "conservation"),

    # GBF Indicators ----
    gbf_indicators_ui(id = "gbf_indicators"),

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
          src = "logo/kew_logo_2015_small_w.png",
          height = "30px",
          style = "margin: 0 15px;"
        )
      )
    )
  )
}
