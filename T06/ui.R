#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(ggiraph)
# prepare LABELS --------------------------------------------------------------
SIZELABEL <- HTML("Sleep cycle (hr)<sup>1</sup>")
COLOURLABEL <- "Feeding habit"
MISSINGLABEL <- "Show missing sleep cycle"
HIGHLIGHTLABEL <- "Show highlights"
SLEEPCYCLE <- HTML(
  "<div class='right'><sup>1</sup>Sleep cycle is the length of one REM cycle and one non-REM cycle.</div>"
)
# define UI -------------------------------------------------------------------
page_fluid(
  tags$head(
    # add CSS in HTML head
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(
    "
document.addEventListener('click', function (e) {
  // ignore clicks on ggiraph data points; legend keys (key-id) still work
  if (e.target.closest('#ggiraph_plot svg [data-id]')) e.stopPropagation();
}, true);
"
    )
  ),
  card( # card container; see ?bslib::card
    card_title(h2("Who sleeps more?")), 
    navset_card_underline( # navigation container; see ?bslib::navset
      sidebar = sidebar(
        # filter for feeding habit
        checkboxGroupInput( # grouped checkbox selector; see ?shiny::checkboxGroupInput
          inputId = "feeding_filter",
          label = COLOURLABEL, 
          choiceValues = list("carni", "herbi", "insecti", "omni", "missing"),
          choiceNames = list(
            span("Carnivore", class = "carni"),
            span("Herbivore", class = "herbi"),
            span("Insectivore", class = "insecti"),
            span("Omnivore", class = "omni"),
            span("Not available", class = "missing")
          ),
          selected = list("carni", "herbi", "insecti", "omni", "missing")
        ),
        # filter for sleep cycle
        sliderInput( # slider UI; see ?shiny::sliderInput
          inputId = "sleep_cycle_filter",
          label = SIZELABEL,
          min = 0,
          max = 1.5,
          step = 0.05,
          ticks = FALSE,
          value = c(0, 1.5)
        ),
        input_switch( # switch UI; see ?bslib::input_switch
          id = "na_sleep_switch", 
          label = MISSINGLABEL,
          value = TRUE
        )
      ),
      nav_panel( # creates a tab inside navset_*(); see ?bslib::nav-items
        "Interaction with Shiny only",
        plotOutput("plot")
      ),
      nav_panel(
        "Interactions with ggiraph & Shiny",
        girafeOutput("ggiraph_plot")
      )
    ),
    card_footer(SLEEPCYCLE)
  ),
  verbatimTextOutput("console")
)
