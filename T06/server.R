#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# deploy:
# rsconnect::deployApp(appDir = ".", appTitle = "STA313H1 SHINY TUTORIAL EXAMPLE")

library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(ggtext)
library(ggrepel)
# prepare LABELS --------------------------------------------------------------
PLOT_TITLE <- "Larger mammals, especially herbivores, tend to sleep less, 
but have longer sleep cycles."
X_AXIS_TITLE <- "Body weight (kg)"
Y_AXIS_TITLE <- "Ratio of sleep time over awake time"
SIZE_LABEL <- expression("Sleep cycle (hr)"^1)
COLOUR_LABEL <- "Feeding habit"
VORE_VALUES <- c("carni", "herbi", "insecti", "omni", "missing")
VORE_LABELS <- c("Carnivore", "Herbivore", "Insectivore", "Omnivore", "Not available")
names(VORE_LABELS) <- VORE_VALUES
# prepare msleep data from ggplot2 --------------------------------------------
msleep <- msleep |>
  mutate(
    vore = factor(
      if_else(is.na(vore), "missing", vore), 
      levels = VORE_VALUES
    )
  )

# prepare BASEPLOT ------------------------------------------------------------
CMAP <- c(RColorBrewer::brewer.pal(4, "Dark2"), "#7f7f7f")
names(CMAP) <- VORE_VALUES
BASETHEME <- theme_minimal() +
  theme(
    legend.text = element_markdown(size = 10),
    legend.title = element_text(size = 10, hjust = 0),
    legend.title.position = "left",
    legend.position = "top",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.justification = c(0, 1),
    legend.margin = margin(),
    legend.key.height = unit(0, "pt"),
    axis.title = element_text(hjust = 0, size = 10),
    title = element_text(size = 12),
    plot.caption = element_text(size = 8, colour = "grey50")
  )
BASEPLOT <- msleep |>
  ggplot(aes(bodywt, sleep_total / awake, colour = vore, size = sleep_cycle)) +
  BASETHEME +
  scale_x_continuous(
    transform = "log", 
    breaks = c(1, 10, 100, 1000),
    labels = scales::label_number(big.mark = ",")
  ) +
  scale_y_continuous(
    transform = "log",
    breaks = c(0.1, 0.5, 1, 2.5, 5)
  ) +
  scale_size_continuous(
    breaks = c(0.5, 1, 1.5, 0.5),
    labels = c("0.5", "1", "1.5", "Not available"),
    limits = c(0, 1.5),
    guide = guide_legend(
      order = 1,
      label.position = "right",
      override.aes = list(shape = c(19, 19, 19, 1), alpha = 1)
    )
  ) +
  labs(
    title = PLOT_TITLE,
    x = X_AXIS_TITLE,
    y = Y_AXIS_TITLE,
    size = SIZE_LABEL, 
    colour = COLOUR_LABEL
  )
# Define server logic ---------------------------------------------------------
function(input, output, session) {
  # keep track of feeding habit filter selections
  vore_selected <- reactiveVal(VORE_VALUES)
  # Interaction with Shiny only -----------------------------------------------
  # update data based on the filters
  filtered_data <- reactive({
    msleep |>
      filter(
        # apply the feeding habit filter
        vore %in% vore_selected(),
        !is.na(sleep_cycle),
        # apply the sleep cycle slider filter
        sleep_cycle >= input$sleep_cycle_filter[1], 
        sleep_cycle <= input$sleep_cycle_filter[2]
      )
  })
  filtered_missing_data <- reactive({
    msleep |>
      filter(
        # apply the feeding habit filter
        vore %in% vore_selected(),
        is.na(sleep_cycle)
      )
  })
  
  output$plot <- renderPlot({
    out <- BASEPLOT + 
      geom_point(
        alpha = 0.5,
        data = filtered_data()
      ) +
      scale_colour_manual(values = CMAP, guide = NULL)
    # add the points with missing sleep cycle if the switch is on 
    if (input$na_sleep_switch) {
      out <- out +
        geom_point(
          size = 3, shape = 1, alpha = 0.5, show.legend = FALSE,
          data = filtered_missing_data()
        )
    }
    out
  }) |>
    bindCache(
      # cache the drawn plots for quick redraws (credit: Claude)
      vore_selected(),
      input$sleep_cycle_filter,
      input$na_sleep_switch,
      input$highlight_switch
    )
  # Interaction with ggiraph & Shiny ------------------------------------------
  # sync Shiny radio buttons and ggiraph legend buttons
  observeEvent(input$ggiraph_plot_key_selected, {
    # ggiraph plot object name + _key_selected
    s <- input$ggiraph_plot_key_selected
    if (is.null(s)) {
      s <- character(0)
    } 
    if (!setequal(s, vore_selected())) {
      updateCheckboxGroupInput(session, "feeding_filter", selected = s)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent(input$feeding_filter, {
    vore_selected(input$feeding_filter)
  }, ignoreNULL = FALSE)
  # update selection based on the filters
  # draw all points and hide filtered out points using ggiraph's opacity change
  id_selected <- reactive({
    ids <- filtered_data() |>
      pull(name)
    if (input$na_sleep_switch) {
      ids <- c(
        ids,
        filtered_missing_data() |> pull(name)
      )
    }
    if (length(ids) == 0) ids <- "__none__"
    ids
  })
  observe(
    session$sendCustomMessage(
      "ggiraph_plot_key_set", 
      as.character(vore_selected())
    )
  )
  observe(
    session$sendCustomMessage(
      "ggiraph_plot_set", 
      as.character(id_selected())
    )
  )
  observeEvent(
    input$ggiraph_plot_selected, 
    session$sendCustomMessage("ggiraph_plot_set", id_selected())
  )
  
  output$ggiraph_plot <- renderGirafe({
    ggiraph_plot <- BASEPLOT +
      geom_point_interactive(
        aes(
          data_id = name,
          tooltip = paste0(name, "\n Sleep cycle: ", round(sleep_cycle, 3), " hr")
        ),
        alpha = 0.5, show.legend = TRUE,
        data = msleep |>
          filter(!is.na(sleep_cycle))
      ) +
      geom_point_interactive(
        aes(
          data_id = name,
          tooltip = name
        ),
        size = 3, shape = 1, alpha = 0.5, show.legend = FALSE,
        data = msleep |>
          filter(is.na(sleep_cycle))
      ) +
      scale_colour_manual_interactive(
        # make the legend keys interactive
        data_id = function(breaks) { as.character(breaks) },
        values = CMAP,
        breaks = VORE_VALUES,
        drop = FALSE,
        labels = paste0("<span style='color:", CMAP, ";'>", VORE_LABELS, "</span>"),
        guide = guide_legend(
          order = 2,
          byrow = TRUE,
          override.aes = list(size = 5, alpha = 0.2)
        )
      )
    girafe(
      ggobj = ggiraph_plot,
      width = 8,
      height = 6,
      options = list(
        opts_selection_key(
          css = "fill-opacity: 1; stroke-opacity: 1; stroke-width: 2; stroke: black;",
          type = "multiple",
          selected = isolate(vore_selected()) # prevent re-initializing 
        ),
        opts_selection(
          css = "fill-opacity: 0.5; stroke-opacity: 1;",
          type = "multiple",
          selected = isolate(id_selected()) # prevent re-initializing 
        ),
        opts_selection_inv(
          css = "fill-opacity: 0; stroke-opacity: 0; pointer-events: none !important;"
        ),
        opts_hover_key(
          css = "fill-opacity: 1; stroke-opacity: 1; stroke-width: 1; stroke: black;",
          reactive = TRUE # send signal to Shiny
        ),
        opts_hover(
          css = "fill-opacity: 1; stroke-opacity: 1; stroke-width: 1",
          reactive = TRUE # send signal to Shiny
        ),
        opts_zoom(min = 1, max = 4),
        opts_toolbar(hidden = c("selection", "misc"))
      )
    )
  })
  # render ggiraph even when on the other tab
  outputOptions(output, "ggiraph_plot", suspendWhenHidden = FALSE)
  # define reactive items to interact with ggiraph plots
  ggiraph_hover <- reactive({
    # ggiraph plot object name + _hovered
    input$ggiraph_plot_hovered
  })
  
  output$console <- renderPrint(ggiraph_hover())
}
