library(bsli)
library(ggplot2)
library(ggrepel)
# ui --------------------------------------------------------------------------
input_switch(
  id = "highlight_switch",
  label = "Show highlights",
  value = FALSE
)
# server ----------------------------------------------------------------------
out <- out +
  geom_point(
    data = filtered_data() |>
      filter(name %in% c("Horse", "Human", "Little brown bat")),
  ) +
  geom_label_repel(
    label = c(
      "An average horse sleeps\nonly 2.7 hours a day!",
      "Humans sleep 8 hours a day and\nhave a sleep cycle of 1.5 hours on average.\nHope you all have 8 hours of daily sleep.", 
      "You are more likely to find\na little brown bat in their sleep\nas they sleep 19.9 hours a day."
    ),
    size = 4,
    data = filtered_data() |>
      filter(name %in% c("Horse", "Human", "Little brown bat")),
    nudge_x = c(-1.5, -2, 1),
    nudge_y = c(-.2, -1, -.5),
    show.legend = FALSE
  )
