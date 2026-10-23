# ui --------------------------------------------------------------------------
input_switch(
  id = "highlight_switch",
  label = "Show highlights",
  value = FALSE
)
# server ----------------------------------------------------------------------
highlight_data <- filtered_data() |>
  inner_join(
    tibble(
      name = c("Horse", "Human", "Little brown bat"),
      label = c(
        "An average horse sleeps\nonly 2.7 hours a day!",
        "Humans sleep 8 hours a day and\nhave a sleep cycle of 1.5 hours on average.\nHope you all have 8 hours of daily sleep.", 
        "You are more likely to find\na little brown bat in their sleep\nas they sleep 19.9 hours a day."
      ),
      nudge_x = c(-1.5, -2, 1),
      nudge_y = c(-.2, -1, -.5)
    )
  )
out <- out +
  geom_point(
    data = highlight_data,
  ) +
  geom_label_repel(
    aes(label = label),
    size = 4,
    data = highlight_data,
    nudge_x = highlight_data$nudge_x,
    nudge_y = highlight_data$nudge_y,
    show.legend = FALSE
  )
