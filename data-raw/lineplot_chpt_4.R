## code to prepare `lineplot_chpt_4` dataset goes here

lineplot_chpt_4 <- plotsettings(
  axis_x_background_colour = "lightblue",
  axis_x_background_width = 0.04,
  axis_x_label_centralize = 0.05,
  axis_x_label_nudge_y = 0.02,
  axis_x_label_size = 2,
  brace_label_gap_y = 0.08,
  brace_label_nudge_x = 0.155,
  brace_label_nudge_y = 0.05,
  brace_label_size = 2,
  brace_line_width = 0.5,
  brace_span_y = 0.1,
  line_width = 0.7,
  margin_bottom = 0.02,
  margin_left = 0.01,
  margin_right = 0.01,
  margin_top = 0.02,
  n_cols = 4,
  point_label_size = 2,
  point_size = 1.7,
  split_plot = TRUE,
  y_axis = FALSE
)

usethis::use_data(lineplot_chpt_4, overwrite = TRUE)
