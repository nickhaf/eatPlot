lineplot_2x3 <- plotsettings_lineplot(
  axis_x_background_width_x = 0.075,
  brace_label_nudge_x = 0.11,
  n_cols = 3,
  margin_bottom = 0.085,
  point_label_nudge = FALSE,
  point_label_nudge_x = 0.02,
  split_plot_gap_width = 0.02,
  default_list = lineplot_4x4_3groups
)

usethis::use_data(lineplot_2x3, overwrite = TRUE)
