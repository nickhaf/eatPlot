## code to prepare `lineplot_4x4` dataset goes here

lineplot_4x4 <- plotsettings_lineplot(
  axis_x_background_colour = cmyk(30, 0, 15, 6),
  axis_x_background_width_x = 0.1,
  axis_x_background_width_y = 0.04,
  axis_x_label_centralize = 0.05,
  axis_x_label_nudge_y = 0.02,
  axis_x_label_size = 2,
  background_line_colour = cmyk(30, 0, 15, 6),
  brace_label_gap_y = 0.08,
  brace_label_nudge_x = 0.155,
  brace_label_nudge_y = 0.05,
  brace_label_size = 2,
  brace_line_width = 0.5,
  brace_span_y = 0.1,
  equal_trend_line_length = TRUE,
  grouping_colours = c(
    cmyk(0, 0, 0, 100),
    cmyk(0, 0, 0, 40)
  ),
  line_type = c(
    "TRUE" = "solid",
    "FALSE" = "dashed"
  ),
  line_width = 0.7,
  margin_bottom = 0.01,
  margin_left = 0.005,
  margin_right = 0.005,
  margin_top = 0.01,
  n_cols = 4,
  point_label_nudge = FALSE,
  point_label_nudge_direction = NULL,
  point_label_nudge_x = 0.02,
  point_label_nudge_y = 0.18,
  point_label_size = 2,
  point_shapes = c(
    "TRUE" = 17,
    "FALSE" = 16
  ),
  point_size = 1.7,
  split_plot = TRUE,
  split_plot_gap_width = 0.03,
  y_axis = FALSE
)

usethis::use_data(lineplot_4x4, overwrite = TRUE)



lineplot_4x4_3groups <- plotsettings_lineplot(
  grouping_colours = c(
    cmyk(0, 0, 0, 100),
    cmyk(0, 0, 0, 40),
    cmyk(0, 0, 0, 20)
  ),
  default_list = lineplot_4x4
)

usethis::use_data(lineplot_4x4_3groups, overwrite = TRUE)



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
