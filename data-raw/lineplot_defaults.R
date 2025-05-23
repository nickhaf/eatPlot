## code to prepare `lineplot_4x4` dataset goes here

lineplot_4x4 <- plotsettings_lineplot(
  axis_x_background_colour = cmyk(40, 0, 20, 8),
  axis_x_background_width_x = 0.1,
  axis_x_background_width_y = 0.13,
  axis_x_label_centralize = 0.05,
  axis_x_label_size = 2,
  background_line_colour = cmyk(7,0,4,1), #cmyk(20, 0, 10, 4), #cmyk(30, 0, 15, 6),
  brace_label_gap_y = 0.08,
  brace_label_nudge_x = 0.3225,
  brace_label_nudge_y = 0.05,
  brace_label_size = 2,
  brace_line_width = 0.5,
  brace_span_y = 0.1,
  equal_trend_line_length = TRUE,
  subgroup_colours = c(
    cmyk(0, 0, 0, 100),
    cmyk(0, 0, 0, 60),
    cmyk(0, 0, 0, 40)
  ),
  line_type = c(
    "TRUE" = "solid",
    "FALSE" = "dashed"
  ),
  line_width = 0.7,
  margin_bottom = 0,
  margin_left = 0.004, ## has to be slightly smaller, because somehow the plot margin goes wider here.
  margin_right = 0.01,
  margin_top = -5,
  n_cols = 4,
  point_label_nudge = FALSE,
  point_label_nudge_direction = NULL,
  point_label_nudge_x = 0.02,
  point_label_nudge_y = 0.09,
  point_label_size = 2,
  point_shapes = c(
    "TRUE" = 15, ## Square
    "FALSE" = 16 ## Circle
  ),
  point_size = 1.7,
  split_plot = FALSE,
  split_plot_gap_width = 0.03,
  axis_y = TRUE
)

usethis::use_data(lineplot_4x4, overwrite = TRUE)

lineplot_germany <- plotsettings_lineplot(
  point_shapes = c(
    "TRUE" = 17, ## Triangle
    "FALSE" = 16 ## Circle
  ),
  default_list = lineplot_4x4
)
usethis::use_data(lineplot_germany, overwrite = TRUE)


