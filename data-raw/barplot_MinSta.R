## code to prepare `barplot_MinSta_trend` dataset goes here
barplot_MinSta_trend <- plotsettings_tablebarplot(
  background_stripes_colour = c(rep(c("white", "white", "white", "lightgrey", "lightgrey", "lightgrey"), 8), rep("darkgrey", 3)),
  bar_fill_colour = c(grDevices::rgb(33, 89, 104, maxColorValue = 255),
                      grDevices::rgb(49, 133, 156, maxColorValue = 255),
                      grDevices::rgb(75, 172, 198, maxColorValue = 255)),
  bar_frame_linetype = c(`TRUE` = "solid", `FALSE` = "dashed"),
  bar_pattern_fill_colour = "white",
  bar_pattern_type = c("TRUE" = "stripe", "FALSE" = "none"),
  bar_sig_type = "pattern",
  bar_width = 0.7,
  headers_nudge_y = 0.3,
  columns_width = c(0.1, 0.15, 0.1, 0.1)
)

usethis::use_data(barplot_MinSta_trend, overwrite = TRUE)


barplot_MinSta <- plotsettings_tablebarplot(
  axis_x_background_width_x = 0.05,
  axis_x_lims = c(0, 35),
  background_stripes_colour = c(rep(c("white", "gray94"), 8), "gray87"),
  bar_fill_colour = c("lightblue"),
  bar_frame_linetype = c("above" = "solid", "below" = "longdash", "no_sig" = "blank"),
  bar_label_nudge_x = -0.2,
  bar_label_size = 1.75,
  bar_line_size = 0.35,
  bar_pattern_fill_colour = c("yellow"),
  bar_sig_type = "frame",
  bar_width = 0.4,
  columns_width = c(0.13),
  font_size = 2,
  default_list = barplot_MinSta_trend
)

usethis::use_data(barplot_MinSta, overwrite = TRUE)
