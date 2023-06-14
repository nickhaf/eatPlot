## code to prepare `barplot_table_plot_pattern` dataset goes here
barplot_table_plot_pattern <- plotsettings_tablebarplot(
  background_stripes_colour = c(rep(c("white", "white", "white", rep(cmyk(7, 0, 4, 1), 3)), 8), rep(cmyk(0, 0, 0, 10), 3)),
  bar_background_lines = "border",
  bar_background_lines_linetype = "dotted",
  bar_fill_colour = c(
   cmyk(85, 0, 43, 17),
   cmyk(40, 0, 20, 8),
   cmyk(20, 0, 10, 4)
  ),
  bar_frame_linetype = c(`TRUE` = "solid", `FALSE` = "dashed"),
  bar_line_width = 0.1,
  bar_pattern_fill_colour = "white",
  bar_pattern_type = c("FALSE" = "stripe", "TRUE" = "none"),
  bar_sig_type = "pattern",
  bar_width = 0.7,
  columns_table_sig_high_letter = "b",
  columns_table_sig_high_letter_nudge_x = 4.7,
  headers_background_colour = cmyk(40, 0, 20, 8),
  font_size = 2,
  bar_pattern_spacing = 0.01,
  bar_pattern_width = 0.4
)

usethis::use_data(barplot_table_plot_pattern, overwrite = TRUE)


barplot_plot_frame <- plotsettings_tablebarplot(
  background_stripes_colour = c(rep(c("white"), 16), cmyk(0, 0, 0, 15)),
  bar_background_lines = "none",
  bar_fill_colour = cmyk(40, 0, 20, 8),
  bar_frame_linetype = c("above" = "solid", "below" = "longdash", "no_sig" = "blank"),
  bar_label_nudge_x = -0.2,
  bar_label_size = 1.75,
  bar_line_width = 0.25,
  bar_pattern_fill_colour = c("yellow"),
  bar_sig_type = "frame",
  bar_width = 0.4,
  font_size = 2,
  headers_background_width_y = 1,
  default_list = barplot_table_plot_pattern
)



usethis::use_data(barplot_plot_frame, overwrite = TRUE)


barplot_plot_frz <- plotsettings_tablebarplot(
  headers_background_colour = cmyk(40, 0, 20, 8),
  background_stripes_colour = c(rep("white", 3), rep(cmyk(7, 0, 4, 1), 3)),
  bar_background_lines = "scale_breaks",
  bar_background_lines_spanners = list(c(1, 3), c(4, 6)),
  bar_line_width = 0.1,
  bar_width = 0.5,
  default_list = barplot_table_plot_pattern
)

usethis::use_data(barplot_plot_frz, overwrite = TRUE)
