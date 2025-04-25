## code to prepare `barplot_table_plot_pattern` dataset goes here
barplot_table_plot_pattern <- plotsettings_tablebarplot(
  axis_x_label_size = 5,
  bar_background_lines_colour = cmyk(0, 0, 0, 60),
  bar_background_lines = "scale_breaks",
  bar_background_lines_linetype = "dashed",
  bar_fill_colour = c(
    cmyk(85, 0, 43, 17),
    cmyk(40, 0, 20, 8),
    cmyk(20, 0, 10, 4)
  ),
  bar_frame_linetype = c(`TRUE` = "solid", `FALSE` = "dashed"),
  bar_line_width = 0.1,
  bar_pattern_fill_colour = "white",
  bar_pattern_type = c("FALSE" = "stripe", "TRUE" = "none"),
  bar_type = "pattern",
  bar_width = 0.6,
  columns_table_sig_superscript_letter = "b",
  columns_table_sig_superscript_letter_nudge_x = 3,
  headers_background_colour = cmyk(40, 0, 20, 8),
  bar_pattern_spacing = 0.015,
  bar_pattern_width = 0.4,
  font_size = 1.55,
  headers_font_size = 1.55,
)

usethis::use_data(barplot_table_plot_pattern, overwrite = TRUE)


abb_6.5 <- plotsettings_tablebarplot(
  axis_x_lims = c(0, 70),
  background_stripes_colour = c(rep(c("white", "white", "white", rep(cmyk(7, 0, 4, 1), 3)), 8), rep(cmyk(0, 0, 0, 10), 3)),
  bar_nudge_y = rep(c(-0.1, 0.1), 34 / 2),
  columns_alignment = c(0, rep(2, 6)),
  columns_nudge_y = c(-0.5, rep(-0.5, 3), rep(0.5, 3)),
  columns_width = c(0.17, rep(0.075, 6), 0.38),
  font_size = 2.3,
  headers_alignment = c(0, rep(0.5, 7)),
  headers_font_size = 2.3,
  headers_nudge_x = c(0, rep(0.5, 7)),
  headers_nudge_y = c(rep(0, 7), 0.5),
  headers_row_height = 1.75,
  column_spanners_row_height = 1.5,
  background_stripes_colour = c(rep(c("white", "white", cmyk(7,0,4,1), cmyk(7,0,4,1)), 8), "grey", "grey"),
  bar_fill_colour = c("#20D479", "#8DEBBC"),
  default_list = barplot_table_plot_pattern
)

usethis::use_data(abb_6.5, overwrite = TRUE)


abb_6.6 <- plotsettings_tablebarplot(
  axis_x_lims = c(-70, 72),
  bar_nudge_y = rep(c(-0.1, 0.1), 34 / 2),
  background_stripes_colour = c(rep(c("white", "white", cmyk(7,0,4,1), cmyk(7,0,4,1)), 8), "grey", "grey"),
  bar_fill_colour = c("#20D479", "#8DEBBC"),
  columns_table_sig_superscript_letter = "a",
  columns_table_sig_superscript_letter_nudge_x = 5.5,
  headers_row_height = 2,
  font_size = 2,
  headers_font_size = 2,
  default_list = barplot_table_plot_pattern
)
usethis::use_data(abb_6.6, overwrite = TRUE)


abb_8.4 <- plotsettings_tablebarplot(
  axis_x_lims = c(-20, 40),
  background_stripes_colour = c(rep(c("white", cmyk(7,0,4,1)), 8), "grey"),
  column_spanners_2_row_height = 1.5,
  headers_row_height = 1.5,
  headers_ggtext = FALSE,
  font_size = 2.2,
  headers_font_size = 2.2,
  default_list = barplot_table_plot_pattern
)

usethis::use_data(abb_8.4, overwrite = TRUE)
