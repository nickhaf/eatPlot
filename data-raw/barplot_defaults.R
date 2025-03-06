## code to prepare `barplot_table_plot_pattern` dataset goes here
barplot_table_plot_pattern <- plotsettings_tablebarplot(
  axis_x_label_size = 6,
  background_stripes_colour = c(rep(c("white", "white", "white", rep(cmyk(7, 0, 4, 1), 3)), 8), rep(cmyk(0, 0, 0, 10), 3)),
  bar_background_lines = "scale_breaks",
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
  bar_width = 0.6,
  columns_table_sig_superscript_letter = "b",
  columns_table_sig_superscript_letter_nudge_x = 3,
  headers_background_colour = cmyk(40, 0, 20, 8),
  bar_pattern_spacing = 0.015,
  bar_pattern_width = 0.4
)

usethis::use_data(barplot_table_plot_pattern, overwrite = TRUE)


abb_6.5 <- plotsettings_tablebarplot(
  axis_x_lims = c(-5, 40),
  bar_nudge_y = rep(c(-0.1, 0.1), 34 / 2),
  columns_alignment = c(0, rep(2, 6)),
  headers_alignment = c(0, rep(0.5, 6), 0.5),
  headers_nudge_x = c(0, rep(1.5, 6), 0),
  headers_nudge_y = c(rep(0, 7), 0.5),
  headers_row_height = 2,
  column_spanners_row_height = 1.5,
  columns_nudge_y = c(rep(-0.5, 4), rep(0.5, 3)),
  columns_width = c(0.1, rep(0.075, 6), 0.45),
  background_stripes_colour = c(rep(c("white", "white", "#EBFDF3", "#EBFDF3"), 8), "grey", "grey"),
  bar_fill_colour = c("#20D479", "#8DEBBC"),
  default_list = barplot_table_plot_pattern
)

usethis::use_data(abb_6.5, overwrite = TRUE)


abb_6.6 <- plotsettings_tablebarplot(
  bar_nudge_y = rep(c(-0.125, 0.125), 34 / 2),
  background_stripes_colour = c(rep(c("white", "white", "#EBFDF3", "#EBFDF3"), 8), "grey", "grey"),
  bar_fill_colour = c("#20D479", "#8DEBBC"),
  default_list = barplot_table_plot_pattern
)
usethis::use_data(abb_6.6, overwrite = TRUE)


abb_8.4 <- plotsettings_tablebarplot(
  background_stripes_colour = c(rep(c("white", "#EBFDF3"), 8), "grey"),
  column_spanners_2_row_height = 2,
  headers_row_height = 2,
  headers_ggtext = FALSE,
  default_list = barplot_table_plot_pattern
)

usethis::use_data(abb_8.4, overwrite = TRUE)


barplot_noTrend <- plotsettings_tablebarplot(
  background_stripes_colour = c(rep(c("white", cmyk(7, 0, 4, 1)), 8), cmyk(0, 0, 0, 10)),
  bar_background_lines = "scale_breaks", # set background lines at scalebreaks
  bar_background_lines_linetype = "solid",
  bar_fill_colour = cmyk(60, 0, 30, 12),
  columns_alignment = c(0, 0.5, 0.5, 0.5, 0.5),
  columns_width = c(0.2, 0.1, 0.1, 0.1, 0.1, 0.4),
  headers_alignment = c(0, 0.5, 0.5, 0.5, 0.5, 0.5),
  headers_background_colour = cmyk(40, 0, 20, 8),
  default_list = barplot_table_plot_pattern
)

usethis::use_data(barplot_noTrend, overwrite = TRUE)


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
