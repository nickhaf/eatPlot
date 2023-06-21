barplot_noTrend <- plotsettings_tablebarplot(
  background_stripes_colour = c(rep(c("white", cmyk(7, 0, 4, 1)), 8), cmyk(0, 0, 0, 10)),
  bar_background_lines = "scale_breaks", # set background lines at scalebreaks
  bar_background_lines_linetype = "solid",
  bar_pattern_width = 0.5, # Increase the size of the white stripes.
  bar_fill_colour = cmyk(60, 0, 30, 12),
  columns_alignment = c(0, 0.5, 0.5, 0.5, 0.5),
  columns_width = c(0.2, 0.1, 0.1, 0.1, 0.1, 0.4),
  headers_alignment = c(0, 0.5, 0.5, 0.5, 0.5, 0.5),
  headers_background_colour = cmyk(40, 0, 20, 8),
  default_list = barplot_table_plot_pattern
)

usethis::use_data(barplot_noTrend, overwrite = TRUE)
