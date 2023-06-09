lineplot_4x4_3groups <- plotsettings_lineplot(
  grouping_colours = c(cmyk(0, 0, 0, 100),
                       cmyk(0, 0, 0, 40),
                       cmyk(0, 0, 0, 20)
                       ),
  default_list = lineplot_4x4
)

usethis::use_data(lineplot_4x4_3groups, overwrite = TRUE)
