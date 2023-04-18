## code to prepare `barplot_MinSta` dataset goes here
barplot_MinSta <- plotsettings_tablebarplot(
  background_stripes_colour = c(rep(c("white", "white", "white", "lightgrey", "lightgrey", "lightgrey"), 8), rep("darkgrey", 3)),
  bar_fill_colour = c("darkblue", "lightblue", "lightgreen"),
  bar_frame_linetype = c(`TRUE` = "solid", `FALSE` = "dashed"),
  bar_pattern_fill_colour = "white",
  bar_pattern_type = c("TRUE" = "stripe", "FALSE" = "none"),
  bar_sig_type = "pattern",
  bar_width = 0.7,
  headers_nudge_y = 1,
  columns_width = c(0.1, 0.15, 0.1, 0.1)
)

usethis::use_data(barplot_MinSta, overwrite = TRUE)
