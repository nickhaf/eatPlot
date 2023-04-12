## code to prepare `barplot_MinSta` dataset goes here
barplot_MinSta <- plotsettings_barplot(
  axis_x_lims = NULL,
  background_stripes_colour = c(grDevices::rgb(219, 238, 244, maxColorValue = 255), "#00000000"),
  bar_fill_colour = c(
    "ohneAdj_TRUE" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
    "mitAdj_TRUE" = grDevices::rgb(33, 89, 104, maxColorValue = 255),
    "ohneAdj_FALSE" = "white",
    "mitAdj_FALSE" = "white"
  ),
  bar_frame_linetype = c("FALSE" = "dashed",
                         "TRUE" = "solid"),
  bar_pattern_fill_colour = c(
    "ohneAdj" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
    "mitAdj" = grDevices::rgb(33, 89, 104, maxColorValue = 255)
  ),
  bar_pattern_type = c(
    "TRUE" = "none",
    "FALSE" = "stripe"
  ),
  bar_sig_type = "pattern"
)


usethis::use_data(barplot_MinSta, overwrite = TRUE)
