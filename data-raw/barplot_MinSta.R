## code to prepare `barplot_MinSta` dataset goes here
barplot_MinSta <- plotsettings_barplot(
  background_stripes_colour = c(grDevices::rgb(219, 238, 244, maxColorValue = 255), "#00000000")
  )


usethis::use_data(barplot_MinSta, overwrite = TRUE)
