calc_plot_coords <- function(plot_dat, years_lines, years_braces, plot_settings) {

  years_list <- lapply(list(years_lines, years_braces), prep_years)
  names(years_list) <- c("years_lines", "years_braces")


  plot_coords <- list(years_list = years_list, plot_lims = plot_lims)
}
