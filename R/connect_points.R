connect_points <- function(my_data, year1, year2, grouping_var ){

  plot_res <- geom_segment(data = my_data,
                           aes(
                             x = rep(as.numeric(year1), nrow(my_data)),
                             xend = rep(as.numeric(year2), nrow(my_data)),
                             y = get(paste0("est_", year1)),
                             yend = get(paste0("est_", year2)),
                             colour = .data[[grouping_var]],
                             linetype = get(paste0("sig_", year1, ".vs.", year2))
                           ))
  return(plot_res)
}
