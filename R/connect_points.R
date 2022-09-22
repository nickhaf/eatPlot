connect_points <- function(my_data, grouping_var ){

  list(
  plot_res <- geom_segment(data = my_data,
                           aes(
                             x = year_start,
                             xend = year_end,
                             y =  est_start,
                             yend = est_end,
                             colour = .data[[grouping_var]],
                             linetype = sigTrend_within),
                           size = 0.7),
  linetype_iqb
  )

  return(plot_res)
}

