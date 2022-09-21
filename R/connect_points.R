connect_points <- function(my_data, grouping_var ){

  ## Hier Point estimates und significancen gleichzeitig. Kriege ich das mit start und EndJahr zusammen mit pointestimates in einen Datensatz?
  ## year1, year 2, est_year_1, est_year_2,

  years_vec <- unique(my_data$year_start)

  list(
  plot_res <- geom_segment(data = my_data,
                           aes(
                             x = years_vec[1],
                             xend = years_vec[2],
                             y =  my_data[my_data$year_star == years_vec[1], "est" ],
                             yend = est_2021,
                             colour = .data[[grouping_var]],
                             linetype = get(paste0("sig_trend"))
                           ),
                           size = 0.7)

  )

  return(plot_res)
}

