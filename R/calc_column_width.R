calc_column_width <- function(width_plot_1, width_plot_2, width_col_1){

  #get_plot_coords(plot)

  width_col_2 <- width_col_1 * width_plot_1 / width_plot_2

  return(width_col_2)

}

