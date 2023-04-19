combine_plots <- function(plot_list){

  coordinates <- vapply(plot_list, function(plot){
    diff(ggplot2::layer_scales(plot)$x$range$range)
  }, FUN.VALUE = numeric(1)
  )

  sum_coords <- sum(coordinates)
  plot_widths <- vapply(coordinates, function(coord_range){coord_range/sum_coords}, FUN.VALUE = numeric(1))

  patchwork::wrap_plots(plot_list, widths = plot_widths) &
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
      # As margin is not perfectly eliminated
      axis.ticks.length.y = ggplot2::unit(0, "pt")
    )


}
