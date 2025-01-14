#' Combine different ggplots
#'
#' @param plot_list List of the plots that should be combined.
#' @param plot_widths Set the widths of the plots manually. Defaults to `NULL`, in which case the width will be calculated so the x axis scales are on the same metric.
#'
#' @return Patchwork plot.
#' @export
#'
#' @examples # tbd
combine_plots <- function(plot_list, plot_widths = NULL) {
  if (!is.null(plot_widths)) {
    warning("Careful! You have provided plot_widths. Please be beware that the x-axes might be distorted, if your plot_widths don't account for different x-axis ranges.", call. = FALSE)
  }

  if (is.null(plot_widths)) {
    coordinates <- get_coordinates(plot_list)

    sum_coords <- sum(coordinates)
    plot_widths <- vapply(coordinates, function(coord_range) {
      coord_range / sum_coords
    }, FUN.VALUE = numeric(1))
  }

  patchwork::wrap_plots(plot_list, widths = plot_widths) &
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
      # As margin is not perfectly eliminated
      axis.ticks.length.y = ggplot2::unit(0, "pt")
    )
}


# Utils -------------------------------------------------------------------
get_coordinates <- function(plot_list) {
  coords <- vapply(plot_list, function(plot) {
    get_x_range(plot)
  }, FUN.VALUE = numeric(1))

  return(coords)
}
