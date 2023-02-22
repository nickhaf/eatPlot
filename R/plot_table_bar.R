#' Title
#'
#' @param p_1 Left plot. Currently a table_plot.
#' @param p_2 Right plot. Currently a bar_plot.
#'
#' @return ggplot2 objet with both plots aligned as one.
#' @export
#'
#' @examples #tbd
plot_table_bar <- function(p_1, p_2){

  ## Cut white border to the right of first plot
  p_1 <- p_1 +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, -0.1, 0, 0), "cm"))

  p_2 <- p_2 +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))

  patchwork::wrap_plots(p_1, p_2, widths = c(0.3, 1))  &
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
      # As margin is not perfectly eliminated
      axis.ticks.length.y = ggplot2::unit(0, "pt")
    )
  }
