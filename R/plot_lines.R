#' Connect Points
#'
#' @param my_data Data
#' @param grouping_var Variable for grouping
#'
#' @return ggplot object
#' @export
#'

connect_points <- function(data_prep, grouping_var) {
  list(
    ggplot2::geom_segment(
      data = my_data,
      ggplot2::aes(
        x = year_start,
        xend = year_end,
        y = est_start,
        yend = est_end,
        colour = .data[[grouping_var]],
        linetype = sigTrend_within
      ),
      size = 0.7),
    linetype_iqb
  )

  return(plot_res)
}

## Connection with prep_points, sig from prep_line
