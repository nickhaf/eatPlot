#' Plot Points.
#'
#' @keywords internal
#' @noRd
#'
#' @param plot_dat Data frame with the point estimates for every year. Prepared by ...
#' @inheritParams plot_lineplot
#' @inheritParams plot_single_lineplot
#' @inheritParams plotsettings_lineplot
#'
#' @return ggplot2 object
#'
#' @examples # tbd
plot_points <- function(plot_dat) {
  check_columns(plot_dat$plot_dat, c("point_est", "year", "trend", "point_sig"))

    plot_dat$dat_final <- calc_y_nudge(plot_dat,
    plot_settings = plot_dat$plot_settings
  )

  plot_dat$dat_final <- calc_x_nudge(plot_dat,
                                     nudge_x = plot_dat$plot_settings$point_label_nudge_x,
    plot_settings = plot_dat$plot_settings
  )
  if(plot_dat$plot_settings$split_plot == FALSE){
    plot_label <- plot_dat$dat_final[!duplicated(plot_dat$dat_final[, c("year", "point_est")]), ]
  }

  list(
    ggplot2::geom_point(ggplot2::aes(shape = .data$point_sig)),

    if (plot_dat$plot_settings$point_label_nudge == TRUE) {
      ggrepel::geom_text_repel(
        data = plot_label,
        ggplot2::aes(
          x = .data$x_coords,
          label = round(.data$point_est, 0),
        ),
        nudge_y = plot_label$nudge_y,
        min.segment.length = 100,
        size = plot_dat$plot_settings$point_label_size,
        direction = "y"
      )
    } else {
    ggplot2::geom_text(
      data = plot_label,
      ggplot2::aes(
        x = .data$x_coords,
        label = round(.data$point_est, 0)
      ),
      nudge_y = plot_label$nudge_y,
      size = plot_dat$plot_settings$point_label_size
    )
     }
  )
}
