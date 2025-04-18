#' Set the colours, pointshapes and linetypes for lineplot.
#'
#'
#' @inheritParams plot_lineplot
#' @return ggplot2 plot settings
#' @export
#'
#' @examples # tbd
set_scales <- function(plot_settings = plotsettings_lineplot()) {
  list(
    if (!is.null(plot_settings$line_type)) {
      ggplot2::scale_linetype_manual(values = plot_settings$line_type)
    },
    if (!is.null(plot_settings$point_shapes)) {
      ggplot2::scale_shape_manual(values = plot_settings$point_shapes)
    },
    if (!is.null(plot_settings$subgroup_colours)) {
      ggplot2::scale_colour_manual(
        values = plot_settings$subgroup_colours
      )
    }
  )
}
