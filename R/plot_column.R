
#' Plot y-axis of a merged plot.
#'
#' @param vec Character vector of the strings that should be plotted as y-axis labels. E.g., states.
#' @param title Character string of the column title that will be ploted. Defaults to `"y_value"`
#' @param fontsize Numeric for the fontsize of the text in the column. Defaults to `"3"`.
#' @param alignment Character string for the alignmen of the column text. Possible are `"left", "center", "right"`. Defaults to `"left"`.
#' @param nudge_x Numeric for nudging the column text on the x-axis. Defaults to `-0.55`.
#' @param ... Arguments for [ggplot2::geom_text()].
#'
#' @return ggplot2 object
#' @export
#'
#' @examples # tbd
plot_column <- function(vec,
                        title = "y_value",
                        ...,
                        fontsize = 3,
                        alignment = "left",
                        nudge_x = -0.55) {
  dat <- as.data.frame(vec)
  dat$x_label <- rep(title, nrow(dat))

  column_settings <- c(
    ggplot2::geom_text(
      data = dat,
      ggplot2::aes(
        x = .data$x_label,
        y = .data$vec,
        label = .data$vec
      ),
      inherit.aes = FALSE,
      size = fontsize,
      hjust = alignment,
      nudge_x = nudge_x,
      ...
    ),
    ggplot2::scale_x_discrete(position = "top", name = NULL)

  )
  return(column_settings)
}
