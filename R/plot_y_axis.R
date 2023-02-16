
#' Plot y-axis of a merged plot.
#'
#' @param vec Character vector of the strings that should be plotted as y-axis labels. E.g., states.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #tbd
plot_y_axis <- function(vec){
  dat <- as.data.frame(vec)
  dat$x_label <- rep("y_value", nrow(dat))

y_axis_settings <-   c(
  ggplot2::geom_text(data = dat,
    ggplot2::aes(x = .data$x_label,
                 y = .data$vec,
                 label = .data$vec),
    size = 3,
    hjust = "left",
    nudge_x = -0.55,
    inherit.aes = FALSE
  ),
  ggstats::geom_stripped_rows(
    odd = grDevices::rgb(219, 238, 244, maxColorValue = 255),
    even = "#00000000"
  )
  )
return(y_axis_settings)
}
