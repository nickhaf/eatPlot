#' Plot braces below plot.
#'
#' @param data_plot_braces Prepared Trend data.
#' @param BL Bundesland, for which the labels should be drawn.
#' @inheritParams plot_lineplot
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples ##
plot_braces <- function(data_plot_braces, BL, label_est, label_se, label_sig_high, label_sig_bold) {
  # Get range for y-axis
  range_est <- range(c(data_plot_braces$est_point_start, data_plot_braces$est_point_end), na.rm = TRUE)

  # Here the coordinates for the braces and brace labels are calculated.
  # Change in the functions for fine tuning.
  coords <- calc_coords(range_est)
  brace_coords <- calc_brace_coords(data_plot_braces, coords)

  brace_coords$label_est <- ifelse(brace_coords[, label_sig_bold] == TRUE, paste0("**", round(brace_coords[, label_est], 0), "**"), round(brace_coords[, label_est], 0))
  brace_coords$label_sig <- ifelse(brace_coords[, label_sig_high] == TRUE, "<sup>a</sup>", "")
  brace_coords$label_se <- brace_coords[, label_se]

  c(
    ## Loop to draw a brace for every year_start
    lapply(unique(data_plot_braces$year_start), function(x) {
      coordinates <- brace_coords[brace_coords$year_start == x, ]
      coordiantes <- unique(brace_coords[, c("year_start", "year_end", "brace_upper_y", "brace_lower_y")])

      ggbrace::geom_brace(
        mapping = ggplot2::aes(
          x = c(coordinates$year_start, coordinates$year_end),
          y = c(coordinates$brace_upper_y, coordinates$brace_lower_y)
        ),
        mid = ifelse(coordinates$year_start == min(brace_coords$year_start), 0.25, 0.5),
        inherit.data = F,
        rotate = 180,
        linewidth = 0.8,
        npoints = 200
      )
    }),
    plot_brace_label(brace_coords, BL),
    # Clip Coordinate system. Necessary, so the brace can be drawn outside of the plot
    ggplot2::coord_cartesian(
      clip = "off",
      ylim = coords #,
      #expand = FALSE
      )
  )
}





## Utils
plot_brace_label <- function(brace_coords, BL) {
  ggtext::geom_richtext(
    data = brace_coords[brace_coords$TR_BUNDESLAND == BL, ],
    mapping = ggplot2::aes(
      x = .data$label_pos_x,
      y = .data$label_pos_y,
      label = paste0(
        .data$label_est,
        .data$label_sig,
        " (", round(.data$label_se, 1), ")"
      )
    ),
    size = 3,
    label.padding = grid::unit(rep(0, 4), "pt"),
    fill = NA,
    label.color = NA
  )
}

calc_pos_label_x <- function(year_start, year_end, brace_indent_pos) {
  year_start + (year_end - year_start) * brace_indent_pos
}
