#' Draw column spanners.
#'
#' @keywords internal
#' @noRd
#'
#' @param y_axis Numeric vector, containing y_axis coordinates.
#' @param spanners List of column spanners.
#' @param column_x_coords Data.frame containing the x coordinates of the table columns.
#' @param x_axis_range Numeric for the x axis range of the whole table.
#' @param headers_text_y Numeric for nudging the y starting position of the headers.
#' @param spanner_y Numeric for nudging the column spanners in y position away from the spanner lines. For 0, the spanning headers will be plotted directly into the spanner line.
#' @param plot_settings Plotsettings of tablebarplot.
#'
#' @return Can be added to a ggplot to draw column spanners on top of the plot.
#'
#' @examples
plot_column_spanners <- function(y_axis, spanners, column_x_coords, x_axis_range, headers_text_y,
                                 spanner_y, plot_settings){

  if(is.null(spanners)){
    return(NULL)
  }

  unlist(lapply(seq_along(spanners), function(spanner) {

    i <- spanners[[spanner]]

    if (length(i) == 1) {
      i <- rep(i, 2)
    }

    min_col <- i[1]
    max_col <- i[2]

    column_x_coords_rev <- column_x_coords[order(rev(rownames(column_x_coords))), ]
    header_x <- mean(
      c(
        max(column_x_coords_rev[c(min_col, max_col), "right"], na.rm = TRUE),
        min(column_x_coords_rev[c(min_col, max_col), "left"], na.rm = TRUE)
      ),
      na.rm = TRUE
    )

    annotations <- c(
      ## Column Spanner line:
      ggplot2::annotate("segment",
                        x = column_x_coords_rev[min_col, "left"] + 0.01 * x_axis_range,
                        xend = column_x_coords_rev[max_col, "right"] - 0.01 * x_axis_range,
                        y = max(y_axis) +
                          headers_text_y +
                          spanner_y +
                          2 * max(plot_settings$headers_nudge_y),
                        yend = max(y_axis) +
                          headers_text_y +
                          spanner_y +
                          2 * max(plot_settings$headers_nudge_y),
                        linewidth = 0.15
      ),
      ggtext::geom_richtext(
        data = data.frame(),
        ggplot2::aes(
          x = header_x,
          y = max(y_axis) +
            headers_text_y +
            spanner_y + # header to column_spanner line,
            spanner_y + # column_spanner line to column_spanner text
            3 * max(plot_settings$headers_nudge_y) +
            # lower border to header, header to column_spanner line, column_spanner line to column_spanner text
            plot_settings$column_spanners_nudge_y
        ),
        colour = "#000000",
        label = names(spanners)[spanner],
        size = plot_settings$headers_font_size,
        label.padding = grid::unit(rep(0, 4), "pt"),
        fill = NA,
        label.color = NA,
        hjust = 0.5
      )
    )

    return(annotations)
  }))

}


plot_column_headers <- function(column_x_coords_headers, headers, y_axis, headers_text_y, plot_settings) {
  lapply(1:nrow(column_x_coords_headers), function(i) {
    if (rev(plot_settings$headers_alignment)[i] == 0) {
      x_axis_i_header <- column_x_coords_headers$left[i]
    } else if (rev(plot_settings$headers_alignment)[i] == 0.5) {
      x_axis_i_header <- column_x_coords_headers$middle[i]
    } else if (rev(plot_settings$headers_alignment)[i] == 1) {
      x_axis_i_header <- column_x_coords_headers$right[i]
    } else {
      x_axis_i_header <- column_x_coords_headers$middle[i]
    }

    if (!is.null(headers)) {
      ggtext::geom_richtext(
        data = data.frame(),
        ggplot2::aes(
          x = x_axis_i_header,
          y = max(y_axis) +
            headers_text_y +
            rev(plot_settings$headers_nudge_y)[1]
        ),
        colour = "#000000",
        label = rev(headers)[[i]],
        size = plot_settings$headers_font_size,
        label.padding = grid::unit(rep(0, 4), "pt"),
        fill = NA,
        label.color = NA,
        hjust = rev(plot_settings$headers_alignment)[i],
        nudge_x = rev(plot_settings$headers_nudge_x)[i]
      )
    }
  })
}

