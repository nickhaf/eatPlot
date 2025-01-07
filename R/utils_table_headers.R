# Column headers ----------------------------------------------------------

#' Draw column headers.
#'
#' @keywords internal
#' @noRd
#'
#' @param column_x_coords_headers Data.frame containing the x coordinates of the table columns that get a header.
#' @param headers List containing character strings of the headers.
#' @param y_axis  Numeric vector, containing y_axis coordinates.
#' @param headers_text_y Numeric for nudging the y starting position of the headers.
#' @param plot_settings Plotsettings of tablebarplot.
#'
#' @return Can be added to a ggplot to draw headers on top of the plot.
#'
#' @examples # tbd
plot_column_headers <- function(column_x_coords_headers,
                                headers,
                                header_y_coords,
                                n_table_cols,
                                plot_settings) {

  plot_settings <- check_headers_requirements(plot_settings, n_table_cols)

  ## Headers_nudge_y needs to be checked in length!

  lapply(1:nrow(column_x_coords_headers), function(i) {
    x_axis_i_header <- set_headers_alignment(
      header_pos = i,
      column_x_coords_headers,
      plot_settings
    )
    if (!is.null(headers)) {
      ggtext::geom_richtext(
        data = data.frame(),
        ggplot2::aes(
          x = x_axis_i_header,
          y = header_y_coords$header_area_start +
            header_y_coords$row_height_headers / 2 +
            rev(plot_settings$headers_nudge_y)[i]
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

#################################
## Utils plot_column_headers() ##
#################################

check_headers_requirements <- function(plot_settings, n_table_cols) {

  if (is.null(plot_settings$headers_alignment)) {
    plot_settings$headers_alignment <- ifelse(plot_settings$columns_alignment == 2,
      0.5,
      plot_settings$columns_alignment
    )
    plot_settings$headers_alignment <- c(plot_settings$headers_alignment,
                                         rep(0.5, n_table_cols - length(plot_settings$headers_alignment))
                                         )
  }

  plot_settings$headers_alignment <- check_length(
    plot_settings$headers_alignment,
    n_table_cols,
    fill = plot_settings$headers_alignment[1]
  )
  plot_settings$headers_nudge_x <- check_length(
    plot_settings$headers_nudge_x,
    n_table_cols,
    fill = plot_settings$headers_nudge_x[1])
  plot_settings$headers_nudge_y <- check_length(
    plot_settings$headers_nudge_y,
    n_table_cols,
    fill = plot_settings$headers_nudge_x[1])

  return(plot_settings)
}


set_headers_alignment <- function(header_pos, column_x_coords_headers, plot_settings) {

  if (rev(plot_settings$headers_alignment)[header_pos] == 0) {
    x_axis_i_header <- column_x_coords_headers$left[header_pos]
  } else if (rev(plot_settings$headers_alignment)[header_pos] == 0.5) {
    x_axis_i_header <- column_x_coords_headers$middle[header_pos]
  } else if (rev(plot_settings$headers_alignment)[header_pos] == 1) {
    x_axis_i_header <- column_x_coords_headers$right[header_pos]
  } else {
    x_axis_i_header <- (column_x_coords_headers$middle[header_pos]) #+ column_x_coords_headers$right[header_pos])/2
  }
  return(x_axis_i_header)
}



# Column spanners ---------------------------------------------------------

check_spanners_requirements <- function(column_spanners, column_spanners_2, plot_settings){

  plot_settings$column_spanners_nudge_y <- check_length(plot_settings$column_spanners_nudge_y,
                                  length(column_spanners),
                                  fill = plot_settings$column_spanners_nudge_y[1]
    )
  if(!is.null(column_spanners_2)){
    plot_settings$column_spanners_2_nudge_y <- check_length(plot_settings$column_spanners_2_nudge_y,
                                    length(column_spanners_2),
                                    fill = plot_settings$column_spanners_2_nudge_y[1]
      )
    }

  return(plot_settings)
}

#' Draw column spanners.
#'
#' @keywords internal
#' @noRd
#'
#' @inheritParams plot_column_headers
#' @param spanners List of column spanners.
#' @param column_x_coords Data.frame containing the x coordinates of the table columns.
#' @param x_axis_range Numeric for the x axis range of the whole table.
#'
#' @return Can be added to a ggplot to draw column spanners on top of the plot.
#'
#' @examples # tbd
plot_column_spanners <- function(y_axis, spanners, column_x_coords, x_axis_range, header_y_coords, spanners_2 = FALSE, plot_settings) {

  if (is.null(spanners)) {
    return(NULL)
  }
  unlist(lapply(seq_along(spanners), function(spanner) {

    i <- spanners[[spanner]]

    if (length(i) == 1) {
      i <- rep(i, 2)
    }

    min_col <- i[1]
    max_col <- i[2]

    column_x_coords_rev <- column_x_coords[order(rev(as.numeric(rownames(column_x_coords)))), ]
    header_x <- mean(
      c(
        max(column_x_coords_rev[c(min_col, max_col), "right"], na.rm = TRUE),
        min(column_x_coords_rev[c(min_col, max_col), "left"], na.rm = TRUE)
      ),
      na.rm = TRUE
    )

    spanner_line_y <- header_y_coords$header_area_start +
      header_y_coords$row_height_headers

    spanner_text_y <- spanner_line_y +
      (header_y_coords$row_height_column_spanners / 2) +
      plot_settings$column_spanners_nudge_y[spanner] # Nudge a bit down, so the text stands on the line

    if(spanners_2 == TRUE){
      spanner_line_y <- spanner_line_y +
        header_y_coords$row_height_column_spanners

      spanner_text_y <- spanner_line_y +
        (header_y_coords$row_height_column_spanners_2) / 2 +
        plot_settings$column_spanners_2_nudge_y[spanner] # Nudge a bit down, so the text stands on the line
    }


    annotations <- c(
      ## Column Spanner line:
      ggplot2::annotate("segment",
        x = column_x_coords_rev[min_col, "left"] + 0.01 * x_axis_range,
        xend = column_x_coords_rev[max_col, "right"] - 0.01 * x_axis_range,
        y = spanner_line_y,
        yend = spanner_line_y,
        linewidth = 0.15
      ),
      ggtext::geom_richtext(
        data = data.frame(),
        ggplot2::aes(
          x = header_x,
          y = spanner_text_y
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
