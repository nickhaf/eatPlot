#' Plot a table and/or a barchart.
#'
#' @param dat Data prepared with [prep_plot()].
#' @param bar_label Character string for the column that should be used for bar labels on top of the bars. If `NULL`, no labels are printed. Defaults to `NULL`.
#' @param bar_label_sig Character string for the column that should be used for marking the bar labels as significant.
#' @param bar_sig Character string for the column that should be used for marking the bars as significant.
#' @param bar_fill Character string for the column that groups the bar filling colours into different groups.
#' @param bar_est Character string for the column that contains the values for the bar chart. If `NULL`, no bar chart will be plotted.
#' @param headers Character vector containing the headers of the ploted table columns, including the bar table.
#' @param column_spanners Named list. The name of each element will be the column header. The list element itself has to be a numeric vector indicating which columns the column spanner should span.
#' #' @param column_spanners Named list. A second dimension of column spanners. The name of each element will be the column header. The list element itself has to be a numeric vector indicating which columns the column spanner should span.
#' @param columns_table List of character strings of the columns that should be plotted as table columns in the plot.
#' @param columns_table_sig_bold List of character strings of the columns that contain the significances for plotting significant values as bold.
#' @param columns_table_sig_high List of character strings of the columns that contain the significances for plotting significant values with a raised a.
#' @param columns_table_se List of character strings of the columns that contain standard errors, which will be plotted in brackets and rounded to `1`.
#' @param columns_round List of numerics, for rounding the column values. Insert `NULL` or `0` for no rounding/character columns.
#' @param plot_settings Named list constructed with `plotsettings_tablebarplot()`. Defaults to a list with all settings set to `0`. There are several predefined lists with optimized settings for different plots. See `plotsettings_tablebarplot()` for an overview.
#' @param y_axis Character string of the columnname used as y-axis. Has to contain unique values.
#'
#' @return [ggplot2] object.
#' @export
#'
#' @examples # tbd
plot_tablebar <- function(dat,
                          bar_est = NULL,
                          bar_label = NULL,
                          bar_label_sig = NULL,
                          bar_sig = NULL,
                          bar_fill = NULL,
                          columns_round = 0,
                          column_spanners = NULL,
                          column_spanners_2 = NULL,
                          columns_table = NULL,
                          columns_table_sig_bold = NULL,
                          columns_table_sig_high = NULL,
                          columns_table_se = NULL,
                          headers = NULL,
                          y_axis = NULL,
                          plot_settings = plotsettings_tablebarplot()) {
  ## Namen der Einstellungslisten checken: Taucht so in der entsprechenden Spalte auf? Kann man auch über die Reihenfolge gehen? eventl. in der scale_manual mit breaks arbeiten (oder so ähnlich) und dann nur Warnung ausgeben.

  if (inherits(dat, "list")) {
    dat <- dat$plot_tablebar
  }

  if (!inherits(headers, "list") & !is.character(headers) & !is.null(headers)) {
    stop("headers has to be a list, or a vector, or NULL.")
  }

  if(is.null(column_spanners) & !is.null(column_spanners_2)){
    stop("You provided column_spanners_2 but not column_spanners. Please use the lower dimension of column_spanners first.")
  }
  # Check columns -----------------------------------------------------------

  if (is.null(y_axis)) {
    stop("Please provide a y-axis.")
  }
  if (any(duplicated(dat[, y_axis]))) {
    stop("Your y-axis has to contain only unique values. Maybe you have to paste state_var and grouping_var into unique values?")
  }

  if (!is.numeric(dat[, bar_est]) & !is.null(bar_est)) {
    stop("Your 'bar_est' column needs to be numeric or NULL.", call. = FALSE)
  }



  dat <- fill_column(dat, column_name = bar_sig, filling = "FALSE")
  dat <- fill_column(dat, column_name = bar_fill, filling = "FALSE")
  dat <- fill_column(dat, column_name = bar_est, filling = NA)
  dat <- fill_column(dat, column_name = y_axis, filling = NA)

  if (!is.null(bar_est)) {
    n_table_cols <- length(columns_table) + 1
  } else {
    n_table_cols <- length(columns_table)
  }

  headers <- check_length(headers, n_table_cols, fill = " ")
  headers <- lapply(headers, function(x) {
    if (is.null(x)) {
      " "
    } else {
      x
    }
  })


  # Sub dashes so they are displayed correctly in pdf:
  for (i in columns_table) {
    dat[, i] <- sub_dash(dat[, i])
  }
  ## Hier nicht, da öfter ein Minus verwendet wird:
  # headers <- sub_dash(headers)
  # names(column_spanners) <- sub_dash(names(column_spanners))

  columns_round <- check_length(columns_round, length(columns_table), fill = columns_round)
  columns_table_sig_bold <- check_length(columns_table_sig_bold, length(columns_table), leng_1 = FALSE)
  columns_table_sig_high <- check_length(columns_table_sig_high, length(columns_table), leng_1 = FALSE)

  columns_table_se <- check_length(columns_table_se, length(columns_table), leng_1 = FALSE)

  if (is.null(plot_settings$headers_alignment)) {
    plot_settings$headers_alignment <- ifelse(plot_settings$columns_alignment == 2,
      0.5,
      plot_settings$columns_alignment
    )

    plot_settings$headers_alignment <- c(plot_settings$headers_alignment, rep(0.5, n_table_cols - length(plot_settings$headers_alignment)))
  }

  plot_settings$columns_alignment <- check_length(plot_settings$columns_alignment, length(columns_table), fill = plot_settings$columns_alignment[1])
  plot_settings$columns_nudge_x <- check_length(plot_settings$columns_nudge_x, length(columns_table), fill = plot_settings$columns_nudge_x[1])
  plot_settings$columns_nudge_y <- check_length(plot_settings$columns_nudge_y, length(columns_table), fill = plot_settings$columns_nudge_y[1])
  plot_settings$headers_alignment <- check_length(plot_settings$headers_alignment, n_table_cols, fill = plot_settings$headers_alignment[1])
  plot_settings$headers_nudge_x <- check_length(plot_settings$headers_nudge_x, n_table_cols, fill = plot_settings$headers_nudge_x[1])
  if (length(plot_settings$background_stripes_colour) < nrow(dat)) {
    plot_settings$background_stripes_colour <- fill_up(plot_settings$background_stripes_colour, leng = nrow(dat), fill = "white")
  }
  if (length(plot_settings$background_stripes_colour) > nrow(dat)) {
    plot_settings$background_stripes_colour <- plot_settings$background_stripes_colour[1:nrow(dat)]
  }

  ## Check Column widths
  ## Warnmeldung wenn eine col_width zu wenig: please provide one for the bar es well.
  ## Wenn nichts angegeben: zu gleichen Teilen ausrechnen
  if (!is.null(bar_est)) {
    columns_total <- append(columns_table, NA)
  } else {
    columns_total <- columns_table
  }

  if (is.null(plot_settings$columns_width)) {
    plot_settings$columns_width <- rep(1 / length(columns_total), length(columns_total))
  }

  if (length(plot_settings$columns_width) != length(columns_total)) {
    stop("Please provide either NULL or as many elements for plot_settings$columns_width as you have columns you want to plot. If you want to plot a bar, it also needs a width specification.")
  }

  if (sum(plot_settings$columns_width) < 0.98 | sum(plot_settings$columns_width) > 1.2) {
    stop("Your plot_settings$columns_width have to amount to approximatly 1.")
  }




  ## check if column names can be found in data
  sapply(unlist(c(
    bar_label,
    bar_sig,
    bar_fill,
    bar_est,
    columns_table,
    columns_table_sig_bold,
    columns_table_sig_high,
    columns_table_se,
    y_axis
  )), function(col) {
    check_column(dat, col)
  })

  new_colnames <- paste0("col_", 1:length(columns_table))

  for (i in seq_along(columns_table)) {
    dat <- construct_label(
      dat,
      new_name = new_colnames[i],
      label_est = columns_table[[i]],
      label_sig_bold = columns_table_sig_bold[[i]],
      label_sig_high = columns_table_sig_high[[i]],
      label_sig_high_extra_column = TRUE,
      round_est = columns_round[[i]],
      plot_settings = plot_settings
    )

    if (!is.null(columns_table_se[[i]])) {
      dat <- construct_label(
        dat,
        new_name = new_colnames[i],
        label_se = columns_table_se[[i]]
      )
    }
  }

  if (!is.null(bar_label)) {
    dat <- construct_label(
      dat,
      new_name = "bar_label_text",
      label_est = bar_label,
      label_se = NULL,
      label_sig_bold = bar_label_sig,
      label_sig_high = NULL,
      round_est = 1,
      plot_settings = plot_settings
    )
  }


  # Build data --------------------------------------------------------------
  dat$x_min <- rep(0, nrow(dat))
  if (!is.factor(dat$y_axis)) {
    dat$y_axis <- factor(dat$y_axis, levels = dat$y_axis)
  }
  dat$y_axis <- rev(as.integer(dat$y_axis))
  dat$background_colour <- plot_settings$background_stripes_colour

  if (!is.null(bar_est)) {
    plot_borders <- set_axis_limits(dat, x_value = c(dat$x_min, dat$bar_est), plot_settings)
    scale_breaks <- unique(c(
      seq(0, plot_borders[1], by = -10),
      seq(0, plot_borders[2], by = 10)
    ))
  } else {
    plot_borders <- c(0, 0)
    scale_breaks <- NULL
  }

  x_axis_range <- diff(range(plot_borders))



  ## Umgang mit Tabellen ohne Plot?
  ## Angabe benötigt was die range für den Plot ist, dann relativ easy berechenbar.

  # Set some nudging parameters ---------------------------------------------
  if (!is.null(column_spanners) == TRUE) {
    headers_text_height_y <- 0.5
  } else {
    headers_text_height_y <- 1 # space above and belower header
  }

  headers_text_y <- 0.5 + headers_text_height_y # space from last line to first text
  spanner_y <- 0.5

  column_x_coords <- calc_column_coords(plot_borders, columns_table, plot_settings)

  max_y <- max(dat$y_axis) +
    headers_text_y +
    max(plot_settings$headers_nudge_y) +
    headers_text_height_y +
    0.2 + # space to upper border is a bit smaller otherwise
    plot_settings$headers_background_width_y
  if (!is.null(column_spanners)) {
    max_y <- max_y +
      headers_text_height_y +
      spanner_y +
      2 * plot_settings$headers_nudge_y + # column_spanner line to text, text to upper border
      2 * plot_settings$column_spanners_nudge_y # below and above spanner text
    if(!is.null(column_spanners_2)){
      max_y <- max_y +
        headers_text_height_y +
        spanner_y +
        2 * plot_settings$headers_nudge_y + # column_spanner line to text, text to upper border
        2 * plot_settings$column_spanners_nudge_y # below and above spanner text
    }
  }

  plot_settings$bar_fill_colour <- construct_colour_scale(
    colours = plot_settings$bar_fill_colour,
    dat = dat,
    colname = "bar_fill"
  )





  # Plot --------------------------------------------------------------------

  res_plot <- ggplot2::ggplot(
    data = dat,
    mapping = ggplot2::aes(
      x = .data$x_min,
      y = .data$y_axis,
    )
  ) +
    build_background_stripes(dat, plot_settings = plot_settings) +
    ggplot2::scale_fill_manual(
      breaks = dat$background_colour,
      values = dat$background_colour
    ) +
    ggplot2::scale_colour_manual(
      breaks = dat$background_colour,
      values = dat$background_colour
    ) +
    add_vlines(plot_settings, plot_borders, dat$y_axis, bar_est) +
    ggplot2::scale_x_continuous(
      breaks = scale_breaks,
      limits = c(NA, max(column_x_coords$right)),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(0, 0.1))) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf, xmax = Inf,
        ymin = max(.data$y_axis) + 0.5, ymax = max_y
      ),
      colour = NA,
      fill = plot_settings$headers_background_colour
    ) +
    theme_table() +
    plot_capped_x_axis(scale_breaks) +
    ## Horizontal background lines around header box:
    ggplot2::annotate("segment", x = -Inf, xend = Inf, y = max(dat$y_axis) + 0.5, yend = max(dat$y_axis) + 0.5, linewidth = 0.1) +
    ggplot2::annotate("segment", x = -Inf, xend = Inf, y = max_y, yend = max_y, linewidth = 0.1) +
    if (!is.null(bar_label)) {
      ggtext::geom_richtext(
        ggplot2::aes(
          x = .data[[bar_label]],
          label = .data$bar_label_text
        ), #
        colour = "#000000",
        label.padding = grid::unit(rep(0, 4), "pt"),
        fill = NA,
        label.color = NA,
        hjust = plot_settings$bar_label_nudge_x,
        size = plot_settings$bar_label_size
      )
    }


  if (!is.null(bar_est)) {
    if (is.null(bar_sig)) {
      res_plot <- res_plot +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_rect(
          data = dat,
          ggplot2::aes(
            xmin = .data$x_min,
            xmax = .data$bar_est,
            ymin = .data$y_axis - plot_settings$bar_width / 2,
            ymax = .data$y_axis + plot_settings$bar_width / 2,
            fill = .data$bar_fill
          ),
          colour = "black",
          linewidth = plot_settings$bar_line_width
        ) +
        ggplot2::scale_fill_manual(values = plot_settings$bar_fill_colour) +
        NULL
    } else if (plot_settings$bar_sig_type == "pattern") {
      ## ggpattern can't deal with NAs, therefore convert them to FALSE (not significant):

      for (i in c(columns_table_sig_bold, columns_table_sig_high, bar_label_sig, "bar_sig")) {
        # if(any(is.na(dat[, i]))){warning("Your column '", i,"' contains missing values. They will be converted to FALSE for the plot.", .call = FALSE)
        # }
        dat <- fill_na(dat, column_name = i, filling = "FALSE")
      }


      res_plot <- res_plot +
        ggnewscale::new_scale_fill() +
        ggnewscale::new_scale_colour() +
        ggpattern::geom_rect_pattern(
          data = dat,
          ggplot2::aes(
            xmin = .data$x_min,
            xmax = .data$bar_est,
            ymin = .data$y_axis - plot_settings$bar_width / 2,
            ymax = .data$y_axis + plot_settings$bar_width / 2,
            # colour = .data$bar_fill,
            fill = .data$bar_fill,
            pattern = .data$bar_sig
          ),
          colour = "black",
          linewidth = plot_settings$bar_line_width,
          pattern_colour = NA,
          pattern_fill = plot_settings$bar_pattern_fill_colour,
          pattern_angle = -45,
          pattern_density = plot_settings$bar_pattern_width,
          pattern_spacing = plot_settings$bar_pattern_spacing,
          pattern_key_scale_factor = 0.6 # legend adjustment
        ) +
        ggpattern::scale_pattern_manual(values = plot_settings$bar_pattern_type) +
        ggplot2::scale_colour_manual(values = plot_settings$bar_fill_colour) +
        ggplot2::scale_fill_manual(values = plot_settings$bar_fill_colour) +
        ggpattern::scale_pattern_manual(values = plot_settings$bar_pattern_type) +
        ggplot2::scale_colour_manual(values = plot_settings$bar_fill_colour) +
        ggplot2::scale_fill_manual(values = plot_settings$bar_fill_colour) +
        NULL
    } else if (plot_settings$bar_sig_type == "frame") {
      res_plot <- res_plot +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_rect(
          data = dat,
          ggplot2::aes(
            xmin = .data$x_min,
            xmax = .data$bar_est,
            ymin = .data$y_axis - plot_settings$bar_width / 2,
            ymax = .data$y_axis + plot_settings$bar_width / 2,
            fill = .data$bar_fill,
            linetype = .data$bar_sig,
          ),
          colour = "black",
          linewidth = plot_settings$bar_line_width
        ) +
        ggplot2::scale_linetype_manual(values = plot_settings$bar_frame_linetype) +
        ggplot2::scale_fill_manual(values = plot_settings$bar_fill_colour) +
        NULL
    } else {
      message("`sig_type` must be either \"frame\" or \"pattern\"")
    }
  }

  if (any(!is.null(columns_table))) {
    res_plot <- res_plot +
      build_columns_3(dat,
        cols = rev(new_colnames),
        column_x_coords = column_x_coords,
        headers = rev(headers),
        plot_borders = plot_borders,
        plot_settings = plot_settings
      ) +
      plot_column_spanners(y_axis = dat$y_axis,
                           spanners = column_spanners,
                           column_x_coords,
                           x_axis_range,
                           headers_text_y,
                           spanner_y,
                           plot_settings) +
      plot_column_spanners(y_axis = dat$y_axis,
                           spanners = column_spanners_2,
                           column_x_coords,
                           x_axis_range,
                           headers_text_y + 1, # Second level has to start a bit above.
                           spanner_y,
                           plot_settings) +
    NULL
  }

  column_x_coords_headers <- column_x_coords[!is.na(column_x_coords$column), ]

  res_plot <- res_plot +
    plot_column_headers(column_x_coords_headers,
      headers,
      y_axis = dat$y_axis,
      headers_text_y,
      plot_settings
    )
  return(res_plot)
}



build_columns_3 <- function(df,
                            cols,
                            column_x_coords,
                            headers,
                            plot_borders,
                            plot_settings = plotsettings_tablebarplot()) {
  # n_cols <- if(!is.null(bar_est)){length(cols) + 1}else{length(cols)}

  column_x_coords_cols <- column_x_coords[!is.na(column_x_coords$column) & column_x_coords$column != "bar", ]

  columns_alignment <- plot_settings$columns_alignment
  x_range <- diff(range(plot_borders))
  c(
    lapply(1:length(cols), function(i) {
      ## Left alignment should start at the left side of the column. Right alignment is mainly needed for aligning the number, they can stay in the middle:
      if (rev(plot_settings$columns_alignment)[i] == 0) {
        x_axis_i <- column_x_coords_cols$left[i]
      } else if (rev(plot_settings$columns_alignment)[i] == 0.5) {
        x_axis_i <- column_x_coords_cols$middle[i]
      } else if (rev(plot_settings$columns_alignment)[i] == 1) {
        x_axis_i <- column_x_coords_cols$right[i]
      } else if (rev(plot_settings$columns_alignment)[i] == 2) { ## right align, but in the middle of the table:
        x_axis_i <- (column_x_coords_cols$middle[i] + column_x_coords_cols$right[i]) / 2
        columns_alignment[length(columns_alignment) - i + 1] <- 1
      }


      column_name <- cols[i]

      df$y_axis <- df$y_axis - ifelse(grepl("<br>", df[, column_name]), 0.25, 0)

      c(
        ggtext::geom_richtext(
          data = df,
          ggplot2::aes(
            x = x_axis_i,
            y = .data$y_axis,
            label = .data[[column_name]]
          ),
          colour = "#000000",
          size = plot_settings$font_size,
          label.padding = grid::unit(rep(0, 4), "pt"),
          fill = NA,
          label.color = NA,
          hjust = rev(columns_alignment)[i],
          nudge_x = rev(plot_settings$columns_nudge_x)[i],
          nudge_y = rev(plot_settings$columns_nudge_y)[i]
        ),
        add_superscript(df,
          column_name,
          x_coord = x_axis_i,
          i,
          x_range = x_range,
          plot_settings
        )
      )
    })
  )
}

build_background_stripes <- function(dat,
                                     plot_settings = plotsettings_tablebarplot()) {
  stripes <- c(
    ggplot2::geom_tile(
      data = dat,
      ggplot2::aes(
        x = .data$x_min,
        y = .data$y_axis,
        width = Inf,
        height = 1,
        fill = .data$background_colour,
        colour = .data$background_colour,
      )
    )
  )
  return(stripes)
}


set_axis_limits <- function(dat, x_value, plot_settings) {
  if (is.null(plot_settings$axis_x_lims)) {
    plot_borders <- calc_plot_borders(x_value)
    ## Plots with only positive values can start at 0.
    if (all(x_value[!is.na(x_value)] >= 0)) {
      plot_borders[1] <- 0
    }
  } else {
    plot_borders <- plot_settings$axis_x_lims
  }
  return(plot_borders)
}

check_length <- function(obj, leng, fill = NULL, leng_1 = TRUE) {
  if(leng_1 == TRUE){
  if (is.null(obj) & is.null(fill)) {
    return(NULL)
  } else if (is.null(obj) & !is.null(fill)) {
    obj <- rep(fill, leng)
  } else if (length(obj) != leng & length(obj) > 1) {
    stop(paste0("The length of ", deparse(substitute(obj)), " should be either equal to the amount of columns you are plotting or equal to 1. Note that your bar chart also is counted as a column if you are plotting one."), call. = FALSE)
  } else if (length(obj) == 1 & leng > 1) {
    obj <- fill_up(obj, leng, fill)
    return(obj)
  } else {
    return(obj)
  }
  }else{
    if(length(obj) != leng & !is.null(obj)){
      stop(paste0("The length of ", deparse(substitute(obj)), " should be equal to the amount of columns you are plotting."), call. = FALSE)
    }else{
      return(obj)
    }
  }

}

fill_up <- function(vec, leng, fill) {
  out <- c(vec, sapply(1:(leng - length(vec)), function(x) {
    fill
  }))
  return(out)
}

calc_column_coords <- function(plot_borders, columns_table = NULL, plot_settings) {
  x_axis_range <- diff(range(plot_borders))

  if (x_axis_range == 0) {
    total_range <- 1
    x_max <- 1
  } else {
    total_range <- x_axis_range / rev(plot_settings$columns_width)[1] ## Last element has to be the bar width
    x_max <- max(plot_borders, na.rm = TRUE)
  }

  cuts <- x_max
  for (i in seq_along(plot_settings$columns_width)) {
    cuts[i + 1] <- cuts[i] - total_range * rev(plot_settings$columns_width)[i]
  }

  if (x_axis_range != 0) { # because then there is a baplot
    col_coords <- data.frame(
      "column" = c("bar", unlist(rev(columns_table))),
      "left" = cuts[2:length(cuts)],
      "right" = cuts[1:length(cuts) - 1]
    )
  } else {
    col_coords <- data.frame(
      "column" = unlist(rev(columns_table)),
      "left" = cuts[2:length(cuts)],
      "right" = cuts[1:length(cuts) - 1]
    )
  }

  col_coords$middle <- rowMeans(col_coords[, c("left", "right")], na.rm = TRUE)
  col_coords <- col_coords[, order(names(col_coords))]

  return(col_coords)
}



add_superscript <- function(df, column_name, x_coord, i, x_range, plot_settings) {
  if (paste0(column_name, "_sig_superscript") %in% colnames(df)) {
    if (any(df[, paste0(column_name, "_sig_superscript")] != "")) {
      ggtext::geom_richtext(
        data = df,
        ggplot2::aes(
          x = x_coord,
          y = .data$y_axis,
          label = .data[[paste0(column_name, "_sig_superscript")]]
        ),
        colour = "#000000",
        size = plot_settings$font_size,
        label.padding = grid::unit(rep(0, 4), "pt"),
        fill = NA,
        label.color = NA,
        hjust = rev(plot_settings$columns_alignment)[i],
        nudge_x = rev(plot_settings$columns_nudge_x)[i] + plot_settings$columns_table_sig_high_letter_nudge_x,
        nudge_y = rev(plot_settings$columns_nudge_y)[i]
      )
    }
  }
}


add_vlines <- function(plot_settings, plot_borders, y_axis, bar_est) {
  if (!is.null(bar_est)) {
    scale_breaks <- unique(c(
      seq(0, plot_borders[1], by = -10),
      seq(0, plot_borders[2], by = 10)
    ))


    if (is.null(plot_settings$bar_background_lines_spanners)) {
      plot_settings$bar_background_lines_spanners <- list(c(max(y_axis) + 0.3, 0.7))
      line_spanners <- FALSE
    } else {
      line_spanners <- TRUE
    }

    lapply(plot_settings$bar_background_lines_spanners, function(y) {
      if (line_spanners == TRUE) {
        y_1 <- y_axis[y[1]]
        y_2 <- y_axis[y[2]]
      } else {
        y_1 <- y[1]
        y_2 <- y[2]
      }

      if (plot_settings$bar_background_lines == "border") {
        x_intercepts <- range(scale_breaks)
      } else if (plot_settings$bar_background_lines == "scale_breaks") {
        x_intercepts <- scale_breaks
      } else {
        x_intercepts <- 0
      }

      c(
        ggplot2::annotate(
          "segment",
          x = x_intercepts,
          xend = x_intercepts,
          y = y_1 + 0.2,
          yend = y_2 - 0.2,
          colour = "darkgrey",
          linetype = plot_settings$bar_background_lines_linetype,
          linewidth = 0.1
        ),
        ggplot2::annotate(
          "segment",
          x = 0,
          xend = 0,
          y = y_1 + 0.2,
          yend = y_2 - 0.2,
          colour = "darkgrey",
          linewidth = 0.1
        )
      )
    })
  }
}

# capped axis line
plot_capped_x_axis <- function(scale_breaks) {
  if (!is.null(scale_breaks)) {
    ggplot2::annotate("segment",
      x = min(scale_breaks),
      xend = max(scale_breaks),
      y = 0.4,
      yend = 0.4,
      linewidth = 0.1
    )
  }
}

check_linebreak <- function(vec) {
  logical_break <- any(sapply(vec, function(vec) {
    grepl("<br>", vec)
  }))
  return(logical_break)
}
