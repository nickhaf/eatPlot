

#' Plot a table and/or a barchart.
#'
#' @param dat Data prepared with [prep_trend()].
#' @param bar_label Character string for the column that should be used for bar labels on top of the bars. If `NULL`, no labels are printed. Defaults to `NULL`.
#' @param bar_sig Character string for the column that should be used for marking the bars as significant.
#' @param bar_fill Character string for the column that groups the bar filling colours into different groups.
#' @param bar_header Character string for the header of the bars.
#' @param bar_pattern_fill Character string for the column that groups the filling colours of the bar patterns into different groups.
#' @param bar_est Character string for the column that contains the values for the bar chart. If `NULL`, no bar chart will be plotted.
#' @param columns_headers Character vector containing the column columns_headers.
#' @param columns_table List of character strings of the columns that should be plotted as table columns in the plot.
#' @param columns_table_sig_bold List of character strings of the columns that contain the significances for plotting significant values as bold.
#' @param columns_table_sig_high List of character strings of the columns that contain the significances for plotting significant values with a raised a.
#' @param columns_table_se List of character strings of the columns that contain standard errors, which will be plotted in brackets behind the column values.
#' @param plot_settings Named list constructed with `plotsettings_tablebarplot()`. Defaults to a list with all settings set to `0`. There are several predefined lists with optimized settings for different plots. See `plotsettings_tablebarplot()` for an overview.
#' @param y_axis Character string of the columnname used as y-axis.
#'
#' @return [ggplot2] object.
#' @export
#'
#' @examples #tbd
plot_tablebar <- function(dat,
                          bar_est = NULL,
                          bar_label = NULL,
                          bar_sig = NULL,
                          bar_fill = NULL,
                          bar_header = NULL,
                          bar_pattern_fill = NULL,
                          columns_headers = NULL,
                          columns_table = NULL,
                          columns_table_sig_bold = NULL,
                          columns_table_sig_high = NULL,
                          columns_table_se = NULL,
                          y_axis = NULL,
                          plot_settings = plotsettings_tablebarplot()) {

  ## Namen der Einstellungslisten checken: Taucht so in der entsprechenden Spalte auf? Kann man auch über die Reihenfolge gehen? eventl. in der scale_manual mit breaks arbeiten (oder so ähnlich) und dann nur Warnung ausgeben.

  # Check columns -----------------------------------------------------------

  if(is.null(y_axis)){stop("Please provide a y-axis.")}

  dat <- build_column_2(dat, column_name = bar_sig, filling = "FALSE")
  dat <- build_column_2(dat, column_name = bar_fill, filling = "FALSE")
  dat <- build_column_2(dat, column_name = bar_pattern_fill, filling = NA)
  dat <- build_column_2(dat, column_name = bar_est, filling = NA)
  dat <- build_column_2(dat, column_name = y_axis, filling = NA)


  ## Hier alle benötigten Spalten bauen mit entsprechenden Defaults. Danach checken, ob richtiges Format. Wenn NULL, sollte ein Default gebaut werden, der im Plot nicht zu sehen ist.

  columns_headers <- check_length(columns_headers, length(columns_table))
  columns_table_sig_bold <- check_length(columns_table_sig_bold, length(columns_table))
  columns_table_sig_high <- check_length(columns_table_sig_high, length(columns_table))
  columns_table_se <- check_length(columns_table_se, length(columns_table))


  ## check if column names can be found in data
  sapply(unlist(c(
    bar_label,
    bar_sig,
    bar_fill,
    bar_pattern_fill,
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
    dat[[new_colnames[i]]] <- construct_label(
      dat,
      label_est = columns_table[[i]],
      label_se = columns_table_se[[i]],
      label_sig_bold = columns_table_sig_bold[[i]],
      label_sig_high = columns_table_sig_high[[i]]
    )
  }



  # Build data --------------------------------------------------------------
  dat$x_min <- rep(0, nrow(dat))
  dat$y_axis <- as.factor(dat$y_axis)
  dat <- dat[order(dat$y_axis), ]
  dat$y_axis <- rev(as.integer(dat$y_axis))
  dat$background_colour <- plot_settings$background_stripes_colour

  ## Das sollte 0 werden wenn keine bars geplotted werden sollen
  plot_borders <- set_axis_limits(dat, x_value = c(dat$x_min, dat$bar_est), plot_settings)
  scale_breaks <- unique(c(
    seq(0, plot_borders[1], by = -10),
    seq(0, plot_borders[2], by = 10)
  ))

  x_axis_min <- plot_borders[1]
  x_axis_range <- diff(range(plot_borders))

  x_axis <- vapply(1:length(columns_table), function(i) {
    x_axis_min - i - (i * x_axis_range * rev(plot_settings$columns_width)[i])
  },
  FUN.VALUE = numeric(1)
  )


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
    ggplot2::geom_vline(
      xintercept = scale_breaks,
      linetype = "dashed", colour = "darkgrey"
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "darkgrey"
    ) +
    ggplot2::scale_x_continuous(breaks = scale_breaks) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .05))) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf, xmax = Inf,
        ymin = max(.data$y_axis) + 0.5, ymax = Inf
      ),
      colour = "lightblue",
      fill = "lightblue"
    ) +
    theme_table() +
    if (!is.null(bar_label)) {
      ggplot2::geom_text(ggplot2::aes(label = .data[[bar_label]]), hjust = -0.2)
    }


  if (!is.null(bar_est)) {
    if (plot_settings$bar_sig_type == "pattern") {
      res_plot <- res_plot +
        ggnewscale::new_scale_fill() +
        ggpattern::geom_rect_pattern(
          data = dat,
          ggplot2::aes(
            xmin = .data$x_min,
            xmax = .data$bar_est,
            ymin = .data$y_axis - plot_settings$bar_width / 2,
            ymax = .data$y_axis + plot_settings$bar_width / 2,
            # colour = .data$bar_fill,
            fill = .data$bar_fill,
            pattern = .data$bar_sig,
            pattern_colour = .data$bar_pattern_fill,
            pattern_fill = .data$bar_pattern_fill
          ),
          colour = NA,
          pattern_colour = "white",
          pattern_angle = -45,
          pattern_density = 0.4, # Streifenbreite
          pattern_spacing = 0.01, # Abstand
          pattern_key_scale_factor = 0.6
        ) +
        ggpattern::scale_pattern_manual(values = plot_settings$bar_pattern_type) +
        ggpattern::scale_pattern_fill_manual(values = plot_settings$bar_pattern_fill_colour) +
        ggplot2::scale_fill_manual(values = plot_settings$bar_fill_colour) +
        ggplot2::annotate("text",
                          x = mean(range(scale_breaks)),
                          y = max(dat$y_axis) + 1 + plot_settings$headers_nudge_y,
                          label = bar_header) +
        theme_table_bar() +
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
          linewidth = 0.9) +
        ggplot2::scale_linetype_manual(values = plot_settings$bar_frame_linetype) +
        ggplot2::scale_fill_manual(values = plot_settings$bar_fill_colour) +
        ggplot2::annotate("text", x = diff(range(plot_borders)) / 2, y = max(dat$y_axis) + 1 + plot_settings$headers_nudge_y, label = bar_header) +
        theme_table_bar() +
        NULL
    } else {
      message("`sig_type` must be either \"frame\" or \"pattern\"")
    }
  }

  if (any(!is.null(columns_table))) {
    res_plot <- res_plot +
      build_columns_3(dat,
        cols = rev(new_colnames),
        columns_headers = rev(columns_headers),
        x_axis = x_axis,
        plot_settings = plot_settings
      ) +
      # ggplot2::annotate("text", x = 0, y = 4.8, label = "Header") +
      #  ggplot2::annotate("text", x = -3.5, y = 4.8, label = "Colspanner") +
      # ggplot2::annotate("segment", x = -4.5, xend = -2.5, y = 4.7, yend = 4.7) +
      NULL
  }

  return(res_plot)
}



build_columns_3 <- function(df,
                            cols,
                            columns_headers,
                            x_axis,
                            plot_settings = plotsettings_tablebarplot()) {
  c(
    lapply(1:length(cols), function(i) {
      x_axis_i <- x_axis[i]
      column_name <- cols[i]

      c(
        ggtext::geom_richtext(
          data = df,
          ggplot2::aes(
            x = x_axis_i,
            y = .data$y_axis,
            label = .data[[column_name]]
          ),
          # size = plot_settings$brace_label_size,
          label.padding = grid::unit(rep(0, 4), "pt"),
          fill = NA,
          label.color = NA,
          hjust = 0
        ),
        ggplot2::annotate("text", x = x_axis_i, y = max(df$y_axis) + 1 + plot_settings$headers_nudge_y, label = columns_headers[i], hjust = 0)
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
        fill = .data$background_colour
      ),
      colour = NA
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

check_length <- function(obj, leng){
  if(is.null(obj)){
    return(NULL)
  }else if(length(obj) != leng){
    warning(paste0("The length of " , deparse(substitute(obj)), " should be equal to the amount of columns you are plotting.", call. = FALSE))
    obj <- c(obj, lapply(1:(leng - length(obj)), function(x){NULL}))
  }else{
    return(obj)
  }
}
