

plot_tablebar <- function(dat,
                          column_bar = NULL,
                          columns_table = NULL,
                          columns_table_sig_bold = NULL,
                          columns_table_sig_high = NULL,
                          columns_table_se = NULL,
                          plot_settings = plotsettings_tablebarplot()) {
  new_colnames <- paste0("col_", 1:length(columns_table))

  for (i in seq_along(columns_table)) {
    dat[, new_colnames[i]] <- construct_label(
      dat,
      label_est = columns_table[[i]],
      label_se = columns_table_se[[i]],
      label_sig_bold = columns_table_sig_bold[[i]],
      label_sig_high = columns_table_sig_high[[i]]
    )
  }


  res_plot <- ggplot2::ggplot(
    data = dat
  ) +
    build_background_stripes(dat, plot_settings = plot_settings) +
    ggplot2::scale_y_continuous(expand = c(0, 0))


  if (!is.null(column_bar)) {
    res_plot <- res_plot +
      ggpattern::geom_rect_pattern(
        ggplot2::aes(
          xmin = x_min,
          xmax = x_max,
          ymin = y_axis - 0.4,
          ymax = y_axis + 0.4
        ),
        pattern = "stripe",
        linetype = 2,
        colour = "red"
      )
  }

  if (any(!is.null(columns_table))) {
    res_plot <- res_plot +
      build_columns_2(dat, cols = rev(new_colnames)) +
      # column headers:
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = -Inf, xmax = Inf,
          ymin = 4.5, ymax = Inf
        ),
        colour = "lightblue",
        fill = "lightblue"
      ) +
      ggplot2::annotate("text", x = 0, y = 4.8, label = "Header") +
      ggplot2::annotate("text", x = -3.5, y = 4.8, label = "Colspanner") +
      ggplot2::annotate("segment", x = -4.5, xend = -2.5, y = 4.7, yend = 4.7) +
      ggplot2::scale_x_continuous(breaks = c(-2, 0, 2), expand = c(0, 0))
  }

  return(res_plot)
}



build_columns_2 <- function(df, cols) {
  x_axis <- min(c(df$x_min, df$x_max))

  c(
    lapply(1:length(cols), function(i) {
      column_name <- cols[i]
      x_axis <- x_axis - i # Das muss natürlich Skalenabhängig sein, und skalierbar
      ggtext::geom_richtext(
        data = df,
        ggplot2::aes(
          x = x_axis,
          y = y_axis,
          label = .data[[column_name]]
        ),
        # size = plot_settings$brace_label_size,
        label.padding = grid::unit(rep(0, 4), "pt"),
        fill = NA,
        label.color = NA,
        hjust = 1
      )
    })
  )
}

build_background_stripes <- function(dat,
                                     plot_settings = plotsettings_tablebarplot()) {
  dat$background_colour <- rev(plot_settings$background_stripes_colour)


  stripes <- c(
    ggplot2::geom_tile(
      data = dat,
      ggplot2::aes(
        x = x_min,
        y = y_axis,
        width = Inf,
        height = 1,
        colour = background_colour,
        fill = background_colour
      )
    ),
    ggplot2::scale_fill_manual(
      breaks = dat$background_colour,
      values = dat$background_colour
    ),
    ggplot2::scale_colour_manual(values = dat$background_colour)
  )
  return(stripes)
}
