

plot_tablebar <- function(dat,
                          bar_label = NULL,
                          bar_sig = NULL,
                          bar_fill = NULL,
                          bar_pattern_fill = NULL,
                          column_bar = NULL,
                          columns_table = NULL,
                          columns_table_sig_bold = NULL,
                          columns_table_sig_high = NULL,
                          columns_table_se = NULL,
                          plot_settings = plotsettings_tablebarplot()) {

  ## Hier checken ob die Längen der Argumente richtig sind, wenn nicht, dann ergänzen
  # Check columns -----------------------------------------------------------
  dat <- build_column_2(dat, column_name = bar_sig, filling = "FALSE")
  dat <- build_column_2(dat, column_name = bar_fill, filling = "FALSE")
  dat <- build_column_2(dat, column_name = bar_pattern_fill, filling = NA)


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

  ## Das sollte 0 werden wenn keine bars geplotted werden sollen
  plot_borders <- set_axis_limits(dat, x_value = c(dat$x_min, dat$x_max), plot_settings)
  scale_breaks <- unique(c(seq(0, plot_borders[1], by = -10),
                           seq(0, plot_borders[2], by = 10)))

  dat$background_colour <- rev(plot_settings$background_stripes_colour)


 res_plot <-  ggplot2::ggplot(
    data = dat,
    mapping = ggplot2::aes(
      x = .data$x_min,
      y = .data$y_axis#,
      #linetype = .data$bar_sig
    )
  ) +
    build_background_stripes(dat, plot_settings = plot_settings) +
    ggplot2::scale_fill_manual(
      breaks = dat$background_colour,
      values = dat$background_colour
    ) +
    ggplot2::scale_colour_manual(values = dat$background_colour) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::geom_vline(
      xintercept = scale_breaks,
      linetype = "dashed", colour = "darkgrey"
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "darkgrey"
    )  +
    ggplot2::scale_x_continuous(breaks = scale_breaks) +
    theme_table()     +
    if(!is.null(bar_label)){
      ggplot2::geom_text(ggplot2::aes(label = .data[[bar_label]]), hjust = -0.2)
    }


 if (!is.null(column_bar)) {
   if (plot_settings$bar_sig_type == "pattern") {

res_plot <- res_plot +
    ggnewscale::new_scale_fill() +
    ggpattern::geom_rect_pattern(
      data = dat,
      ggplot2::aes(
        xmin = .data$x_min,
        xmax = .data$x_max,
        ymin = .data$y_axis - 0.4,
        ymax = .data$y_axis + 0.4,
        #colour = .data$bar_fill,
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
    ggplot2::scale_fill_manual(values = plot_settings$bar_fill) +
    theme_table_bar() +
    NULL
}
} else if (plot_settings$bar_sig_type == "frame") {
  res_plot <- res_plot +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_rect(
      data = dat,
      ggplot2::aes(
        xmin = x_min,
        xmax = x_max,
        ymin = y_axis - 0.4,
        ymax = y_axis + 0.4,
        fill = .data$bar_fill,
        linetype = .data$bar_sig,
        ),
      colour = "black",
      linewidth = 0.9
    ) +
        ggplot2::scale_linetype_manual(values = plot_settings$bar_frame_linetype) +
        theme_table_bar() +
        NULL
    } else {
      message("`sig_type` must be either \"frame\" or \"pattern\"")
    }

  if (any(!is.null(columns_table))) {
    res_plot <- res_plot +
      build_columns_3(dat,
                      cols = rev(new_colnames),
                      plot_borders = plot_borders,
                      plot_settings = plot_settings) +
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
      #ggplot2::scale_x_continuous(breaks = c(-2, 0, 2), expand = c(0, 0)) +
      NULL
  }

  return(res_plot)
}



build_columns_3 <- function(df,
                            cols,
                            plot_borders = 0,
                            plot_settings = plotsettings_tablebarplot()) {

  x_axis_min <- min(c(df$x_min, df$x_max))
  x_axis_range <- diff(range(plot_borders))
#   df$x_axis <- x_axis_min - df$y_axis - (i * x_axis_range * plot_settings$column_width[df$y_axis])


  c(
    lapply(1:length(cols), function(i) {
      column_name <- cols[i]
      x_axis <- x_axis_min - i - (i * x_axis_range * plot_settings$columns_width[i])

      ggtext::geom_richtext(
        data = df,
        ggplot2::aes(
          x = x_axis,
          y = .data$y_axis,
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


  stripes <- c(
    ggplot2::geom_tile(
      data = dat,
      ggplot2::aes(
        x = .data$x_min,
        y = .data$y_axis,
        width = Inf,
        height = 1,
        colour = .data$background_colour,
        fill = .data$background_colour
      )
   )
  )
  return(stripes)
}

set_axis_limits <- function(dat, x_value, plot_settings){

  if(is.null(plot_settings$axis_x_lims)){
    plot_borders <- calc_plot_borders(x_value)
    ## Plots with only positive values can start at 0.
    if(all(x_value[!is.na(x_value)] >= 0)){
      plot_borders[1] <- 0
    }
  }else{
    plot_borders <- plot_settings$axis_x_lims
  }
  return(plot_borders)
}
