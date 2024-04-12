#' Plot braces below plot.
#'
#' @param dat Prepared Trend data.
#' @inheritParams plot_lineplot
#' @inheritParams plot_single_lineplot
#' @inheritParams plotsettings_lineplot
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples ##
plot_braces <- function(dat,
                        plot_lims,
                        label_est = NULL,
                        label_se = NULL,
                        label_sig_high = NULL,
                        label_sig_bold = NULL,
                        plot_settings = plotsettings_lineplot()) {
  sapply(c(label_est, label_se, label_sig_high, label_sig_bold), check_column, dat = dat)


  dat <- fill_column(dat, column_name = label_est, filling = NA)
  dat <- fill_column(dat, column_name = label_se, filling = NA)
  dat <- fill_column(dat, column_name = label_sig_high, filling = FALSE)
  dat <- fill_column(dat, column_name = label_sig_bold, filling = FALSE)


  # Construct brace labels --------------------------------------------------
  ## Significances can be shown with bold font or a raised a.
  dat <- construct_label(dat,
    new_name = "brace_label",
    label_est = "label_est",
    label_se = "label_se",
    label_sig_high = "label_sig_high",
    label_sig_bold = "label_sig_bold",
    round_est = 0,
    round_se = 1
  )

  # Calculate brace coordinates ---------------------------------------------
  brace_coordinates <- calc_brace_coords(dat,
    plot_lims$coords,
    plot_settings = plot_settings
  )


  # Draw braces and labels --------------------------------------------------
  c(
    draw_braces(brace_coordinates, plot_settings),
    draw_brace_label(brace_coordinates, plot_settings) # ,
  )
}


# Utils -------------------------------------------------------------------
draw_braces <- function(dat, plot_settings = plotsettings_lineplot()) {
  if (plot_settings$split_plot == TRUE) {
    res <- ggbrace::stat_brace(
      data = unique(dat[, c("years_Trend", "year_axis", "brace_y")]),
      mapping = ggplot2::aes(
        x = .data$year_axis,
        y = .data$brace_y,
        group = .data$years_Trend
      ),
      rotate = 180,
      linewidth = plot_settings$brace_line_width,
      npoints = 200,
      outside = FALSE
    )
  } else {
    res <- lapply(unique(dat$years_Trend), function(x) {
      dat_year <- unique(dat[dat$years_Trend == x, c("year_start_axis", "year_end_axis", "upper_y", "lower_y", "mid")])
      ggbrace::stat_brace(
        mapping = ggplot2::aes(
          x = c(dat_year$year_start_axis, dat_year$year_end_axis),
          y = c(dat_year$upper_y, dat_year$lower_y),
        ),
        mid = unique(dat_year$mid),
        rotate = 180,
        linewidth = plot_settings$brace_line_width,
        npoints = 200,
        outside = FALSE
      )
    })
  }
  return(res)
}

draw_brace_label <- function(dat, plot_settings = plot_settings()) {
  ggtext::geom_richtext(
    data = dat,
    mapping = ggplot2::aes(
      x = .data$label_pos_x,
      y = .data$label_pos_y,
      label = .data$brace_label
    ),
    colour = "#000000",
    size = plot_settings$brace_label_size,
    label.padding = grid::unit(rep(0, 4), "pt"),
    fill = NA,
    label.color = NA,
    hjust = 1
  )
}
