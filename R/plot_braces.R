#' Plot braces below plot.
#'
#' @param dat Prepared Trend data.
#' @inheritParams plot_lineplot
#' @inheritParams plot_single_lineplot
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples ##
plot_braces <- function(dat,
                        split_plot = FALSE,
                        y_range,
                        label_est = NULL,
                        label_se = NULL,
                        label_sig_high = NULL,
                        label_sig_bold = NULL,
                        nudge_x_axis = 0) {


dat <- build_column(dat, old = label_est, new = "label_est")
dat <- build_column(dat, old = label_se, new = "label_se")
dat <- build_column(dat, old = label_sig_high, new = "label_sig_high")
dat <- build_column(dat, old = label_sig_bold, new = "label_sig_bold")



  # Construct brace labels --------------------------------------------------
  ## Significances can be shown with bold font or a raised a.
  dat$label_est <- ifelse(dat$label_sig_bold == TRUE,
    paste0("**", round(dat$label_est, 0), "**"),
    round(dat$label_est, 0)
  )
  dat$label_sig <- ifelse(dat$label_sig_high == TRUE, "<sup>a</sup>", "")
  dat$label_se <- ifelse(!is.na(dat$label_se),
    paste0(" (", format(round(dat$label_se, 1), trim = TRUE), ")"),
    ""
  )

  dat[, c("label_est", "label_sig", "label_se")][is.na(dat[, c("label_est", "label_sig", "label_se")])] <- ""

  dat$brace_label <- paste0(
    dat$label_est,
    dat$label_sig,
    dat$label_se
  )

  # Calculate brace coordinates ---------------------------------------------
  coords <- calc_coords(y_range)
  if (split_plot == TRUE) {
    dat <- calc_brace_coords(dat, coords, output_format = "long", nudge_x_axis = nudge_x_axis)
  } else {
    dat <- calc_brace_coords(dat, coords, nudge_x_axis = nudge_x_axis)
  }

  c(
    draw_braces(dat, split_plot),
    draw_brace_label(dat),
    set_cartesian_coords(coords)
  )
}




# Utils -------------------------------------------------------------------
draw_braces <- function(dat, split_plot) {
  if (split_plot == TRUE) {
    res <- ggbrace::geom_brace(
      data = unique(dat[, c("trend", "year", "brace_y")]),
      mapping = ggplot2::aes(
        x = .data$year,
        y = .data$brace_y,
        group = .data$trend
      ),
      rotate = 180,
      linewidth = 0.5,
      npoints = 200
    )
  } else {
    res <- lapply(unique(dat$year_start), function(x) {
      dat_year <- unique(dat[dat$year_start == x, c("year_start", "year_end", "upper_y", "lower_y", "mid")])
      ggbrace::geom_brace(
        mapping = ggplot2::aes(
          x = c(dat_year$year_start, dat_year$year_end),
          y = c(dat_year$upper_y, dat_year$lower_y),
        ),
        mid = unique(dat_year$mid),
        rotate = 180,
        linewidth = 0.5,
        npoints = 200
      )
    })
  }
  return(res)
}

draw_brace_label <- function(dat) {
  ggtext::geom_richtext(
    data = dat,
    mapping = ggplot2::aes(
      x = .data$label_pos_x,
      y = .data$label_pos_y,
      label = .data$brace_label
    ),
    size = 2,
    label.padding = grid::unit(rep(0, 4), "pt"),
    fill = NA,
    label.color = NA,
    hjust = 1
  )
}
