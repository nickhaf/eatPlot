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
                        label_est,
                        label_se,
                        label_sig_high,
                        label_sig_bold) {
  col_vec <- c(
    "label_est" = label_est,
    "label_se" = label_se,
    "label_sig_high" = label_sig_high,
    "label_sig_bold" = label_sig_bold
  )
  missing_cols <- check_colnames(
    x = col_vec,
    colnames(dat)
  )

  for (i in missing_cols) {
    dat[, i] <- NA
  }

  col_names <- c("label_est", "label_se", "label_sig_high", "label_sig_bold")
  for (i in col_names) {
    dat[, i] <- dat[, eval(parse(text = i))]
  }

  # Construct brace labels --------------------------------------------------
  ## Significances can be shown with bold font or a raised a.
  dat$label_est <- ifelse(dat$label_sig_bold == TRUE,
    paste0("**", round(dat$label_est, 0), "**"),
    round(dat$label_est, 0)
  )
  dat$label_sig <- ifelse(dat$label_sig_high == TRUE, "<sup>a</sup>", "")
  dat$label_se <- ifelse(!is.na(dat$label_se),
    paste0(" (", round(dat$label_se, 1), ")"),
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
    dat <- calc_brace_coords(dat, coords, output_format = "long")
  } else {
    dat <- calc_brace_coords(dat, coords)
  }

  c(
    draw_braces(dat, split_plot),
    draw_brace_label(dat),
    ggplot2::coord_cartesian(
      clip = "off", # Clip Coordinate system. Necessary, so the brace can be drawn under the x-axis.
      ylim = coords
    )
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
      linewidth = 0.8,
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
        linewidth = 0.8,
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
    size = 3,
    label.padding = grid::unit(rep(0, 4), "pt"),
    fill = NA,
    label.color = NA
  )
}
