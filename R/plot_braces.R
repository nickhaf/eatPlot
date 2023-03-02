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
                        y_range,
                        label_est,
                        label_se,
                        label_sig_high,
                        label_sig_bold) {

  # Put calc_dat in plot_braces already.

missing_cols <- check_colnames(c("label_est" = label_est,
                                 "label_se" = label_se,
                                 "label_sig_high" = label_sig_high,
                                 "label_sig_bold" = label_sig_bold),
                               colnames(dat))

for(i in missing_cols){
dat[, i] <- NA
}

# can this be evaled/parsed ...?
dat <- rename_column(dat, label_est, "label_est")
dat <- rename_column(dat, label_se, "label_se")
dat <- rename_column(dat, label_sig_high, "label_sigh_high")
dat <- rename_column(dat, label_sig_bold, "label_sig_bold")



# Construct brace labels --------------------------------------------------
    dat$label_est <- ifelse(dat[, "label_sig_bold"] == TRUE,
                                     paste0("**", round(dat[, "label_est"], 0), "**"),
                                     round(dat[, label_est], 0))
    dat$label_sig <- ifelse(dat[, "label_sig_high"] == TRUE, "<sup>a</sup>", "")
    dat$label_se <- ifelse(is.na(dat$label_se),
                       paste0(" (", round(dat$label_se, 1), ")")
                       , "")

    dat[, c("label_est", "label_sig", "label_se")][is.na(dat[, c("label_est", "label_sig", "label_se")])] <- ""


dat$brace_label <- paste0(
      dat$label_est,
      dat$label_sig,
      dat$label_se
    )

coords <- calc_coords(y_range)
dat <- calc_brace_coords(dat, coords)


  ## Bei calc_coord: neue Spalte ob brace indented sein soll oder nicht
  c(
    draw_braces(dat),
    draw_brace_label(dat),
    # Clip Coordinate system. Necessary, so the brace can be drawn outside of the plot
    ggplot2::coord_cartesian(
      clip = "off",
      ylim = coords #,
      #expand = FALSE
      )
  )
}



## Utils

draw_braces <- function(dat){

  ggbrace::geom_brace(
    data = unique(dat[, c("grouping_var","year", "brace_y")]),
    mapping = ggplot2::aes(
      x = .data$year,
      y = .data$brace_y,
      group = .data$grouping_var
    ),
      #mid = ifelse(coordinates$year_start == min(dat$year_start) & any(dat$overlap == TRUE), 0.25, 0.5),
   #   inherit.data = F,
      rotate = 180,
      linewidth = 0.8,
      npoints = 200
    )

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

calc_pos_label_x <- function(year_start, year_end, brace_indent_pos) {
  year_start + (year_end - year_start) * brace_indent_pos
}
