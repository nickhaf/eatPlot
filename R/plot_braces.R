#' Plot braces below plot.
#'
#' @param dat Prepared Trend data.
#' @param BL Bundesland, for which the labels should be drawn.
#' @inheritParams plot_lineplot
#' @inheritParams plot_single_lineplot
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples ##
plot_braces <- function(dat,
                        y_range,
                        BL,
                        label_est,
                        label_se,
                        label_sig_high,
                        label_sig_bold) {

  # rename label columns to the arguments.
  # Also make this function possible, if no labels are provided. For this, paste together the labels in the data preperation for this function.
  # Put calc_brace_coords in plot_braces already.

  if(label_sig_bold %in% colnames(brace_coords)){
    brace_coords <- rename_column()
    brace_coords$label_est <- ifelse(brace_coords[, label_sig_bold] == TRUE,
                                     paste0("**", round(brace_coords[, label_est], 0), "**"),
                                     round(brace_coords[, label_est], 0))
  }else{brace_coords$label_est <- ""}

  if(label_sig_high %in% colnames(brace_coords)){
    brace_coords$label_sig <- ifelse(brace_coords[, label_sig_high] == TRUE, "<sup>a</sup>", "")}else{brace_coords$label_sig <- ""}

  if(label_se %in% colnames(brace_coords)){
    brace_coords$label_se <- brace_coords[, label_se]}else{brace_coords$label_se <- ""}


  coords <- calc_coords(y_range)
  brace_coords <- calc_brace_coords(dat, coords)


  ## Bei calc_coord: neue Spalte ob brace indented sein soll oder nicht
  c(
    draw_braces(brace_coords),
    draw_brace_label(brace_coords, BL),
    # Clip Coordinate system. Necessary, so the brace can be drawn outside of the plot
    ggplot2::coord_cartesian(
      clip = "off",
      ylim = coords #,
      #expand = FALSE
      )
  )
}



## Utils

draw_braces <- function(brace_coords){

  ggbrace::geom_brace(
    data = unique(brace_coords[, c("grouping_var","year", "brace_y")]),
    mapping = ggplot2::aes(
      x = .data$year,
      y = .data$brace_y,
      group = .data$grouping_var
    ),
      #mid = ifelse(coordinates$year_start == min(brace_coords$year_start) & any(brace_coords$overlap == TRUE), 0.25, 0.5),
   #   inherit.data = F,
      rotate = 180,
      linewidth = 0.8,
      npoints = 200
    )

}

draw_brace_label <- function(brace_coords) {

  ggtext::geom_richtext(
    data = brace_coords,
    mapping = ggplot2::aes(
      x = .data$label_pos_x,
      y = .data$label_pos_y,
      label = paste0(
        .data$label_est,
        .data$label_sig,
        " (", round(.data$label_se, 1), ")"
      )
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
