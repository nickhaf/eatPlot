prep_brace <- function(plot_dat, brace_coords){

    brace_coords$coord_dat_test1 <- brace_coords$coord_dat %>%
    dplyr::mutate(trend = paste0(.$year_start, "_", .$year_end)) %>%
    tidyr::pivot_longer(cols = c("upper_y", "lower_y"),
                        values_to = "y") %>%
    tidyr::pivot_longer(cols = c("year_start", "year_end"),
                        values_to = "year",
                        names_to = "year_type")

  brace_labels <- merge(plot_dat[, c("TR_BUNDESLAND", "id", "brace_label_est", "brace_label_se", "brace_label_sig_high", "brace_label_sig_bold", "mhg", "trend")],
                        brace_coords$group_labels,
                        by.x = "mhg",
                        by.y = "grouping_lvls",
                        all.x = TRUE)

  # Construct brace labels --------------------------------------------------
  # Significances can be shown with bold font or a raised a.

  brace_labels$brace_label <- construct_label(
    brace_labels,
    column_est = "brace_label_est",
    column_se = "brace_label_se",
    column_sig_bold = "brace_label_sig_bold",
    column_sig_superscript = "brace_label_sig_high",
    sig_superscript_letter = "a",
    round_est = 0,
    round_se = 1
  )

  brace_dat <- merge(brace_coords$coord_dat_test1,
                                       brace_labels,
                                       by = "trend",
                                       all.x = TRUE)


  return(brace_dat)
}


draw_braces <- function(brace_coords, plot_settings = plotsettings_lineplot()) {

  # if (plot_settings$split_plot == TRUE) {
  #   res <- ggbrace::stat_brace(
  #     data = unique(dat[, c("years_Trend", "year_axis", "brace_y")]),
  #     mapping = ggplot2::aes(
  #       x = .data$year_axis,
  #       y = .data$brace_y,
  #       group = .data$years_Trend
  #     ),
  #     rotate = 180,
  #     linewidth = plot_settings$brace_line_width,
  #     npoints = 200,
  #     outside = FALSE
  #   )
  # } else {

  res <- ggbrace::stat_brace(
    data = brace_coords,
    mapping = ggplot2::aes(
      x = year,
      y = y,
      group = trend
    ),
    mid = brace_coords$brace_position_x,
    rotate = 180,
    linewidth = plot_settings$brace_line_width,
    npoints = 200,
    outside = FALSE,
    colour = "black"
  )

  #}
  return(res)
}

draw_brace_label <- function(brace_coords, plot_settings = plot_settings()) {
  ggtext::geom_richtext(
    data = unique(brace_coords[, c("label_pos_x", "label_pos_y", "brace_label")]),
    mapping = ggplot2::aes(
      x = .data$label_pos_x,
      y = .data$label_pos_y,
      label = .data$brace_label,
      group = NA
    ),
    colour = "black",
    size = plot_settings$brace_label_size,
    label.padding = grid::unit(rep(0, 4), "pt"),
    fill = NA,
    label.color = NA,
    hjust = 1
  )
}
