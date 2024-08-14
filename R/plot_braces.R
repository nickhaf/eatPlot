prep_brace <- function(plot_dat, brace_coords){

  plot_dat_BL <- plot_dat %>%
    dplyr::filter(TR_BUNDESLAND == "Berlin")



  brace_coords$coord_dat_test1 <- brace_coords$coord_dat %>%
    dplyr::mutate(trend = paste0(.$year_start, "_", .$year_end)) %>%
    tidyr::pivot_longer(cols = c("upper_y", "lower_y"),
                        values_to = "y") %>%
    tidyr::pivot_longer(cols = c("year_start", "year_end"),
                        values_to = "year",
                        names_to = "year_type") %>%
    dplyr::mutate(year = as.character(year))


  brace_labels <- merge(plot_dat_BL[, c("id", "est_comp", "mhg", "trend")],
                        brace_coords$group_labels,
                        by.x = "mhg",
                        by.y = "grouping_lvls",
                        all.x = TRUE)


  # Construct brace labels --------------------------------------------------
  ## Significances can be shown with bold font or a raised a.
  # dat <- construct_label(
  #   dat,
  #   new_name = "brace_label",
  #   label_est = "label_est",
  #   label_se = "label_se",
  #   label_sig_high = "label_sig_high",
  #   label_sig_bold = "label_sig_bold",
  #   round_est = 0,
  #   round_se = 1
  # )

  brace_coords$coord_dat_test <- merge(brace_coords$coord_dat_test1,
                                       brace_labels,
                                       by = "trend",
                                       all.x = TRUE) %>%
    dplyr::mutate(label_pos_x = as.character(label_pos_x))


  return(brace_coords$coord_dat_test)
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
    outside = FALSE
  )

  #}
  return(res)
}

draw_brace_label <- function(brace_coords, plot_settings = plot_settings()) {
  ggtext::geom_richtext(
    data = brace_coords,
    mapping = ggplot2::aes(
      x = .data$label_pos_x,
      y = .data$label_pos_y,
      label = .data$est_comp
    ),
    colour = "#000000",
    size = plot_settings$brace_label_size,
    label.padding = grid::unit(rep(0, 4), "pt"),
    fill = NA,
    label.color = NA,
    hjust = 1
  )
}
