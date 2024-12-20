prep_brace <- function(plot_dat, plot_lims, plot_settings) {
  check_columns(plot_dat, c("facet_var", "id", "brace_label_est", "brace_label_se", "brace_label_sig_high", "brace_label_sig_bold", "subgroup_var", "trend"))

  plot_lims$brace_coords$coord_dat_test1 <- plot_lims$brace_coords$coord_dat %>%
    dplyr::mutate(trend = paste0(.$year_start, "_", .$year_end)) %>%
    tidyr::pivot_longer(
      cols = c("upper_y", "lower_y"),
      values_to = "y"
    ) %>%
    tidyr::pivot_longer(
      cols = c("year_start", "year_end"),
      values_to = "year",
      names_to = "year_type"
    )

  brace_labels <- merge(plot_dat[, c("facet_var", "id", "brace_label_est", "brace_label_se", "brace_label_sig_high", "brace_label_sig_bold", "subgroup_var", "trend")],
    plot_lims$brace_coords$group_labels,
    by.x = "subgroup_var",
    by.y = "grouping_lvls",
    all.x = TRUE
  )
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

  brace_labels_merged <- merge(unique(brace_labels[ , c("trend", "subgroup_var", "facet_var", "brace_label", "label_pos_y")]),
                        unique(plot_lims$brace_coords$coord_dat_test1[ ,c("trend", "label_pos_x")]),
                        by = "trend",
                        all.y = TRUE)

brace_list <- list(brace_dat=plot_lims$brace_coords$coord_dat_test1,
                   brace_label = brace_labels_merged,
                   brace_coords=plot_lims$brace_coords)

  return(brace_list)
}


draw_braces <- function(brace_coords, plot_settings = plotsettings_lineplot()) {
  check_columns(brace_coords, c("year", "y", "trend", "brace_position_x"))

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
  res <- lapply(unique(brace_coords$trend), function(x) {
    dat <- brace_coords[brace_coords$trend == x, ]

    ggbrace::stat_brace(
    data = dat,
    mapping = ggplot2::aes(
      x = year,
      y = y,
      group = trend
    ),
    mid = unique(dat$brace_position_x),
    rotate = 180,
    linewidth = plot_settings$brace_line_width,
    npoints = 200,
    outside = FALSE,
    colour = "black"
  )
  })

  # }
  return(res)
}

draw_brace_label <- function(brace_label, plot_settings = plot_settings()) {
    check_columns(brace_label, c("label_pos_x", "label_pos_y", "brace_label"))
    ggtext::geom_richtext(
    data = unique(brace_label[, c("label_pos_x", "label_pos_y", "brace_label")]),
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
