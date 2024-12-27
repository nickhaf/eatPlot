#' Plot a lineplot.
#'
#' @param plot_dat Input is a list prepared by `prep_plot()`.
#' @param seperate_plot_var_box Character vector, containing strings from the `seperate_plot_var`-column, that should get a box drawn around them.
#' @param point_values Character string of the column name in `plot_dat[["plot_points"]]` containing the y-values for the plotted points. Defaults to `est_noTrend_noComp`.
#' @param point_sig Character string of the column name containing significance values for `point_values`. Defaults to `"sig_noTrend_Comp_crossDiff_wholeGroup"`.
#' @param line_se Character vector of the column name containing the standard errors for the plotted lines. Defaults to `NULL`, in which case they will be deducted from the line values.
#' @param line_sig Character string of the column name containing significance values for `line_values`. Defaults to `"sig_Trend_noComp"`, which will show the significance of the difference between two time points.
#' @param label_est Character string of the column name containing the brace labels.
#' @param label_se Character string of the column name containing the standard errors for `label_est`. Will be put in bracktes behind `label_est`.
#' @param label_sig_high Character string of the column name containing significance values for `label_est`. Significant values will be marked by a raised 'a'. Normally, should be the comparison of the trend vs. the trend in whole Germany, which can be found in the trendDiff_cross parameter. Defaults to `NULL`, as this parameter is not always provided.
#' @param label_sig_bold Character string of the column name containing significance values for `label_est`. Significant values will be marked as bold. Defaults to `"sig_Trend_noComp"`.
#' @param title_superscripts Named list for superscripts at the plot_titles. The name of the list element has to be equal to the title, the value of the list element has to be the superscript. Defaults to `NULL`.
#' @param years_lines  List of numeric vectors containing the start and end year, between which a trend line should be plotted. Per default, lines are drawn from every year to the next consecutive year.
#' @param years_braces List of numeric vectors containing the start and end year, between which a brace should be plotted. Per default, braces are drawn from the last year to every other year included in the data.
#' @param plot_settings Named list constructed with `plotsettings_lineplot()`. Defaults to a list with all settings set to `0`. There are several predefined lists with optimized settings for different plots. See `plotsettings_lineplot()` for an overview.
#' @return [ggplot2] object.
#' @export
#'
#' @examples # tbd
plot_lineplot <- function(eatPlot_dat,
                          point_est,
                          point_sig,
                          line_est,
                          line_sig,
                          line_se,
                          brace_label_est,
                          brace_label_sig,
                          brace_label_se,
                          brace_label_sig_high,
                          brace_label_sig_bold,
                          years_lines,
                          years_braces,
                          subgroup_var,
                          facet_var = "TR_BUNDESLAND",
                          background_facet = "total",
                          background_subgroup = "total",
                          seperate_plot_var_box = "wholeGroup",
                          title_superscripts = NULL,
                          plot_settings = plotsettings_lineplot()) {
  # Check ----------------------------------------------------------------
  check_plotsettings_lineplot(plot_settings)
  check_columns(eatPlot_dat,
    cols = c(facet_var)
  )
  eatPlot_dat <- check_facets(eatPlot_dat, facet_var)

  if(plot_settings$split_plot){
    warning("Split lineplot currently not supported. Set to non-split.")
    plot_settings$split_plot <- FALSE
  }

  # Rename/Build needed columns ---------------------------------------------
  years_list <- prep_years_list(years_lines, years_braces)
  # plot_dat <- equalize_line_length(plot_dat, plot_settings)


  eatPlot_dat <- eatPlot_dat |>
    build_column(old = point_est, new = "point_est") |>
    build_column(old = point_sig, new = "point_sig") |>
    build_column(old = line_sig, new = "line_sig") |>
    build_column(old = line_est, new = "line_est") |>
    build_column(old = brace_label_est, new = "brace_label_est") |>
    build_column(old = brace_label_se, new = "brace_label_se") |>
    build_column(old = brace_label_sig_high, new = "brace_label_sig_high") |>
    build_column(old = brace_label_sig_bold, new = "brace_label_sig_bold") |>
    build_column(old = facet_var, new = "facet_var") |>
    build_column(old = subgroup_var, new = "subgroup_var") |>
    build_column(old = line_se, new = "line_se")

  # Calculate Coordinates ---------------------------------------------------
  plot_lims <- calc_plot_lims(eatPlot_dat, years_list, background_subgroup, plot_settings)

  # Prepare Subsets ---------------------------------------------------------
  background_line_dat <- subset(eatPlot_dat,
                                facet_var == background_facet &
                                  subgroup_var == background_subgroup)
  background_line_dat <- filter_years(background_line_dat, years = years_lines)

  line_dat <- subset(eatPlot_dat,
                     facet_var != background_facet &
                       subgroup_var != background_subgroup)
  line_dat <- filter_years(line_dat, years = years_lines)


  brace_dat_list <- prep_brace(eatPlot_dat, plot_lims, plot_settings)
  brace_dat <- brace_dat_list$brace_dat |>
    subset(facet_var != background_facet & subgroup_var != background_subgroup)
  brace_coordinates <- brace_dat_list$brace_coords
  brace_dat <- filter_years(brace_dat, years = years_braces)

  if (!checkmate::test_subset(vapply(years_lines, paste0, collapse = "_", FUN.VALUE = character(1)), choices = line_dat$trend)) {
    stop("Some of the trends you provided in 'years_lines' are not in the data.")
  }

  if (!checkmate::test_subset(vapply(years_braces, paste0, collapse = "_", FUN.VALUE = character(1)), choices = line_dat$trend)) {
    stop("Some of the trends you provided in 'years_braces' are not in the data.")
  }

  dat_p <- list(plot_dat = line_dat, brace_dat = brace_dat_list, background_line_dat = background_line_dat, plot_lims = plot_lims, plot_settings = plot_settings)

  plot_list <- list()
  position <- 1
  facet_values <- levels(dat_p$plot_dat[, "facet_var"])[levels(dat_p$plot_dat[, "facet_var"]) != background_facet]

  for (i in facet_values) {

    dat_p_facet <- dat_p
    dat_p_facet$plot_dat <- dat_p_facet$plot_dat[!is.na(dat_p_facet$plot_dat[, "facet_var"]), ]
    dat_p_facet$plot_dat <- dat_p_facet$plot_dat[dat_p_facet$plot_dat[, "facet_var"] == i & !is.na(dat_p_facet$plot_dat[, "facet_var"]), ]


    dat_p_facet$brace_dat$brace_label <- dat_p_facet$brace_dat$brace_label[!is.na(dat_p_facet$brace_dat$brace_label[, "facet_var"]), ]
    dat_p_facet$brace_dat$brace_label <- dat_p_facet$brace_dat$brace_label[dat_p_facet$brace_dat$brace_label[, "facet_var"] == i & !is.na(dat_p_facet$brace_dat$brace_label[, "facet_var"]), ]
    p_state <- ggplot2::ggplot(dat_p_facet$plot_dat,
      mapping = ggplot2::aes(
        x = year,
        y = point_est,
        group = id,
        colour = .data$subgroup_var
      )
    ) +
      plot_single_lineplot(dat_p_facet) +
      plot_title(sub_dash(i), title_superscripts) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(
        dat_p$plot_settings$margin_top,
        dat_p$plot_settings$margin_right,
        dat_p$plot_settings$margin_bottom,
        dat_p$plot_settings$margin_left
      ), "npc"))

    plot_list[[i]] <- p_state
    position <- position + 1
  }

  names(plot_list) <- facet_values

  # Add y axis as seperate plot --------------------------------------------------------------
  if (dat_p$plot_settings$axis_y == TRUE) {
    y_axis_plot <- ggplot2::ggplot() +
      plot_y_axis(
        dat_p_facet,
        plot_settings = dat_p$plot_settings
      )

    positions_y_axis <- calc_y_positions(facet_values, dat_p$plot_settings$n_cols)

    for (i in positions_y_axis) {
      plot_list <- append(plot_list, list(y_axis_plot), after = i - 1)
    }

    widths_setting <- c(0.02, rep(1 - 0.02 / dat_p$plot_settings$n_cols, times = dat_p$plot_settings$n_cols))
    dat_p$plot_settings$n_cols <- dat_p$plot_settings$n_cols + 1
  } else {
    widths_setting <- 1 / dat_p$plot_settings$n_cols
  }

  ## The wholeGroup plot gets a box drawn around it.
  for (plot_names in names(plot_list)) {
    if (plot_names %in% seperate_plot_var_box) {
      plot_list[[plot_names]] <- plot_list[[plot_names]] +
        ggplot2::theme(plot.background = ggplot2::element_rect(
          color = "black",
          linewidth = dat_p$plot_settings$seperate_plot_var_box_linewidth,
          fill = NA
        ))
    }
  }

  ## Build the finished plot:
  patchwork::wrap_plots(plot_list,
    ncol = dat_p$plot_settings$n_cols,
    widths = widths_setting
  ) +
    patchwork::plot_annotation(theme = ggplot2::theme(plot.margin = ggplot2::margin(0.0017, 0.0017, 0.0017, 0.0017, "npc"))) # keep small margin, so the box isn't cut off
}


# Utils -------------------------------------------------------------------
plot_title <- function(title, title_superscript) {
  if (!is.null(title_superscript)) {
    names(title_superscript) <- sapply(names(title_superscript), sub_dash)
    pos <- which(title == names(title_superscript))
    if (length(pos) == 0) {
      ggplot2::labs(title = title)
    } else {
      superscript <- title_superscript[[pos]]
      ggplot2::labs(title = bquote(.(title)^.(superscript)))
    }
  } else {
    ggplot2::labs(title = title)
  }
}


equalize_line_length <- function(plot_dat, plot_settings) {
  ## To plot the values with equal distance, a new y-axis is needed:
  if (plot_settings$equal_trend_line_length == TRUE) {
    plot_dat$plot_points$year_axis <- as.numeric(factor(plot_dat$plot_points$year))
    sub_years <- extract_gsub_values(plot_dat)
  } else {
    plot_dat$plot_points$year_axis <- plot_dat$plot_points$year
  }

  if (plot_settings$background_lines == TRUE) {
    loop_objects <- names(plot_dat)[names(plot_dat) %in% c("plot_lines", "plot_braces", "plot_background_lines")]
  } else {
    loop_objects <- names(plot_dat)[names(plot_dat) %in% c("plot_lines", "plot_braces")]
  }

  for (i in loop_objects) {
    plot_dat[[i]]$year_start_axis <- plot_dat[[i]]$year_start
    plot_dat[[i]]$year_end_axis <- plot_dat[[i]]$year_end

    if (plot_settings$equal_trend_line_length == TRUE) {
      for (j in seq_along(sub_years$year)) {
        plot_dat[[i]]$year_start_axis <- gsub(sub_years$year[j], sub_years$year_axis[j], plot_dat[[i]]$year_start_axis)
        plot_dat[[i]]$year_end_axis <- gsub(sub_years$year[j], sub_years$year_axis[j], plot_dat[[i]]$year_end_axis)
      }

      plot_dat[[i]]$year_start_axis <- as.numeric(plot_dat[[i]]$year_start_axis)
      plot_dat[[i]]$year_end_axis <- as.numeric(plot_dat[[i]]$year_end_axis)
    }
  }

  return(plot_dat)
}

extract_gsub_values <- function(plot_dat) {
  sub_years <- plot_dat$plot_points[plot_dat$plot_points$year != plot_dat$plot_points$year_axis, c("year", "year_axis")]
  sub_years <- stats::na.omit(sub_years)
  return(sub_years)
}



calc_plot_lims <- function(plot_dat, years_list, background_subgroup, plot_settings) {
  check_columns(plot_dat, c("facet_var", "point_est", "year"))

  ## Axis limits can be set manually. If not, calculate by range.
  if (is.null(plot_settings$axis_y_lims)) {
    y_range <- range(plot_dat$point_est, na.rm = TRUE)
    coords <- calc_y_value_coords(y_range)
  }
  # else {
  #   y_range <- range(seq_over(
  #     from = plot_settings$axis_y_lims[1],
  #     to = plot_settings$axis_y_lims[2],
  #     by = plot_settings$axis_y_tick_distance
  #   ))
  #   coords <- calc_y_value_coords(y_range, nudge_param_lower = 0, nudge_param_upper = 0.3) # In this case, the brace starts at the lowest provided value, and the upper value is reduced.
  # }
  subgroup_lvls <- unique(plot_dat$subgroup_var[plot_dat$subgroup_var != background_subgroup])

  brace_coords <- calc_brace_coords(
    subgroup_lvls,
    coords,
    years_list,
    plot_settings = plot_settings
  )

  unique_years <- unique(unlist(lapply(years_list, function(df) unlist(df))))
  x_range <- range(unique_years)

  y_lims_total <- c(min(brace_coords$group_labels$label_pos_y) - diff(range(coords)) * 0.06, max(coords))

  coord_list <- list(
    x_range = x_range,
    y_range = y_range,
    y_lims_total = y_lims_total,
    coords = coords,
    brace_coords = brace_coords
  )

  return(coord_list)
}


prep_years <- function(years) {
  years_df <- data.frame(do.call("rbind", years))
  colnames(years_df) <- c("year_start", "year_end")
  return(years_df)
}
