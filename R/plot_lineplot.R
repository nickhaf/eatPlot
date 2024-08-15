#' Plot a lineplot.
#'
#' @param plot_dat Input is a list prepared by [prep_plot()].`
#' @param seperate_plot_var Character string of the column containing the tiles. For every unique value, a new tile will be plotted. Defaults to `state_var`.
#' @param seperate_plot_var_order Character vector containing all unique elements in the `seperate_plot_var` column. The lineplot-tiles will be ordered according to the order of this vector.
#' @param seperate_plot_var_box Character vector, containing strings from the `seperate_plot_var`-column, that should get a box drawn around them.
#' @param point_values Character string of the column name in `plot_dat[["plot_points"]]` containing the y-values for the plotted points. Defaults to `est_noTrend_noComp`.
#' @param point_sig Character string of the column name containing significance values for `point_values`. Defaults to `"sig_noTrend_Comp_crossDiff_wholeGroup"`.
#' @param line_values Character vector with two elements. Column names in `plot_dat[["plot_lines"]]` containing the y-values for the plotted lines. Defaults to `c("est_noTrendStart_noComp", "est_noTrendEnd_noComp")`. If set to `NULL`, no lines will be plotted.
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
plot_lineplot <- function(eatRep_dat,
                          grouping_var = NULL,
                          parameter = "mean",
                          line_sig = "trend",
                          years_lines = list(c(2009, 2015), c(2015, 2022)),
                          years_braces = list(c(2009, 2015), c(2015, 2022)),
                          facets = "TR_BUNDESLAND",
                          # seperate_plot_var_order = NULL,
                          # seperate_plot_var_box = "wholeGroup",
                          # point_values = "est_noTrend_noComp",
                          # point_sig = "sig_noTrend_Comp_crossDiff_wholeGroup",
                          # line_values = c("est_noTrendStart_noComp", "est_noTrendEnd_noComp"),
                          brace_label_est = "trend",
                          # brace_label_se = "se_Trend_noComp",
                          # brace_label_sig_high = NULL,
                          # brace_label_sig_bold = "sig_Trend_noComp",
                          # line_se = NULL,
                          title_superscripts = NULL,
                          plot_settings = plotsettings_lineplot()) {

  check_eatRep_dat(eatRep_dat)
  check_plotsettings_lineplot(plot_settings)

  dat_p <- prep_lineplot(eatRep_dat,
                         line_sig = line_sig,
                         parameter = parameter,
                         brace_label_est = brace_label_est)


  years_lines_df <- data.frame(do.call("rbind", years_lines))
  years_braces_df <- data.frame(do.call("rbind", years_braces))

  ## calc overlap, coords, brace_coords, calc_brace_position here directly, put together into a coord_list and give into necessary functions.

  colnames(years_lines_df) <- c("year_start", "year_end")
  colnames(years_braces_df) <- c("year_start", "year_end")

  ## What happens, if no years are provided?
  plot_settings_expanded <- plot_settings
  plot_settings_expanded$years_list <- list("years_lines" = years_lines_df,
                                            "years_braces" = years_braces_df)


  dat_p$dat_final <- filter_years(dat_p$dat_final, l = years_lines, b = years_braces)
  dat_p$dat_final <- dat_p$dat_final %>%
    dplyr::filter(mhg %in% c("ersteGen", "einET"))

  #plot_dat <- equalize_line_length(plot_dat, plot_settings)




  ## Give out warning if seperate plot var is not an ordered factor. In this case, sort alphabetically. Provide guidance on how to achieve that.
  # dat_p$dat_final[, facets] <- check_facets(dat_p$dat_final[, facets])
  ## Wie filtere ich dann richtig? da gabs irgendwie ein Problem

  plot_single_lineplot(dat_p, plot_settings)

  plot_list <- list()
  position <- 1

  facet_values <- unique(dat_p$dat_final[, facets])[!is.na(unique(dat_p$dat_final[, facets]))]
i = "Berlin"

  for (i in facet_values) {

    dat_p_facet <- dat_p
    dat_p_facet$dat_final <- dat_p_facet$dat_final[!is.na(dat_p_facet$dat_final[, facets]), ]

    dat_p_facet$dat_final <- dat_p_facet$dat_final[dat_p_facet$dat_final[, facets] == i & !is.na(dat_p_facet$dat_final[, facets]), ]

    p_state <- plot_single_lineplot(dat_p_facet, plot_settings) +
      plot_title(sub_dash(i), title_superscripts) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(
        plot_settings$margin_top,
        plot_settings$margin_right,
        plot_settings$margin_bottom,
        plot_settings$margin_left
      ), "npc"))

    plot_list[[i]] <- p_state
    position <- position + 1
  }

  names(plot_list) <- facet_values

  # # Add y axis --------------------------------------------------------------
  # if (plot_settings$axis_y == TRUE) {
  #   y_axis_plot <- ggplot2::ggplot() +
  #     plot_y_axis(
  #       plot_dat,
  #       plot_lims = plot_lims,
  #       plot_settings = plot_settings
  #     ) +
  #     ggplot2::theme(plot.margin = ggplot2::unit(c(
  #       plot_settings$margin_top,
  #       0,
  #       plot_settings$margin_bottom,
  #       0
  #     ), "npc"))
  #
  #   positions_y_axis <- calc_y_positions(tiles, plot_settings$n_cols)
  #
  #   for (i in positions_y_axis) {
  #     plot_list <- append(plot_list, list(y_axis_plot), after = i - 1)
  #   }
  #
  #   widths_setting <- c(0.02, rep(1 - 0.02 / plot_settings$n_cols, times = plot_settings$n_cols))
  #   plot_settings$n_cols <- plot_settings$n_cols + 1
  # } else {
  #   widths_setting <- 1 / plot_settings$n_cols
  # }
  #
  # ## The wholeGroup plot gets a box drawn around it.
  # for (plot_names in names(plot_list)) {
  #   if (plot_names %in% seperate_plot_var_box) {
  #     plot_list[[plot_names]] <- plot_list[[plot_names]] +
  #       ggplot2::theme(plot.background = ggplot2::element_rect(
  #         color = "black",
  #         linewidth = plot_settings$seperate_plot_var_box_linewidth,
  #         fill = NA
  #       ))
  #   }
  # }

  ## Build the finished plot:
  patchwork::wrap_plots(plot_list,
    ncol = plot_settings$n_cols#,
    #widths = widths_setting
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


#' Calculate different plot limit values.
#'
#' @inheritParams plot_lineplot
#'
#' @return List containing the following elements:
#' * `range_y`: Minimum and maximum of the values in `point_values`.
#' * `y_lims_total`: Minimum and maximum value of the plot.
#' * `coords`: Y-value of the first brace start, and heighest y-value of the plot.
#' @examples # tbd
calc_plot_lims <- function(plot_dat, plot_settings) {


  ## Axis limits can be set manually. If not, calculate by range.
  if (is.null(plot_settings$axis_y_lims)) {

    y_range <- range(plot_dat$est_point, na.rm = TRUE)

    ## This increases the coords by a nudging parameter:
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

  ## And what does this do extra?:
  # Okay, this seems to calculate the brace positions again (will happen later on too), so the plot limits can be set.

  # Better: Calc the brace positions once. Putt the lowest value in here, to define the minimum y value
#
#   y_lims_total <- calc_plot_lims_y(
#     plot_dat$plot_braces,
#     coords,
#     plot_settings = plot_settings
#   )

  ## Why need this? calc if necessary!

  x_range <- range(plot_dat$year)


  ## Output an object with the plot coordinate informations:
  coord_list <- list(
    x_range = x_range,
    y_range = y_range,
    # y_lims_total = y_lims_total,
    coords = coords
  )
  return(coord_list)
}
