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
#' @param background_lines Logical, indicating whether the whole group trend should be plotted in the background.
#' @param plot_settings Named list constructed with `plotsettings_lineplot()`. Defaults to a list with all settings set to `0`. There are several predefined lists with optimized settings for different plots. See `plotsettings_lineplot()` for an overview.
#' @return [ggplot2] object.
#' @export
#'
#' @examples # tbd
plot_lineplot <- function(plot_dat,
                          seperate_plot_var = "state_var",
                          seperate_plot_var_order = NULL,
                          seperate_plot_var_box = "wholeGroup",
                          point_values = "est_noTrend_noComp",
                          point_sig = "sig_noTrend_Comp_crossDiff_wholeGroup",
                          line_values = c("est_noTrendStart_noComp", "est_noTrendEnd_noComp"),
                          line_sig = "sig_Trend_noComp",
                          label_est = "est_Trend_noComp",
                          label_se = "se_Trend_noComp",
                          label_sig_high = NULL,
                          label_sig_bold = "sig_Trend_noComp",
                          line_se = NULL,
                          title_superscripts = NULL,
                          years_lines = NULL,
                          years_braces = NULL,
                          background_lines = TRUE,
                          plot_settings = plotsettings_lineplot()) {
  stopifnot(all(sapply(years_lines, is.numeric)) | is.null(years_lines))
  stopifnot(all(sapply(years_braces, is.numeric)) | is.null(years_braces))
  stopifnot(inherits(title_superscripts, "list") | is.null(title_superscripts))

  check_plotsettings_lineplot(plot_settings)

  if (!is.null(years_lines)) {
    if (any(!unique(unlist(years_lines)) %in% plot_dat$plot_points$year)) {
      stop("Please check your years_lines argument. Are the years included in your data?")
    }
  }

  if (!is.null(years_braces)) {
    if (any(!unique(unlist(years_braces)) %in% plot_dat$plot_points$year)) {
      stop("Please check your years_braces argument. Are the years included in your data?")
    }
  }

  if (is.null(years_lines)) {
    years_lines <- consecutive_numbers(c(plot_dat[["plot_lines"]]$year_start, plot_dat[["plot_lines"]]$year_end))
  }

  plot_dat <- lapply(plot_dat, function(x) {
    x$grouping_var <- as.factor(x$grouping_var)
    x$grouping_var <- droplevels(x$grouping_var)
    return(x)
  })

  ### prepare seperate_plot_var
  plot_dat$plot_lines$seperate_plot_var <- plot_dat$plot_lines[, seperate_plot_var]

  if (!is.null(seperate_plot_var_order)) {
    if (!all(plot_dat$plot_lines$seperate_plot_var %in% seperate_plot_var_order)) {
      stop("Please provide all unique elements in the seperate_plot_var-column of your data in the seperate_plot_var_order - argument.")
    }
    if (any(!(seperate_plot_var_order %in% plot_dat$plot_lines$seperate_plot_var))) {
      stop("At least one element in seperate_plot_var_order is not part of your seperate_plot_var - column.")
    }
  }

  # filter years ------------------------------------------------------------
  plot_dat <- filter_plot_years(plot_dat, years_lines, years_braces)

  plot_dat <- equalize_line_length(plot_dat, plot_settings)


  states <- unique(plot_dat[[1]]$state_var)

  if (!is.null(seperate_plot_var_order)) {
    tiles <- seperate_plot_var_order
  } else {
    tiles <- unique(plot_dat$plot_lines$seperate_plot_var)
  }

  plot_list <- list()


  plot_lims <- calc_plot_lims(plot_dat, point_values, plot_settings)

  position <- 1

  for (i in tiles) {
    plot_dat_tile <- filter_rows(plot_dat, column_name = seperate_plot_var, subsetter = i)
    if (seperate_plot_var == "competence_var") {
      plot_dat_tile[["plot_background_lines"]] <- plot_dat_tile[["plot_background_lines"]][plot_dat_tile[["plot_background_lines"]]$competence_var == i, ]
    }

    p_state <- ggplot2::ggplot() +
      plot_single_lineplot(
        plot_dat = plot_dat_tile,
        x_range = range(plot_dat$plot_points$year),
        y_range = range_est,
        point_values = point_values,
        point_sig = point_sig,
        line_values = line_values,
        line_se = line_se,
        line_sig = line_sig,
        label_est = label_est,
        label_se = label_se,
        label_sig_high = label_sig_high,
        label_sig_bold = label_sig_bold,
        background_lines = background_lines,
        plot_settings = plot_settings
      ) +
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

  names(plot_list) <- tiles

  # Add y axis --------------------------------------------------------------
  if (plot_settings$axis_y == TRUE) {
    y_axis_plot <- ggplot2::ggplot() +
      plot_y_axis(plot_dat,
        point_values = point_values,
        plot_settings = plot_settings
      ) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(
        plot_settings$margin_top,
        0,
        plot_settings$margin_bottom,
        0
      ), "npc"))

    positions_y_axis <- calc_y_positions(states, plot_settings$n_cols)

    for (i in positions_y_axis) {
      plot_list <- append(plot_list, list(y_axis_plot), after = i - 1)
    }

    widths_setting <- c(0.02, rep(1 - 0.02 / plot_settings$n_cols, times = plot_settings$n_cols))
    plot_settings$n_cols <- plot_settings$n_cols + 1
  } else {
    widths_setting <- 1 / plot_settings$n_cols
  }


  # Adjust plot margins according to position ------------------------------
  # plot_list <- set_plot_margins(plot_list, plot_settings)

  # The wholeGroup plot gets a box drawn around it.
  for (plot_names in names(plot_list)) {
    if (plot_names %in% seperate_plot_var_box) {
      plot_list[[plot_names]] <- plot_list[[plot_names]] +
        ggplot2::theme(plot.background = ggplot2::element_rect(
          color = "black",
          linewidth = 1,
          fill = NA
        ))
    }
  }

  # margin_bottom <- plot_settings$margin_bottom + 0.006 * (length(levels(plot_dat[["plot_braces"]]$grouping_var)) - 1) # more brace labels need more space

  ## Build the finished plot:
  patchwork::wrap_plots(plot_list,
    ncol = plot_settings$n_cols,
    widths = widths_setting
  ) +
    # ggplot2::theme(
    #   plot.margin = ggplot2::unit(
    #     c(
    #       plot_settings$margin_top,
    #       plot_settings$margin_right,
    #       plot_settings$margin_bottom,
    #       plot_settings$margin_left
    #     ),
    #     "npc"
    #   ) # t, r, b, l
    # ) +
    patchwork::plot_annotation(theme = ggplot2::theme(plot.margin = ggplot2::margin(0.0017, 0.0017, 0.0017, 0.0017, "npc"))) # keep small margin, so the box isn't cut off
}


# Utils -------------------------------------------------------------------
# Return rows with respective start and end years.
filter_years <- function(dat, year_list) {
  # Filter the respective rows
  year_rows <- unlist(lapply(year_list, function(x) {
    which(dat$year_start == x[1] & dat$year_end == x[2])
  }))
  return(year_rows)
}


filter_plot_years <- function(plot_dat, years_lines = NULL, years_braces = NULL) {
  if (is.null(years_braces)) {
    ## Draw braces from last year to every other year
    plot_years <- unique(c(plot_dat[["plot_braces"]]$year_start, plot_dat[["plot_braces"]]$year_end))
    braceplot_years <- lapply(plot_years[-which(plot_years == max(plot_years))], function(x) {
      c(x, max(plot_years))
    })
  } else {
    braceplot_years <- years_braces
  }

  plot_dat[["plot_lines"]] <- plot_dat[["plot_lines"]][filter_years(plot_dat[["plot_lines"]], years_lines), ]
  plot_dat[["plot_braces"]] <- plot_dat[["plot_braces"]][filter_years(plot_dat[["plot_braces"]], braceplot_years), ]
  plot_dat[["plot_background_lines"]] <- plot_dat[["plot_background_lines"]][filter_years(plot_dat[["plot_background_lines"]], years_lines), ]
  plot_dat[["plot_points"]] <- plot_dat[["plot_points"]][plot_dat[["plot_points"]]$years_Trend %in% c(unique(plot_dat$plot_lines$years_Trend), unique(plot_dat$plot_braces$years_Trend)), ]

  return(plot_dat)
}


plot_title <- function(title, title_raised_letter) {
  if (!is.null(title_raised_letter)) {
    names(title_raised_letter) <- sapply(names(title_raised_letter), sub_dash)
    pos <- which(title == names(title_raised_letter))
    if (length(pos) == 0) {
      ggplot2::labs(title = title)
    } else {
      superscript <- title_raised_letter[[pos]]
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

  loop_objects <- names(plot_dat)[names(plot_dat) %in% c("plot_lines", "plot_braces", "plot_background_lines")]

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



set_plot_margins <- function(plot_list, plot_settings) {
  n_rows <- ceiling(length(plot_list) / plot_settings$n_cols)

  for (j in 1:length(plot_list)) {
    # Only one row ------------------------------------------------------------
    if (n_rows == 1) {
      # upper left --------------------------------------------------------------
      if (j == 1) {
        plot_list[[j]] <- plot_list[[j]] +
          ggplot2::theme(plot.margin = ggplot2::unit(c(
            plot_settings$margin_top,
            plot_settings$margin_right,
            0,
            0
          ), "npc"))
      }


      # Middle in upper row -----------------------------------------------------
      if (j > 1 & j < plot_settings$n_cols) {
        plot_list[[j]] <- plot_list[[j]] +
          ggplot2::theme(plot.margin = ggplot2::unit(c(
            plot_settings$margin_top,
            plot_settings$margin_right,
            0,
            plot_settings$margin_left
          ), "npc"))
      }

      # Upper right -------------------------------------------------------------
      if (j == plot_settings$n_cols) {
        plot_list[[j]] <- plot_list[[j]] +
          ggplot2::theme(plot.margin = ggplot2::unit(c(
            plot_settings$margin_top,
            0,
            0,
            plot_settings$margin_left
          ), "npc"))
      }
      return(plot_list)
    }



    # multiple rows -----------------------------------------------------------
    # upper left --------------------------------------------------------------
    if (j == 1) {
      plot_list[[j]] <- plot_list[[j]] +
        ggplot2::theme(plot.margin = ggplot2::unit(c(
          plot_settings$margin_top,
          plot_settings$margin_right,
          plot_settings$margin_bottom,
          0
        ), "npc"))
    }

    # Middle in upper row -----------------------------------------------------
    if (j > 1 & j < plot_settings$n_cols) {
      plot_list[[j]] <- plot_list[[j]] +
        ggplot2::theme(plot.margin = ggplot2::unit(c(
          plot_settings$margin_top,
          plot_settings$margin_right,
          plot_settings$margin_bottom,
          plot_settings$margin_left
        ), "npc"))
    }



    # Upper right -------------------------------------------------------------
    if (j == plot_settings$n_cols) {
      plot_list[[j]] <- plot_list[[j]] +
        ggplot2::theme(plot.margin = ggplot2::unit(c(
          plot_settings$margin_top,
          0,
          plot_settings$margin_bottom,
          plot_settings$margin_left
        ), "npc"))
    }

    # Middle in left column -------------------------------------------------------------
    if (check_middle_left(j, plot_settings$n_cols, n_rows)) {
      plot_list[[j]] <- plot_list[[j]] +
        ggplot2::theme(plot.margin = ggplot2::unit(c(
          plot_settings$margin_top,
          plot_settings$margin_right,
          plot_settings$margin_bottom,
          plot_settings$margin_left
        ), "npc"))
    }


    # Middle middle columns -----------------------------------------------------------
    if (check_middle_middle(j, plot_settings$n_cols, n_rows)) {
      plot_list[[j]] <- plot_list[[j]] +
        ggplot2::theme(plot.margin = ggplot2::unit(c(
          plot_settings$margin_top,
          plot_settings$margin_right,
          plot_settings$margin_bottom,
          plot_settings$margin_top
        ), "npc"))
    }

    # Middle right columns ------------------------------------------------------------
    if (j %% plot_settings$n_cols == 0 & j != plot_settings$n_cols & j != length(plot_list)) {
      plot_list[[j]] <- plot_list[[j]] +
        ggplot2::theme(plot.margin = ggplot2::unit(c(
          plot_settings$margin_top,
          plot_settings$margin_right,
          plot_settings$margin_bottom,
          plot_settings$margin_left
        ), "npc"))
    }


    # Bottom left -------------------------------------------------------------
    if (j == (length(plot_list) - plot_settings$n_cols) + 1) {
      plot_list[[j]] <- plot_list[[j]] +
        ggplot2::theme(plot.margin = ggplot2::unit(c(
          plot_settings$margin_top,
          plot_settings$margin_right,
          0,
          plot_settings$margin_left
        ), "npc"))
    }


    # Bottom middle -----------------------------------------------------------
    if (j > ((length(plot_list) - plot_settings$n_cols) + 1) & j < length(plot_list)) {
      plot_list[[j]] <- plot_list[[j]] +
        ggplot2::theme(plot.margin = ggplot2::unit(c(
          plot_settings$margin_top,
          plot_settings$margin_right,
          0,
          plot_settings$margin_left
        ), "npc"))
    }



    # Bottom right ------------------------------------------------------------
    if (j == length(plot_list)) {
      plot_list[[j]] <- plot_list[[j]] +
        ggplot2::theme(plot.margin = ggplot2::unit(c(
          plot_settings$margin_top,
          plot_settings$margin_right,
          0,
          plot_settings$margin_left
        ), "npc"))
    }
  }
  return(plot_list)
}
## Für y-axis bereits bei der Achse setzen
## Margin einmal für between und einmal für untereinander

## If y_axis: abstand nach rechts etwas kleiner

check_middle_left <- function(j, n_cols, n_rows) {
  j > 1 &
    ceiling(j / n_cols) < n_rows & # not in last row
    (j - 1) %% n_cols == 0 # one after the last column
}

check_middle_middle <- function(j, n_cols, n_rows) {
  ceiling(j / n_cols) < n_rows &
    ceiling(j / n_cols) > 1 & # not in first row
    (j - 1) %% n_cols != 0 & # not first column
    j %% n_cols != 0 # not last column
}



#' Calculate different plot limit values.
#'
#' @param plot_dat Plot_dat object, put out bei [prep_plot()].
#' @param point_values Character string of the column name of the column used for value axis.
#'
#' @return List containing the following elements:
#' * `range_y`: Minimum and maximum of the values in `point_values`.
#' * `y_lims_total`: Minimum and maximum value of the plot.
#' * `coords`: Y-value of the first brace start, and heighest y-value of the plot.
#' @examples
calc_plot_lims <- function(plot_dat, point_values, plot_settings) {
  if (is.null(plot_settings$axis_y_lims)) {
    if (!is.null(point_values)) {
      range_y <- range(plot_dat[["plot_points"]][, point_values], na.rm = TRUE)
      coords <- calc_y_value_coords(y_range)
    } else {
      stop("Please provide point-values.")
    }
  } else {
    range_y <- plot_settings$axis_y_lims
    coords <- calc_y_value_coords(y_range, nudge_param_lower = 0) # In this case, the brace starts at the lowest provided value.
  }

  y_lims_total <- calc_plot_lims_y(plot_dat$plot_braces,
    coords,
    plot_settings = plot_settings
  )

  coord_list <- list(
    range_y = range_y,
    y_lims_total = y_lims_total,
    coords = coords
  )
  return(coords_list)
}
