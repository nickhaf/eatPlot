#' Plot a lineplot.
#'
#' @param plot_dat Input is a list prepared by [prep_plot()].`
#' @param seperate_plot_var Character string of the column containing the tiles. For every unique value, a new tile will be plotted. Defaults to `state_var`.
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

  # filter years ------------------------------------------------------------
  ## Eigene Funktion:
  plot_dat <- filter_plot_years(plot_dat, years_lines, years_braces)

  states <- unique(plot_dat[[1]]$state_var)
  tiles <- unique(plot_dat$plot_lines[, seperate_plot_var]) # Hier die Level nehmen

  plot_list <- list()
  if (!is.null(point_values)) {
    range_est <- range(plot_dat[["plot_points"]][, point_values], na.rm = TRUE)
  } else {
    stop("Please provide point-values.")
  }
  position <- 1

  for (i in tiles) {
    plot_dat_tile <- filter_rows(plot_dat, column_name = seperate_plot_var, subsetter = i)
    if (seperate_plot_var == "competence_var") {
      plot_dat_tile[["plot_background_lines"]] <- plot_dat_tile[["plot_background_lines"]][plot_dat_tile[["plot_background_lines"]]$competence_var == i, ]
    }

    p_state <- ggplot2::ggplot() +
      plot_single_lineplot(
        plot_dat = plot_dat_tile,
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
     plot_title(i, title_superscripts) +
          set_plot_coords(plot_dat,
        point_values = point_values,
        plot_settings = plot_settings
      )

    ## The wholeGroup plot gets a box drawn around it.
    # if (i == "wholeGroup") {
    #   p_state <- p_state +
    #     ggplot2::theme(plot.background = ggplot2::element_rect(color = "black", linewidth = 0.5, fill = NA))
    # }

    plot_list[[i]] <- p_state
    position <- position + 1
  }

  # Add y axis --------------------------------------------------------------
  if (plot_settings$y_axis == TRUE) {
    y_axis_plot <- ggplot2::ggplot() +
      plot_y_axis(plot_dat)

    positions_y_axis <- calc_y_positions(states, plot_settings$n_cols)

    for (i in positions_y_axis) {
      plot_list <- append(plot_list, list(y_axis_plot), after = i - 1)
    }

    widths_setting <- c(0.02, rep(1 - 0.02 / plot_settings$n_cols, times = plot_settings$n_cols))
    plot_settings$n_cols <- plot_settings$n_cols + 1
  } else {
    widths_setting <- 1 / plot_settings$n_cols
  }

  margin_bottom <- plot_settings$margin_bottom + 0.006 * (length(levels(plot_dat[["plot_braces"]]$grouping_var)) - 1) # more brace labels need more space

  ## Build the finished plot:
  patchwork::wrap_plots(plot_list, ncol = plot_settings$n_cols, widths = widths_setting) &
    ggplot2::theme(
      plot.margin = ggplot2::unit(
        c(
          plot_settings$margin_top,
          plot_settings$margin_right,
          margin_bottom,
          plot_settings$margin_left
        ),
        "npc"
      ) # t, r, b, l
    )
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
  if (is.null(years_lines)) {
    lineplot_years <- consecutive_numbers(c(plot_dat[["plot_lines"]]$year_start, plot_dat[["plot_lines"]]$year_end))
  } else {
    lineplot_years <- years_lines
  }

  if (is.null(years_braces)) {
    ## Draw braces from last year to every other year
    plot_years <- unique(c(plot_dat[["plot_braces"]]$year_start, plot_dat[["plot_braces"]]$year_end))
    braceplot_years <- lapply(plot_years[-which(plot_years == max(plot_years))], function(x) {
      c(x, max(plot_years))
    })
  } else {
    braceplot_years <- years_braces
  }

  plot_dat[["plot_lines"]] <- plot_dat[["plot_lines"]][filter_years(plot_dat[["plot_lines"]], lineplot_years), ]
  plot_dat[["plot_braces"]] <- plot_dat[["plot_braces"]][filter_years(plot_dat[["plot_braces"]], braceplot_years), ]
  plot_dat[["plot_background_lines"]] <- plot_dat[["plot_background_lines"]][filter_years(plot_dat[["plot_background_lines"]], lineplot_years), ]
  plot_dat[["plot_points"]] <- plot_dat[["plot_points"]][plot_dat[["plot_points"]]$years_Trend %in% c(unique(plot_dat$plot_lines$years_Trend), unique(plot_dat$plot_braces$years_Trend)), ]

  return(plot_dat)
}


plot_title <- function(title, title_raised_letter){
  if(!is.null(title_raised_letter)){
   pos <- which(title == names(title_raised_letter))
   if(length(pos) == 0){
     ggplot2::labs(title = title)
   }else{
   superscript <- title_raised_letter[[pos]]
   ggplot2::labs(title = bquote(.(title)^.(superscript)))
   }
  }else{
    ggplot2::labs(title = title)
  }
}
