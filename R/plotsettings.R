# Checking
check_plotsettings <- function(settings_list) {
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct length. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      length(settings_list) == 22
  )
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct names. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      names(settings_list) %in% c(
        "axis_x_background_colour",
        "axis_x_background_width",
        "axis_x_label_centralize",
        "axis_x_label_nudge_y",
        "axis_x_label_size",
        "brace_label_gap_y",
        "brace_label_nudge_x",
        "brace_label_nudge_y",
        "brace_label_size",
        "brace_line_width",
        "brace_span_y",
        "line_width",
        "margin_bottom",
        "margin_left",
        "margin_right",
        "margin_top",
        "n_cols",
        "point_label_size",
        "point_size",
        "split_plot",
        "split_plot_gap_width",
        "y_axis"
      )
  )

  stopifnot(is_colour(settings_list$axis_x_background_colour))
  stopifnot(is.numeric(settings_list$axis_x_background_width))
  stopifnot(is.numeric(settings_list$axis_x_label_centralize))
  stopifnot(is.numeric(settings_list$axis_x_label_nudge_y))
  stopifnot(is.numeric(settings_list$axis_x_label_size))
  stopifnot(is.numeric(settings_list$brace_label_gap_y))
  stopifnot(is.numeric(settings_list$brace_label_nudge_x))
  stopifnot(is.numeric(settings_list$brace_label_nudge_y))
  stopifnot(is.numeric(settings_list$brace_label_size))
  stopifnot(is.numeric(settings_list$brace_line_width))
  stopifnot(is.numeric(settings_list$brace_span_y))
  stopifnot(is.numeric(settings_list$line_width))
  stopifnot(is.numeric(settings_list$margin_bottom))
  stopifnot(is.numeric(settings_list$margin_left))
  stopifnot(is.numeric(settings_list$margin_right))
  stopifnot(is.numeric(settings_list$margin_top))
  stopifnot(is.numeric(settings_list$n_cols) & settings_list$n_cols %% 1 == 0) # check for whole number
  stopifnot(is.numeric(settings_list$point_label_size))
  stopifnot(is.numeric(settings_list$point_size))
  stopifnot(is.logical(settings_list$split_plot))
  stopifnot(is.numeric(settings_list$split_plot_gap_width))
  stopifnot(is.logical(settings_list$y_axis))
}


#' Set parameters for the lineplots.
#'
#' @param axis_x_background_colour Colour value of the x-axis background.
#' @param axis_x_background_width Numeric. The background space will be increased, if this parameter is increased.
#' @param axis_x_label_centralize Numeric. The x-axis labels will be nudged into the center by this amount, if the plot is a split lineplot.
#' @param axis_x_label_nudge_y Numeric for shifting the x-axis labels vertically. Increase to lower the x-axis labels.
#' @param axis_x_label_size Numeric for the font size of the x-axis labels.
#' @param brace_label_gap_y Numeric for the size of the vertical gap between brace labels.
#' @param brace_label_nudge_x Numeric. The brace labels will be shifted along the x-axis by this amount. Increase to shift the labels further to the right.
#' @param brace_label_nudge_y Numeric. The brace labels will be shifted along the y-axis by this amount. Increase to let the labels start further below.
#' @param brace_label_size Numeric fontsize of the bracelabels.
#' @param brace_line_width Numeric for the thickness of the brace.
#' @param brace_span_y Numeric for the width of the brace on the y-axis.
#' @param line_width Numeric for the thicknes of the plotted lines.
#' @param margin_bottom,margin_left,margin_right,margin_top Numeric for the area around the plot. See [ggplot2::theme()].
#' @param n_cols Numeric, indicating how many columns of smaller plots the final lineplot should have.
#' @param point_label_size Numeric for the fontsize of the pointlabels.
#' @param point_size Numeric for the size of plotted points.
#' @param split_plot Logical, indicating whether the different trends should be split or not.
#' @param split_plot_gap_width Numeric wor the width of the gap in a split plot in npc.
#' @param y_axis Logical, indicating whether a y-axis should be plotted to the left of each row or not.
#' @param default_list Named list with predefined settings. Defaults to a list with all settings set to `0`.
#'
#' @return A named list with settings for the lineplots.
#' @export
#'
#' @examples
#' plotsettings(n_cols = 2, axis_x_label_centralize = 0.1)
plotsettings <- function(axis_x_background_colour = NULL,
                         axis_x_background_width = NULL,
                         axis_x_label_centralize = NULL,
                         axis_x_label_nudge_y = NULL,
                         axis_x_label_size = NULL,
                         brace_label_gap_y = NULL,
                         brace_label_nudge_x = NULL,
                         brace_label_size = NULL,
                         brace_label_nudge_y = NULL,
                         brace_line_width = NULL,
                         brace_span_y = NULL,
                         line_width = NULL,
                         margin_bottom = NULL,
                         margin_left = NULL,
                         margin_right = NULL,
                         margin_top = NULL,
                         n_cols = NULL,
                         point_label_size = NULL,
                         point_size = NULL,
                         split_plot = NULL,
                         split_plot_gap_width = NULL,
                         y_axis = NULL,
                         default_list = NULL) {
  ## Build a list with sensible defaults if no default is provided
  if (is.null(default_list)) {
    plot_settings <- list(
      "axis_x_background_colour" = "lightgrey",
      "axis_x_background_width" = 0.04,
      "axis_x_label_centralize" = 0,
      "axis_x_label_nudge_y" = 0.02,
      "axis_x_label_size" = 2,
      "brace_label_gap_y" = 0.08,
      "brace_label_nudge_x" = 0,
      "brace_label_nudge_y" = 0.05,
      "brace_label_size" = 2,
      "brace_line_width" = 0.5,
      "brace_span_y" = 0.1,
      "line_width" = 1,
      "margin_bottom" = 0,
      "margin_left" = 0,
      "margin_right" = 0,
      "margin_top" = 0,
      "n_cols" = 1,
      "point_label_size" = 2,
      "point_size" = 1,
      "split_plot" = FALSE,
      "split_plot_gap_width" = 0,
      "y_axis" = FALSE
    )
  } else {
    plot_settings <- default_list
  }

  check_plotsettings(plot_settings)

  ## Update the default list with all values that are != NULL
  for (i in names(plot_settings)) {
    value <- get(i)
    if (!is.null(value)) {
      plot_settings[[i]] <- value
    }
  }

  return(plot_settings)
}
