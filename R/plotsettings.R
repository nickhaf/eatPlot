# Checking
check_plotsettings <- function(settings_list) {
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct length. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      length(settings_list) == 5
  )
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct names. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      names(settings_list) %in% c("n_cols",
                                  "nudge_brace_labels_x",
                                  "nudge_x_axis",
                                  "split_plot",
                                  "y_axis")
  )

  stopifnot(is.numeric(settings_list$n_cols) & settings_list$n_cols %% 1 == 0) # check for whole number
  stopifnot(is.numeric(settings_list$nudge_brace_labels_x))
  stopifnot(is.numeric(settings_list$nudge_x_axis))
  stopifnot(is.logical(settings_list$split_plot))
  stopifnot(is.logical(settings_list$y_axis))


}


#' Set parameters for the lineplots.
#'
#' @param n_cols Numeric, indicating how many columns of smaller plots the final lineplot should have.
#' @param nudeg_brace_labels_x Numeric. The brace labels will be shifted along the x-axis by this amount.
#' @param nudge_x_axis Numeric. The x-axis labels will be nudged into the center by this amount, if the plot is a split lineplot.
#' @param split_plot Logical, indicating whether the different trends should be split or not.
#' @param y_axis Logical, indicating whether a y-axis should be plotted to the left of each row or not.
#' @param default_list Named list with predefined settings. Defaults to a list with all settings set to `0`.
#'
#' @return A named list with settings for the lineplots.
#' @export
#'
#' @examples
#' plotsettings(n_cols = 2, nudge_x_axis = 0.1)
plotsettings <- function(...,
                         margin_bottom,
                         margin_top,
                         n_cols = NULL,
                         nudge_brace_labels_x = NULL,
                         nudge_x_axis = NULL,
                         split_plot = NULL,
                         y_axis = NULL,
                         default_list = NULL) {

  ## Build a list with sensible defaults if no default is provided
  if (is.null(default_list)) {
    plot_settings <- list(
      "n_cols" = 1,
      "nudge_brace_labels_x" = 0,
      "nudge_x_axis" = 0,
      "split_plot" = FALSE,
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
