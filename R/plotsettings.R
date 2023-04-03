
# constructor
new_plotsettings <- function(n_cols = 1, nudge_x_axis = 0) {
  list(
    "n_cols" = n_cols,
    "nudge_x_axis" = nudge_x_axis
  )
}



# Checking
check_plotsettings <- function(settings_list) {
  stopifnot(
    "The object provided for the 'default' argument does not have the correct length. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      length(settings_list) == 2
  )
  stopifnot(
    "The object provided for the 'default' argument does not have the correct names. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      names(settings_list) %in% c("n_cols", "nudge_x_axis")
  )

  stopifnot(is.numeric(settings_list$n_cols) & settings_list$n_cols %% 1 == 0) # check for whole number
  stopifnot(is.numeric(settings_list$nudge_x_axis))
}


#' Set parameters for the lineplots.
#'
#' @param n_cols Numeric, indicating how many columns of smaller plots the final lineplot should have. Defaults to `4`.
#' @param nudge_x_axis Numeric. The x-axis labels will be nudged into the center by this amount, if the plot is a split lineplot. Defaults to `0.4`.
#' @param default Named list with predefined settings. Defaults to a list with all settings set to `0`.
#'
#' @return A named list with settings for the lineplots.
#' @export
#'
#' @examples
#' plotsettings(n_cols = 2, nudge_x_axis = 0.1)
plotsettings <- function(n_cols = NULL, nudge_x_axis = NULL, default = new_plotsettings()) {
  check_plotsettings(default)

  plot_settings <- default

  if (!is.null(n_cols)) plot_settings$n_cols <- n_cols
  if (!is.null(nudge_x_axis)) plot_settings$nudge_x_axis <- nudge_x_axis


  check_plotsettings(plot_settings)

  return(plot_settings)
}
