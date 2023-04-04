# Checking
check_plotsettings <- function(settings_list) {
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct length. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      length(settings_list) == 2
  )
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct names. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      names(settings_list) %in% c("n_cols", "nudge_x_axis")
  )

  stopifnot(is.numeric(settings_list$n_cols) & settings_list$n_cols %% 1 == 0) # check for whole number
  stopifnot(is.numeric(settings_list$nudge_x_axis))
}


#' Set parameters for the lineplots.
#'
#' @param n_cols Numeric, indicating how many columns of smaller plots the final lineplot should have. Defaults to `4`.
#' @param nudge_x_axis Numeric. The x-axis labels will be nudged into the center by this amount, if the plot is a split lineplot. Defaults to `0.4`.
#' @param default_list Named list with predefined settings. Defaults to a list with all settings set to `0`.
#'
#' @return A named list with settings for the lineplots.
#' @export
#'
#' @examples
#' plotsettings(n_cols = 2, nudge_x_axis = 0.1)
plotsettings <- function(n_cols = NULL, nudge_x_axis = NULL, default_list = NULL) {

  ## Build a list with sensible defaults if no default is provided
  if (is.null(default_list)) {
    plot_settings <- list(
      "n_cols" = 1,
      "nudge_x_axis" = 0
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
