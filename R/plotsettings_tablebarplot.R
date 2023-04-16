# Checking
check_plotsettings_barplot <- function(settings_list) {
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct length. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      length(settings_list) == 10
  )
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct names. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      names(settings_list) %in% c(
        "axis_x_lims",
        "background_stripes_colour",
        "bar_fill_colour",
        "bar_frame_linetype",
        "bar_pattern_fill_colour",
        "bar_pattern_type",
        "bar_sig_type",
        "bar_width",
        "headers_nudge_y",
        "columns_width"
      )
  )

  stopifnot(is.numeric(settings_list$axis_x_lims) & length(is.numeric(settings_list$axis_x_lims)) == 2 | is.null(settings_list$axis_x_lims))
  stopifnot(all(is_colour(settings_list$background_stripes_colour)))
  stopifnot(all(is_colour(settings_list$bar_fill_colour)))
  stopifnot(is.character(settings_list$bar_frame_linetype))
  stopifnot(all(is_colour(settings_list$bar_pattern_fill_colour)))
  stopifnot(is.character(settings_list$bar_pattern_type))
  stopifnot(settings_list$bar_sig_type %in% c("pattern", "frame"))
  stopifnot(is.numeric(settings_list$bar_width))
  stopifnot(is.numeric(settings_list$headers_nudge_y))
  stopifnot(is.numeric(settings_list$columns_width) | is.null(settings_list$columns_width))
}


#' Set parameters for the barplots.
#'
#' @param axis_x_lims Numeric vector of length `2` for the x-axis limits. Will be set automatically, if `NULL`.
#' @param background_stripes_colour Named vector containing the two colours that should be used for the striped background.
#' @param bar_fill_colour Colour of the bar filling. Can be either one colour for all bars, or a named vector with the names of the groups specified in `bar_fill`.
#' @param bar_frame_linetype Named vector with the bar frame linetypes.
#' @param bar_pattern_fill_colour Named vector with the filling colours for the bar pattern. Names of the vector must be found in the column specified in `bar_pattern_fill`.
#' @param bar_pattern_type Named vector with the pattern types for the barpattern.
#' @param bar_sig_type Character string indicating whether levels of the grouping variable should be visualized by pattern fill ("pattern") or line type ("frame").
#' @param bar_width Numeric between `0` and `1` specifying the width of the bar.
#' @param headers_nudge_y Numeric between `-1` and `1` to nudge the column_headers in y dircetion.
#' @param columns_width Numeric vector with the length equal to the number of ploted table columns. Adjusts the column width.
#' @param default_list Named list with predefined settings. Defaults to a list with all settings set to `0`.
#'
#' @return A named list with settings for the barplots.
#' @export
#'
#' @examples
#' plotsettings_tablebarplot()
#'
plotsettings_tablebarplot <- function(axis_x_lims = NULL,
                                      background_stripes_colour = NULL,
                                      bar_fill_colour = NULL,
                                      bar_frame_linetype = NULL,
                                      bar_pattern_fill_colour = NULL,
                                      bar_pattern_type = NULL,
                                      bar_sig_type = NULL,
                                      bar_width = NULL,
                                      headers_nudge_y = NULL,
                                      columns_width = NULL,
                                      default_list = NULL) {
  ## Build a list with sensible defaults if no default is provided
  if (is.null(default_list)) {
    plot_settings <- list(
      "axis_x_lims" = NULL,
      "background_stripes_colour" = "white",
      "bar_fill_colour" = "white",
      "bar_frame_linetype" = "solid",
      "bar_pattern_fill_colour" = "white",
      "bar_pattern_type" = "none",
      "bar_sig_type" = "frame",
      "bar_width" = 0.4,
      "headers_nudge_y" = 0,
      "columns_width" = NULL
    )
  } else {
    plot_settings <- default_list
  }

  check_plotsettings_barplot(plot_settings)

  ## Update the default list with all values that are != NULL
  for (i in names(plot_settings)) {
    value <- get(i)
    if (!is.null(value)) {
      plot_settings[[i]] <- value
    }
  }
  return(plot_settings)
}
