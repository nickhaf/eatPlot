# Checking
check_plotsettings_barplot <- function(settings_list) {
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct length. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      length(settings_list) == 1
  )
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct names. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      names(settings_list) %in% c(
"background_stripes_colour"
      )
  )

  stopifnot(all(is_colour(settings_list$background_stripes_colour)))

}


#' Set parameters for the barplots.
#'
#' @param background_stripes_colour Named vector containing the two colours that should be used for the striped background.
#'
#' @return A named list with settings for the barplots.
#' @export
#'
#' @examples
#' plotsettings_barplot()
#'
plotsettings_barplot <- function(background_stripes_colour = NULL,
                                 default_list = NULL) {
  ## Build a list with sensible defaults if no default is provided
  if (is.null(default_list)) {
    plot_settings <- list(
    "background_stripes_colour" = c("white", "white")
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
