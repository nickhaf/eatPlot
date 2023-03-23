#' Extract or remove states from prepared data.
#'
#' @param plot_data List of data.frames, output of [prep_trend].
#' @param state Character string of the state you want to extract or remove.
#' @param remove Logical. If true, the state defined in `state` will be removed. Defaults to `FALSE`.
#'
#' @return List of data.frames.
#' @export
#'
#' @examples #tbd
get_state <- function(plot_data, state, remove = FALSE) {
  for (i in c("plot_points", "plot_lines", "plot_braces")) {
    if(remove == FALSE){
      plot_data[[i]] <- plot_data[[i]][plot_data[[i]]$state_var == state, ]
    }else{
      plot_data[[i]] <- plot_data[[i]][plot_data[[i]]$state_var != state, ]
    }
  }
  return(plot_data)
}
