#' Extract or remove rows from prepared data.
#'
#' @param plot_data List of data.frames, output of [prep_trend].
#' @param column_name Column that the subsetter will be searched in.
#' @param subsetter Character string of the state you want to extract or remove.
#' @param remove Logical. If true, the state defined in `state` will be removed. Defaults to `FALSE`.
#'
#' @return List of data.frames.
#' @export
#'
#' @examples #tbd
filter_rows <- function(plot_data, column_name, subsetter, remove = FALSE) {
  for (i in c("plot_points", "plot_lines", "plot_braces")) {
    if(remove == FALSE){
      plot_data[[i]] <- plot_data[[i]][plot_data[[i]][, column_name] == subsetter, ]
    }else{
      plot_data[[i]] <- plot_data[[i]][plot_data[[i]][, column_name] != subsetter, ]
    }
  }
  return(plot_data)
}