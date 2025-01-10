#' Extract or remove rows from prepared data.
#'
#' @inheritParams filter_plot_dat
#' @param plot_dat List of data.frames, output of [prep_lineplot].
#' @param column_name Column that the subsetter will be searched in.
#' @param subsetter Character string of the state you want to extract or remove.
#' @param remove Logical. If `TRUE`, the state defined in `state` will be removed. Defaults to `FALSE`.
#' @param remove_na Logical. If `TRUE`, rows with `NAs` in `column_name` will be removed.
#'
#' @return List of data.frames.
#' @export
#'
#' @examples # tbd
filter_rows <- function(plot_dat, column_name, subsetter, list_elements = c("plot_points", "plot_lines", "plot_braces"), remove = FALSE, remove_na = FALSE) {

  if(any(!list_elements %in% names(plot_dat))){
    stop("Some of your list_elements are not part of plot_dat.", call. = FALSE)
  }

  for (i in list_elements) {
    if(!column_name %in% colnames(plot_dat[[i]])){
      stop(paste0("Your column_name '", column_name, "' is not part of the sublist '", i, "' of your plot_dat."), call. = FALSE)
    }

    if(remove_na == TRUE){
      plot_dat[[i]] <- plot_dat[[i]][!is.na(plot_dat[[i]][, column_name]), ]
    }

    if (remove == FALSE) {
      plot_dat[[i]] <- plot_dat[[i]][plot_dat[[i]][, column_name] == subsetter & !is.na(plot_dat[[i]][, column_name]), ]
    } else {
      plot_dat[[i]] <- plot_dat[[i]][plot_dat[[i]][, column_name] != subsetter | is.na(plot_dat[[i]][, column_name]), ]
    }
  }
  return(plot_dat)
}
