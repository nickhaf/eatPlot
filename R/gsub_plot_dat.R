#' [gsub()] for lists and data.frames.
#'
#' @param plot_dat List or data.frame, mainly usage intention for plot_dat - objects from [prep_plot()].
#' @param search_for Character string that should be replaced.
#' @param replace_with Character string that will be the replacement.
#'
#' @return Depending on input data.frame or list with the subbed character strings.
#' @export
#'
#' @examples
#'   plot_dat <- list(dat_1 = data.frame(col_1 = c("wholeGroup", "notwholeGroup"),
#'                                       col_2 = c("wholeGroup", NA),
#'                                       col_3 = c(1, 2)),
#'                    dat_2 = data.frame(col_1 = c("wholeGroup", "notwholeGroup"),
#'                                       col_2 = c("wholeGroup", NA),
#'                                       col_3 = c(TRUE, FALSE)))
#' gsub_plot_dat(plot_dat)

gsub_plot_dat <- function(plot_dat, search_for = "wholeGroup", replace_with = "Deutschland") {
  if (inherits(plot_dat, "list")) {
    for (i in names(plot_dat)) {
      plot_dat[[i]][] <- lapply(plot_dat[[i]], function(x) {
        if(is.character(x)){gsub(search_for, replace_with, x)
          }else{x}
      })
    }
  } else if (is.data.frame(plot_dat)) {
    plot_dat[] <- lapply(plot_dat, function(x) {
      if(is.character(x)){gsub(search_for, replace_with, x)
        }else{x}
    })
  } else {
    stop("Please provide a list or a data.frame.")
  }
  return(plot_dat)
}
