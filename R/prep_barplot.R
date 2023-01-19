#' Prepare data for plotting a barplot.
#'
#' @param data Output from \code{eatRep}.
#' @param sub_groups sub_groups which should be plotted in the same plot tile. E.g. you can distinguish between adjusted and not adjusted means.
#' @param sig_niveau At which significance niveau do p-values become significant?
#'
#' @return Returns a \code{data.frame} which can be used as input for \code{plot_bar}.
#' @export
#'
#' @examples # tbd
prep_barplot <- function(data, sub_groups, sig_niveau) {
  colnames(data)[colnames(data) == sub_groups] <- "sub_groups"
  data <- calc_sig(data, sig_niveau)
  data <- calc_fill(data)

  data <- subset(data, data$parameter == "mean" & !is.na(data$parameter) &
    data$comparison == "crossDiff" & !is.na(data$comparison))
}


## Helpers

calc_fill <- function(data) {
  if (any(is.na(data$sub_groups))) {
    stop("Your subgroups should not contain any missings. Please check your input data.")
  } else {
    data$fill <- paste0(data$sub_groups, "_", data$sig)
    return(data)
  }
}
