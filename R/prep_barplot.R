#' Prepare data for plotting a barplot.
#'
#' @param data Output from \code{}
#' @param subGroups Subgroups which should be plotted in the same plot tile. E.g. you can distinguish between adjusted and not adjusted means.
#' @param sigNiveau At which significance niveau do p-values become significant?
#'
#' @return Returns a \code{data.frame} which can be used as input for \code{plot_bar}.
#' @export
#'
#' @examples #tbd
prep_barplot <- function(data, subGroups, sigNiveau){

  data$subGroups <- data[, subGroups]
  data$sig <- factor(ifelse(data$p < sigNiveau, TRUE, FALSE)) # Alternativer name: pattern
  data$fill <- paste0(data[ , subGroups], "_", data$sig)

  data <- data[data$comparison == "crossDiff", ]

}
