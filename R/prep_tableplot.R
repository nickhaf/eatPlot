
#' Prepare data to plot as table.
#' @param data Output from \code{eatRep}.
#' @param columns Column in data that contains the different columns that should be plottet.
#' @param competence Competence that should be plottet.
#' @param sig_niveau At which significance niveau do p-values become significant?
#'
#' @return  Returns a \code{data.frame} which can be used as input for \code{plot_table}.
#' @export
#'
#' @examples # tbd
prep_tableplot <- function(data, columns, competence, sig_niveau = 0.05){

  data <- adjusted_means
  colnames(data)[colnames(data) == columns] <- "columns"
  data <- calc_sig(data)
  data <- data[data$group != "wholeGroup" & data$kb == competence, ]
  ## crossdiff p with and without adjustment in NA p at the respective place
  data <- data[!is.na(data$comparison), ] #lookout: later we need the p values from data$comparison == NA

  data <- data[ , c("group", "est" ,"columns")]

  groups <- unique(data$group)
  data <- rbind(data, data.frame(group = groups, est = rep(NA, length(groups)), columns = rep("y_label", length(groups))))
  colnames(data) <- c("group", "value", "x_labels")
  data$x_labels <- factor(data$x_labels, levels = c("y_label", unique(data$x_labels[data$x_labels != "y_label"])),  ordered = TRUE)

  data$value <- as.character(round(data$value))
  return(data)

}
