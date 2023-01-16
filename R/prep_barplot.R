#' Prepare data for plotting a barplot.
#'
#' @param data Output from \code{}
#' @param sub_groups sub_groups which should be plotted in the same plot tile. E.g. you can distinguish between adjusted and not adjusted means.
#' @param sig_niveau At which significance niveau do p-values become significant?
#'
#' @return Returns a \code{data.frame} which can be used as input for \code{plot_bar}.
#' @export
#'
#' @examples #tbd
prep_barplot <- function(data, sub_groups, sig_niveau){

  colnames(data)[colnames(data) == sub_groups] <- "sub_groups"

  if(any(is.na(data$p))){
    stop("Your p-values should not contain any missings. Please check your input data.")
  }else{
  data$sig <- as.factor(ifelse(data$p < sig_niveau & !is.na(data$p), TRUE, FALSE))
  }

  if(any(is.na(data$sub_groups))){
    stop("Your subgroups should not contain any missings. Please check your input data.")
  }else{
  data$fill <- paste0(data$sub_groups, "_", data$sig)
  }

  data <- subset(data, data$parameter == "mean" & !is.na(data$parameter) &
                       data$comparison == "crossDiff" & !is.na(data$comparison))

}

## Reshape to wide, damit nur ein Datensatz bleibt (p_crossDiff....)
