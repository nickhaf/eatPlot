#' Fill in and rename necessary columns
#'
#' `clean_data()` performs some simple data wrangling to prepare the input `data.frame` for the more plot specific data transformations.
#'
#' The following operations are performed on `data`:
#' * For consistency, points in column names are subbed with underscores.
#' * Only rows containing `mean` in `data$parameter` are returend.
#' * The group `wholeGroup` currently contains `NAs` in the columns `data$TR_BUNDESLAND` and `data$grouping_var`. These are subbed with `wholeGroup` or `noGroup` respectively.
#' * The column `data$TR_BUNDESLAND` is filled up by extracting the first state found in `data$groups` for the respective row.
#' @inheritParams prep_trend
#'
#' @return `clean_data()` returns a `data` with renamend columns and filled in `NAs`.
#' @export
#'
#' @examples #tbd
clean_data <- function(data, grouping_var, competence){

  if (grouping_var == "") {
    data$grouping_var <- rep(NA, nrow(data))
  } else {
    colnames(data)[colnames(data) == grouping_var] <- "grouping_var"
  }

  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]
  data$TR_BUNDESLAND <- write_group(data$group, groups = BLs)
  data[is.na(data$TR_BUNDESLAND) & get_wholeGroup(data$group), "TR_BUNDESLAND"] <- "wholeGroup"
  data[is.na(data$grouping_var), "grouping_var"] <- "noGroup"

  colnames(data) <- gsub("\\.", "_", colnames(data))
  colnames(data) <- gsub("sig_", "p_", colnames(data))
  data <- data[data$kb == competence & data$parameter == "mean", ]
  data <- data[ , !colnames(data) %in% c("modus","depVar", "modus", "parameter", "kb")]

  return(data)
}
