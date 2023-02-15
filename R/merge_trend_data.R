#' Wrapper for merging to trend dataframes by the year columns.
#'
#' @inheritParams prep_data_blocks
#' @param trend_data_1 Trend data containing trend comparisons.
#' @param trend_data_2 Trend data containing estimates for the trends.
#' @param suffixes Suffixes that should be added to duplicated columns.
#'
#' @return Merged data.frame stemming from the input data.frames.
#' @export
#'
#' @examples # tbd
merge_trend_data <- function(trend_data_1,
                             trend_data_2,
                             suffixes,
                             ...) {
  trend_data_1 <- trend_data_1[, !(colnames(trend_data_1) %in% c("depVar", "modus", "comparison", "parameter", "kb"))]
  trend_data_2 <- trend_data_2[, !(colnames(trend_data_2) %in% c("depVar", "modus", "comparison", "parameter", "kb"))]

data_merged <- merge(
    trend_data_1,
    trend_data_2,
    by = c("state_var", "grouping_var", "year_start", "year_end"),
    sort = FALSE,
    suffixes = suffixes,
    ...
  )
  return(data_merged)
}
