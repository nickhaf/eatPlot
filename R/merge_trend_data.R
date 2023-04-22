#' Wrapper for merging to trend data.frames by the year columns.
#'
#' @param trend_data_1 Trend data.frame 1.
#' @param trend_data_2 Trend data.frame 2.
#' @param suffixes Suffixes that should be added to duplicated columns.
#' @param ... Further arguments provided by [merge()].
#'
#' @return Merged data.frame stemming from the input data.frames.
#' @export
#'
#' @examples # tbd
merge_trend_data <- function(trend_data_1,
                             trend_data_2,
                             suffixes,
                             ...) {
  trend_data_1 <- trend_data_1[, !(colnames(trend_data_1) %in% c("modus", "comparison", "parameter", "group_var", "keyword"))]
  trend_data_2 <- trend_data_2[, !(colnames(trend_data_2) %in% c("modus", "comparison", "parameter", "group_var", "keyword"))]

data_merged <- merge(
    trend_data_1,
    trend_data_2,
    by = c("state_var", "grouping_var", "year_start", "year_end", "depVar", "competence_var", "years_Trend"),
    sort = FALSE,
    suffixes = suffixes,
    ...
  )

colnames(data_merged) <- gsub("_trend_Trend", "_Trend", colnames(data_merged))

  return(data_merged)
}
