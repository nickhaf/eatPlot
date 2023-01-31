#' Merge point estimates onto the resepective timepoints.
#'
#' @param trend_whole Data.frame with the trend comparisons against the whole group (e.g. Germany).
#' @param trend_within Data.frame with the trend comparisons within the group (e.g. states).
#' @param point_estimates Data.frame with the point estimates for the start and end years.
#'
#' @return Data frame with the within significance estimates and the point estimates for start and end year.
#' @export
#'
#' @examples #tbd
prep_trend_point <- function(trend_whole, trend_within, point_estimates) {
  trend_whole <- trend_whole[, c("TR_BUNDESLAND", "grouping_var", "year_start", "year_end", "est_trend_whole","sig_trend_whole")]
  trend_within <- trend_within[, c("TR_BUNDESLAND", "grouping_var", "year_start", "year_end", "est_trend_within", "se_trend_within","sig_trend_within")]
  point_estimates <- point_estimates[, c("TR_BUNDESLAND", "grouping_var", "time", "est_point")]

  trend_start <- merge(trend_within,
    point_estimates,
    by.x = c("TR_BUNDESLAND", "year_start", "grouping_var"),
    by.y = c("TR_BUNDESLAND", "time", "grouping_var"),
    all.x = TRUE, all.y = FALSE,
    sort = FALSE
  )

  trend_start <- rename_column(trend_start, "est_point", "est_point_start")

  trend <- merge(trend_start,
    point_estimates,
    by.x = c("TR_BUNDESLAND", "year_end", "grouping_var"),
    by.y = c("TR_BUNDESLAND", "time", "grouping_var"),
    all.x = TRUE, all.y = FALSE,
    sort = FALSE
  )

  trend$year_start <- as.numeric(trend$year_start)
  trend$year_end <- as.numeric(trend$year_end)

  trend <- rename_column(trend, "est_point", "est_point_end")

  trend_final <- merge(trend, trend_whole, by = c("TR_BUNDESLAND", "grouping_var", "year_start", "year_end"), sort = FALSE)

  return(trend_final)
}
