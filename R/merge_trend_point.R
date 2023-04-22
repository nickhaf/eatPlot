#' Merge point estimates onto the resepective timepoints.
#'
#' @param trend_data Data.frame with the trend comparisons.
#' @param point_data Data.frame with the point estimates for the start and end years.
#'
#' @return Data frame with the within significance estimates and the point estimates for start and end year.
#' @export
#'
#' @examples #tbd
merge_trend_point <- function(trend_data, point_data){

  trend_data <- trend_data[ , !colnames(trend_data) %in% c("modus", "comparison", "parameter", "group_var", "keyword")]
  point_data <- point_data[ , !colnames(point_data) %in% c("modus", "comparison", "parameter", "group_var", "keyword")]

  trend_start <- merge(trend_data,
                     point_data,
                     by.x = c("state_var", "year_start", "grouping_var", "depVar", "competence_var"),
                     by.y = c("state_var", "year", "grouping_var", "depVar", "competence_var"),
                     all.x = TRUE,
                     all.y = FALSE,
                     sort = FALSE
)

colnames(trend_start) <- gsub("_noTrend", "_noTrendstart", colnames(trend_start))

trend <- merge(trend_start,
               point_data,
               by.x = c("state_var", "year_end", "grouping_var", "depVar", "competence_var"),
               by.y = c("state_var", "year", "grouping_var", "depVar", "competence_var"),
               all.x = TRUE,
               all.y = FALSE,
               sort = FALSE
)

colnames(trend) <- gsub("_noTrend_", "_noTrendend_", colnames(trend))

return(trend)
}
