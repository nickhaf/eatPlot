merge_trend_data <- function(trend_data_1,
                             trend_data_2,
                             suffixes,
                             ...) {
  trend_data_1 <- trend_data_1[, !(colnames(trend_data_1) %in% c("modus", "comparison", "parameter", "group_var", "keyword"))]
  trend_data_2 <- trend_data_2[, !(colnames(trend_data_2) %in% c("modus", "comparison", "parameter", "group_var", "keyword"))]

  data_merged <- merge_2(
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


merge_trend_point <- function(trend_data, point_data) {
  trend_data <- trend_data[, !colnames(trend_data) %in% c("modus", "comparison", "parameter", "group_var", "keyword")]
  point_data <- point_data[, !colnames(point_data) %in% c("modus", "comparison", "parameter", "group_var", "keyword")]

  trend_start <- merge_2(trend_data,
    point_data,
    by.x = c("state_var", "year_start", "grouping_var", "depVar", "competence_var"),
    by.y = c("state_var", "year", "grouping_var", "depVar", "competence_var"),
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )

  colnames(trend_start) <- gsub("_noTrend", "_noTrendStart", colnames(trend_start))

  trend <- merge(trend_start,
    point_data,
    by.x = c("state_var", "year_end", "grouping_var", "depVar", "competence_var"),
    by.y = c("state_var", "year", "grouping_var", "depVar", "competence_var"),
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )

  colnames(trend) <- gsub("_noTrend_", "_noTrendEnd_", colnames(trend))

  return(trend)
}
