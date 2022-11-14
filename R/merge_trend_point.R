merge_trend_point <- function(trend1, trend2, point, grouping_var) {
  merged_trend <- merge(trend1, trend2,
    by = c("TR_BUNDESLAND", grouping_var, "year_start", "year_end")
  )

  trend_merged <- merge(merged_trend,
    point,
    by.x = c("TR_BUNDESLAND", "year_start", grouping_var),
    by.y = c("TR_BUNDESLAND", "year", grouping_var),
    all.x = TRUE,
    sort = FALSE
  )
  colnames(trend_merged)[colnames(trend_merged) == "est"] <- "est_start"

  trend_merged2 <- merge(trend_merged,
    point[, c("TR_BUNDESLAND", "KBuecher_imp3", "year", "est")],
    by.x = c("TR_BUNDESLAND", "KBuecher_imp3", "year_end"),
    by.y = c("TR_BUNDESLAND", "KBuecher_imp3", "year"),
    all.x = TRUE,
    sort = FALSE
  )
  colnames(trend_merged2)[colnames(trend_merged2) == "est"] <- "est_end"
  return(trend_merged2)
}
