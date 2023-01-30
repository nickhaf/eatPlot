#' Merge point estimates onto the resepective timepoints.
#'
#' @param prep_list List stemming from \code{prep_lineplot()}.
#'
#' @return Data frame with the within significance estimates and the point estimates for start and end year.
#' @export
#'
#' @examples #tbd
prep_lines <- function(prep_list) {
  trend_whole <- prep_list[["trend_whole"]][, c("TR_BUNDESLAND", "grouping_var", "year_start", "year_end", "est_trend_whole","sig_trend_whole")]
  trend_within <- prep_list[["trend_within"]][, c("TR_BUNDESLAND", "grouping_var", "year_start", "year_end", "est_trend_within", "se_trend_within","sig_trend_within")]
  point_estimates <- prep_list[["point_estimates"]][, c("TR_BUNDESLAND", "grouping_var", "time", "est_point")]


  ## only use consecutive years:
  years <- c(prep_list[["trend_within"]]$year_start, prep_list[["trend_within"]]$year_end)
  plot_years <- consecutive_numbers(years)

  # Filter the respective rows
  year_rows <- unlist(lapply(plot_years, function(x) {
    which(trend_within$year_start == x[1] & trend_within$year_end == x[2])
  }))
  trend_within <- trend_within[year_rows, ]



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
  return(trend)
}
