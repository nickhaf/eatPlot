
prep_list <- prep_lineplot(trend_books, grouping_var = "KBuecher_imp3", competence = "GL")

prep_lines <- function(prep_list){

  ## only use consecutive years:



  trend_within <- prep_list[["trend_within_group"]][, c("TR_BUNDESLAND", "grouping_var", "year_start", "year_end", "sig_trend_within")]
  point_estimates <- prep_list[["point_estimates"]][, c("TR_BUNDESLAND", "grouping_var", "time", "est_point")]

  trend_start <- merge(trend_within,
                        point_estimates,
                        by.x = c("TR_BUNDESLAND", "year_start", "grouping_var"),
                        by.y = c("TR_BUNDESLAND", "time", "grouping_var"),
                        all.x = TRUE, all.y = FALSE,
                        sort = FALSE
  )

  trend_start <- rename_column(trend_start, "est_point", "est_point_start")

}


df_points <- prep_points(trend_books, competence = "GL", grouping_var = "KBuecher_imp3")
df_trend <- prep_trend(trend_books, competence = "GL", grouping_var = "KBuecher_imp3")

