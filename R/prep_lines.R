
prep_list <- prep_lineplot(trend_books, grouping_var = "KBuecher_imp3", competence = "GL")

prep_lines <- function(prep_list){



  trend_merged <- merge(prep_list[["trend_within_group"]],
                        prep_list[["point_estimates"]],
                        by.x = c("TR_BUNDESLAND", "year_start", grouping_var),
                        by.y = c("TR_BUNDESLAND", "year", grouping_var),
                        all.x = TRUE,
                        sort = FALSE
  )

}


df_points <- prep_points(trend_books, competence = "GL", grouping_var = "KBuecher_imp3")
df_trend <- prep_trend(trend_books, competence = "GL", grouping_var = "KBuecher_imp3")
