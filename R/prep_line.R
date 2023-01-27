prep_line <- function(data_points, data_trend){

  trend_merged <- merge(data_trend[, c()],
                        data_points,
                        by.x = c("TR_BUNDESLAND", "year_start", grouping_var),
                        by.y = c("TR_BUNDESLAND", "year", grouping_var),
                        all.x = TRUE,
                        sort = FALSE
  )

}


df_points <- prep_points(trend_books, competence = "GL", grouping_var = "KBuecher_imp3")
df_trend <- prep_trend(trend_books, competence = "GL", grouping_var = "KBuecher_imp3")
