## Es müssen ja immer wieder andere rows rangehängt werden. Vielleicht zwei Funktionen, wo man die rows reingibt, und dann wird nach dem selben pattern gemerged?

merge_ <- function(d)











## Add point estimates to trend data
add_point <- function(prepped_list){

trend_start <- merge(prepped_list[["trend_data"]],
        prepped_list[["point_data"]],
        by.x = c("TR_BUNDESLAND", "year_start", "grouping_var"),
        by.y = c("TR_BUNDESLAND", "time", "grouping_var"),
        all.x = TRUE, all.y = FALSE,
        sort = FALSE
  )


}
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
