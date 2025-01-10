## Zuordnung über group-dataframe. Falls also eine Gruppe nicht geplottet werden soll,
## einfach hier rausnehmen.

trend_3$group <- subset(trend_3$group, trend_3$group$mhg %in% c("einET", "ersteGen")) #| is.na(trend_3$group$mhg))
trend_3$group$TR_BUNDESLAND <- factor(trend_3$group$TR_BUNDESLAND,
                                      levels = unique(trend_3$group$TR_BUNDESLAND)[-1]
)

trend_3_prepped <- prep_lineplot(
  trend_3,
  parameter = "mean"
)


test_that("x axis can be built on plot with relational distance x-axis", {


})


