test_that("year columns are build correctly", {

df <- data.frame(group = c(0, 0, 1, 1),
                 TR_BUNDESLAND = c("Berlin", "Berlin", "Bremen", "Bremen"),
                 est_trend_2011.vs.2013 = c(1, 2, 0, 1),
                 est_trend_2019.vs.2050 = c(7, 5, 6, 4),
                 sig_trend_2011.vs.2013 = c(0.05, 0, 0.02, 0.03),
                 sig_trend_2019.vs.2050 = c(0.01, 0.2, 1, 54),
                 grouping_var = c(1, 1, 0, 0)
                 )

df_prep <- prep_line(df, grouping_var = "grouping_var", sig_niveau = 0.05)

expect_equal(unlist(df_prep[1, c("year_start", "year_end")]), c(year_start = "2011", year_end = "2013"))


})
