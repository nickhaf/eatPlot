test_that("Barplot is still the same", {
df_bar <- data.frame(x = 1:4,
                     y = rep("a", 4),
                     bar_pattern = c(TRUE, TRUE, FALSE, FALSE),
                     bar_fill = c("a", "b", "c", "d"),
                     bar_pattern_fill = c("a", "a", "b", "b")
                     )


vdiffr::expect_doppelganger("Bar plot", plot_bar(df_bar, x_value = "x", y_value = "y",
                                                 bar_pattern = "bar_pattern", bar_fill = "bar_fill", bar_pattern_fill = "bar_pattern_fill",
                                                 bar_fill_setting = c("a" = "red", "b" = "blue", "c" = "green", "d" = "grey"),
                                                 bar_pattern_fill_setting = c("a" = "yellow", "b" = "orange"))
                            )
})


test_that("Example barplot is still the same", {
  plot_data <- prep_no_trend(data = adjusted_means, grouping_var = "adjust", columns = "adjust", competence = "GL", sig_niveau = 0.05)
  vdiffr::expect_doppelganger("Bar plot for trend_books", plot_bar(plot_data[["plot_bar"]]))
})
