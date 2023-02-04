test_that("Table-barplot (trend_books) is still the same", {
  plot_data <- prep_no_trend(data = adjusted_means, grouping_var = "adjust", columns = "adjust", competence = "GL", sig_niveau = 0.05)
  p1 <- plot_table(plot_data[["plot_table"]])
  p2 <- plot_bar(plot_data[["plot_bar"]])

vdiffr::expect_doppelganger("Table bar plot", plot_table_bar(p1, p2))
})

