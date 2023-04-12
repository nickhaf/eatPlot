test_that("Table-barplot (trend_books) is still the same", {
  plot_data <- prep_no_trend(dat = adjusted_means,
                             columns = "adjust",
                             competence = "GL",
                             grouping_var = "adjust",
                             sig_niveau = 0.05)
  p1 <- plot_table(plot_data[["plot_table"]])
  p2 <- plot_bar(plot_data[["plot_bar"]])

vdiffr::expect_doppelganger("Table bar plot pattern", plot_table_bar(p1, p2))
})

test_that("Table-barplot (trend_books) is still the same with frames", {
  plot_data <- prep_no_trend(dat = adjusted_means,
                             columns = "adjust",
                             competence = "GL",
                             grouping_var = "adjust",
                             sig_niveau = 0.05)
  p1 <- plot_table(plot_data[["plot_table"]])
  p2 <- plot_bar(plot_data[["plot_bar"]], bar_sig_type = "frame")

  vdiffr::expect_doppelganger("Table bar plot frame", plot_table_bar(p1, p2))
})

