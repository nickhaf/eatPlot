test_that("single column can be plotted", {

  vdiffr::expect_doppelgange("single column plot",
                             ggplot2::ggplot() +
                               plot_single_column(c("a", "b"))
  )
})
