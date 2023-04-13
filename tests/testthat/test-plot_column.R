test_that("single column can be plotted", {

  vdiffr::expect_doppelganger("single column plot",
                                plot_column(c("a", "b"))
  )
})
