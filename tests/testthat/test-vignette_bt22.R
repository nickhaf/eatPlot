test_that("minsta_plot works", {
  vdiffr::expect_doppelganger("minsta_plot", minsta_plot )
})

test_that("minsta_plot_trend works", {
  vdiffr::expect_doppelganger("minsta_plot_trend", minsta_plot_trend )
})

test_that("p_line_states works", {
  vdiffr::expect_doppelganger("p_line_states", p_line_states )
})

test_that("p_line_ger works", {
  vdiffr::expect_doppelganger("p_line_ger", p_line_ger )
})

