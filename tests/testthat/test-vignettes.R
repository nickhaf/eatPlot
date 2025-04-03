ksource(vignette_path_line)
ksource(vignette_path_table)


test_that("lineplot with two groups works", {
  vdiffr::expect_doppelganger("lineplot_2", lineplot_2)
})

test_that("lineplot of Germany with two groups works", {
  vdiffr::expect_doppelganger("lineplot_germany", lineplot_germany)
})

test_that("lineplot of Germany with two groups works", {
  vdiffr::expect_doppelganger("lineplot_germany_2", lineplot_germany_2)
})

test_that("lineplot with one group works", {
  vdiffr::expect_doppelganger("lineplot_1", lineplot_1)
})

test_that("tableplot 6.5 works", {
  vdiffr::expect_doppelganger("tableplot_6.5", tableplot_6.5)
})

test_that("tableplot 6.6 works", {
  vdiffr::expect_doppelganger("tableplot_6.6", tableplot_6.6)
})

test_that("tableplot 8.4 works", {
     vdiffr::expect_doppelganger("tableplot_8.4", tableplot_8.4)
})
