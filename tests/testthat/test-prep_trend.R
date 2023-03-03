test_that("groups are extracted correctly", {

  expect_equal(get_group(c("a.al", "stb3" ,"s.4.r"), c("a", "4")), c(TRUE, FALSE, TRUE))

})

test_that("correct line years are extracted", {
 prepped_list_consecutive <- prep_trend(trend_books, competence = "GL")
 expect_true(!("2011vs2021" %in% prepped_list_consecutive[["plot_lines"]]$trend_years))

 prepped_list_consecutive <- prep_trend(trend_books, competence = "GL", x_years = list(c(2011, 2021)))
 expect_true(all(prepped_list_consecutive[["plot_lines"]]$trend_years == "2011vs2021"))

 })


test_that("trend column is build correctly", {

  test_list <- list("a" = data.frame(year_start = c(2, 3),
                                     year_end = c(3, 4),
                                     third_col = c(1, 2)),
                    "b" = data.frame(year_start = c(1,2))
  )

  trend_list <- get_trend(test_list)

  expect_equal(ncol(trend_list[[1]]), 4)
  expect_equal(trend_list[[1]]$trend, c("23", "34"))
  expect_equal(ncol(trend_list[[2]]), 1)
})
