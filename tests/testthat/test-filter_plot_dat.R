test_that("multiplication works", {
  test_list <- list(
    "a" = data.frame(
      col_1 = c(1, 2),
      col_2 = c("a", "b")
    ),
    "b" = data.frame(
      col_1 = c(1, 2),
      col_2 = c("a", "b")
    )
  )

  expect_equal(
    filter_plot_dat(test_list, filter_statement = "dat$col_1 == 1", list_elements = c("a", "b")),
    list(
      "a" = data.frame(
        col_1 = c(1),
        col_2 = c("a")
      ),
      "b" = data.frame(
        col_1 = c(1),
        col_2 = c("a")
      )
    )
  )

  expect_no_error(filter_plot_dat(test_list, filter_statement = "dat$col_3 == 3", list_elements = c("a", "b")))
})
