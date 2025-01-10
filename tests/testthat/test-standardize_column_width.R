test_that("standardization of column widths works for 2 plots", {
  expect_equal(
    standardize_column_width(
      column_widths = list(
        p1 = c(0.2, 0.2),
        p2 = c(0.2, 0.4)
      ),
      plot_ranges = c(0, 0)
    ),
    list(
      p1 = c(0.5, 0.5),
      p2 = c(1 / 3, 2 / 3)
    )
  )

  expect_equal(
    standardize_column_width(
      column_widths = list(
        p1 = c(0.2, NA),
        p2 = c(0.2, NA)
      ),
      plot_ranges = c(10, 20)
    ),
    list(
      p1 = c(0.5, 0.5),
      p2 = c(1 / 3, 2 / 3)
    )
  )
})

test_that("standardization of column widths works for 3 plots", {
  expect_equal(
    standardize_column_width(
      column_widths = list(
        p1 = c(0.2, NA),
        p2 = c(0.2, NA),
        p3 = c(0.2, NA)
      ),
      plot_ranges = c(10, 20, 10)
    ),
    list(
      p1 = c(2 / 3, 1 / 3),
      p2 = c(0.5, 0.5),
      p3 = c(2 / 3, 1 / 3)
    )
  )
})
