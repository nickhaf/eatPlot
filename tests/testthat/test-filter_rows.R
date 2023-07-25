test_that("multiplication works", {
  test_list <- list(
    "a" = data.frame(
      col_1 = c(1, NA),
      col_2 = c("a", "b")
    ),
    "b" = data.frame(
      col_1 = c(1, NA),
      col_2 = c("a", "a")
    )
  )

  expect_equal(
    filter_rows(test_list,
                column_name = "col_2",
                subsetter = "a",
                list_elements = c("a", "b")
                ),
    list(
      "a" = data.frame(
        col_1 = c(1),
        col_2 = c("a")
      ),
      "b" = data.frame(
        col_1 = c(1, NA),
        col_2 = c("a", "a")
      )
    )
  )

  expect_equal(filter_rows(test_list,
                              column_name = "col_2",
                              subsetter = "c",
                              list_elements = c("a", "b"),
                              remove = TRUE
  ), test_list)

  ## Build resulting data.frames for testing
  res_dat <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(res_dat) <- c("col_1", "col_2")
  res_dat$col_1 <- as.double(res_dat$col_1)
  res_dat$col_2 <- as.character(res_dat$col_2)


  expect_equal(filter_rows(test_list,
                           column_name = "col_2",
                           subsetter = "c",
                           list_elements = c("a", "b"),
                           remove = FALSE
  ),
  list("a" = res_dat,
       "b" = res_dat
       )
  )
})
