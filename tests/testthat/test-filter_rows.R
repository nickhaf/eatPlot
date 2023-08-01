test_list <- list(
  "a" = data.frame(
    col_1 = c(1, 2),
    col_2 = c("a", NA)
  ),
  "b" = data.frame(
    col_1 = c(1, NA),
    col_2 = c("a", "a")
  )
)

test_that("filter_rows throws the expected errors", {
  expect_error(filter_rows(test_list,
                           column_name = "col_2",
                           subsetter = "c",
                           list_elements = c("a", "c"),
                           remove = FALSE
  ), "Some of your list_elements are not part of plot_dat."
  )

  expect_error(filter_rows(test_list,
                           column_name = "col_3",
                           subsetter = "c",
                           list_elements = c("a", "b"),
                           remove = FALSE
  ), "Your column_name 'col_3' is not part of the sublist 'a' of your plot_dat.")
})



test_that("filter_rows works for NAs", {
  ## Build resulting empty data.frame
  res_dat <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(res_dat) <- c("col_1", "col_2")
  res_dat$col_1 <- as.double(res_dat$col_1)
  res_dat$col_2 <- as.character(res_dat$col_2)

  ## Don't remove
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

  ## Remove
  res_dat_a <- data.frame(
    col_1 = 2,
    col_2 = as.character(NA)
  )
  rownames(res_dat_a) <- as.integer(2)

  expect_equal(
    filter_rows(test_list,
                column_name = "col_2",
                subsetter = "a",
                list_elements = c("a", "b"),
                remove = TRUE
    ),
    list(
      "a" = res_dat_a,
      "b" = res_dat
      )
    )

  # Nonsense subsetter remove
  expect_equal(filter_rows(test_list,
                              column_name = "col_2",
                              subsetter = "c",
                              list_elements = c("a", "b"),
                              remove = TRUE
  ), test_list)


  # Nonsens subsetter
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

  # Remove NAs
  expect_false(any(is.na(filter_rows(test_list,
                           column_name = "col_2",
                           subsetter = "c",
                           list_elements = c("a", "b"),
                           remove = TRUE,
                           remove_na = TRUE
  )$a$col_1)))

})
