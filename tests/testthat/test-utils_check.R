col_1 <- 1
dat_check <- data.frame("col_1" = col_1,
                        "col_2" = 2)

test_that("check_cols flags columns not part of a data.frame", {

  expect_error(check_columns(col_1, c("col_1", "col_3")),
               "Assertion on 'dat' failed: Must be of type 'data.frame', not 'double'.")
  expect_error(check_columns(dat_check, c("col_1", "col_3")))
  expect_no_error(check_columns(dat_check, c("col_1", "col_2")))
})
