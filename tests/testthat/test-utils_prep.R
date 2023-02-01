test_that("calc_sig(): significance niveau is working correctly", {

  test_sig <- calc_sig(c(0.02, 0.1, 0.01, 12.4), sig_niveau = 0.03)

  expect_equal(test_sig, factor(c(TRUE, FALSE, TRUE, FALSE)))

  test_sig_2 <- calc_sig(c(0.02, 0.1, 0.01, NA))
  expect_equal(test_sig_2, factor(c(TRUE, FALSE, TRUE, NA)))
})


test_that("filter_string(): correct rows are filtered", {
df_1 <- data.frame(BL = c("Berlin", "Berlin"), col1 = c("Berlin_a", "Berlin_a"))

  expect_error(filter_strings(identifier = unique(df_1$BL), paste_vec = "_a", val_vec = df_1$col1),
               "Duplicated groups. For example, there might be two groups of the same type within the same Bundesland.",
               fixed = TRUE)

  df_2 <- data.frame(BL = c("Berlin", "Berlin", "Bremen", "Bremen"),
                   col1 = c("Berlin_a", "Berlin_b", "Bremen_a", "Bremen_b"))

  expect_equal(filter_strings(identifier = unique(df_2$BL), paste_vec = "_a", val_vec = df_2$col1), c(1, 3))
})

test_that("consecutive numbers are correct", {

  expect_equal(consecutive_numbers(c(2011, 2012, 2013, 2012, 2013)), list(c(2011, 2012), c(2012, 2013)))
})


test_that("get_group() filters the correct rows", {

  expect_true(unique(get_group(c("a", "b"), "c" )) == FALSE)
  expect_equal(get_group(c("a", "b", "c"), c("a","b")), c(TRUE, TRUE, FALSE))
  expect_equal(get_group(c("ab", "ac", "cb", "cc"), "c", starts_with = "^"), c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(get_group(c("ab", "ac", "cb", "cc"), "c", ends_with = "$"), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(get_group(c("ab", "ac", "cb", "cc"), c("b","c"), ends_with = "$"), c(TRUE, TRUE, TRUE, TRUE))
})


test_that("reshaping to long format works", {
df_wide <- data.frame("est.1" = 1:2,
                      "est.2" = 3:4,
                      "p.1" = 4:5,
                      "p.2" = 6:7)

df_long_test <- prep_long(df_wide, include_pattern = "est\\.|p\\.")

expect_equal(df_long_test$est_, 1:4)
expect_equal(df_long_test$year,  c(1, 1, 2, 2))
expect_equal(colnames(df_long_test), c("year", "est_", "p_"))

df_long_test_2 <- prep_long(df_wide, include_pattern = "est", remove_pattern = "p\\.", suffix = "test")

expect_equal(colnames(df_long_test_2), c("year", "est_test"))

})


test_that("years are split correctly", {

df_years <- data.frame(year = c("2011vs2016", "2013.2014"))

expect_equal(split_years(df_years)$year_start, c(2011, 2013))



})
