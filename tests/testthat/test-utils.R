test_that("Plot bordres are calculated correctly", {
  expect_equal(calc_plot_borders(c(-114, 12, 140, -211)), c(-220, 220))
  expect_equal(calc_plot_borders(c(22, -15, -17), accuracy = 5), c(-25, 25))
  expect_error(calc_plot_borders(c(1, "a", 4)))
})

test_that("coords are calculated correctly", {
  expect_equal(calc_coords(c(10, 100)), c(0, 110))
  expect_equal(calc_coords(c(0, 1)), c(0, 10))
})



test_that("calc_sig(): significance niveau is working correctly", {

  test_sig <- calc_sig(c(0.02, 0.1, 0.01, 12.4), sig_niveau = 0.03)
  expect_equal(test_sig, c(TRUE, FALSE, TRUE, FALSE))

})


test_that("write_group finds the correct group membership", {

  expect_equal(write_group(c("a_1", NA, "b_1", "ab_1", "c_a_1"), groups = c("a", "c")),
               c("a", NA, NA, NA, "c"))
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
                      "p.2" = 6:7,
                      "p_trend.1" = 1:2,
                      "p_trend.2" = 2:3)

df_long_test <- prep_long(df_wide, include_pattern = "est\\.|p\\.|trend\\.")

expect_equal(df_long_test$est, 1:4)
expect_equal(df_long_test$year,  c(1, 1, 2, 2))
expect_equal(colnames(df_long_test), c("year", "est", "p", "p_trend"))

df_long_test_2 <- prep_long(df_wide, include_pattern = "est", remove_pattern = "p\\.|trend", suffix = "_test")

expect_equal(colnames(df_long_test_2), c("year", "est_test"))

})


test_that("years are split correctly", {

df_years <- data.frame(year = c("2011vs2016", "2013.2014"))

expect_equal(split_years(df_years)$year_start, c(2011, 2013))



})


test_that("number insertion works", {
  expect_equal(insert_first_number("a4b5", insertion = "\\."), "a.4b5")
  expect_equal(insert_first_number("a45c6", insertion = "\\."), "a.45c6")

})

test_that("comparison splits works", {

df_comp <- data.frame(comp = c("a.vs.b", "b.vs.c"))

expect_equal(get_comparisons(df_comp, group_col = "comp", states = "a", sub_groups = c("b", "c"))$compare_1, c("BL", "_groupingVar"))
expect_equal(get_comparisons(df_comp, group_col = "comp", states = "b", sub_groups = c("a"))$compare_2, c("BL", "c"))

})

test_that("column renaming works", {
  df_col <- data.frame(col1 = 1)

  expect_equal(colnames(rename_column(df_col, "col1", "col_1")), "col_1")
})


test_that("brace_position is calculated correctly", {

expect_equal(calc_overlap(year_start = c(1, 1, 2, 2), year_end = c(3, 3, 4, 4)),
             c(FALSE, FALSE, TRUE, TRUE))

expect_equal(calc_overlap(year_start = c(1, 1, 2, 2), year_end = c(2, 2, 3, 4)),
               c(FALSE, FALSE, FALSE, FALSE))


})