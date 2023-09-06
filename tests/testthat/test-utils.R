test_that("colour checks are working", {
  expect_equal(is_colour(c("lightblue", grDevices::rgb(147, 205, 221, maxColorValue = 255), "no_colour")), c(TRUE, TRUE, FALSE))
})

test_that("Plot borders are calculated correctly", {
  expect_equal(calc_plot_borders(c(-114, 12, 140, -211)), c(-220, 220))
  expect_equal(calc_plot_borders(c(22, -15, -17), accuracy = 5), c(-25, 25))
  expect_error(calc_plot_borders(c(1, "a", 4)))
})

test_that("calc_sig(): significance niveau is working correctly", {

  test_sig <- calc_sig(c(0.02, 0.1, 0.01, NA, 0.03), sig_niveau = 0.03)

  expect_equal(test_sig, c(TRUE, FALSE, TRUE, FALSE, FALSE))
})


test_that("write_group finds the correct group membership", {
  expect_equal(
    write_group(c("a_1", NA, "b_1", "ab_1", "c_a_1"), groups = c("a", "c")),
    c("a", NA, NA, NA, "c")
  )

  expect_equal(
    write_group(
      "TR_BUNDESLAND=Berlin____einET-alle.vs.zweiteGen-alle.VS.all.group=1____einET-alle.vs.zweiteGen-alle",
      groups = "Berlin"
    ), "Berlin"
  )
})


test_that("consecutive numbers are correct", {
  expect_equal(consecutive_numbers(c(2011, 2012, 2013, 2012, 2013)), list(c(2011, 2012), c(2012, 2013)))
})


test_that("reshaping to long format works", {
  df_wide <- data.frame(
    "est.1" = 1:2,
    "est.2" = 3:4,
    "p.1" = 4:5,
    "p.2" = 6:7,
    "p_trend.1" = 1:2,
    "p_trend.2" = 2:3
  )

  df_long_test <- prep_long(df_wide, include_pattern = "est\\.|p\\.|trend\\.")

  expect_equal(df_long_test$est, 1:4)
  expect_equal(df_long_test$year, c(1, 1, 2, 2))
  expect_equal(colnames(df_long_test), c("year", "est", "p", "p_trend"))

  df_long_test_2 <- prep_long(df_wide,
    include_pattern = "est",
    remove_pattern = "p\\.|trend",
    suffix = "_test"
  )

  expect_equal(colnames(df_long_test_2), c("year", "est_test"))
})


test_that("years are split correctly", {
  df_years <- data.frame(year = c("2011vs2016", "2013.2014"))

  expect_equal(split_years(df_years, year_col = "year")$year_start, c(2011, 2013))
})


test_that("number insertion works", {
  expect_equal(insert_first_number("a4b5", insertion = "\\."), "a.4b5")
  expect_equal(insert_first_number("a45c6", insertion = "\\."), "a.45c6")
})

test_that("comparison splits works for simple example", {
  df_comp <- data.frame(
    group_var = c("a.vs.b", "b.vs.c"),
    grouping_var = c(NA, NA)
  )

  expect_equal(get_comparisons(df_comp, states = "a", sub_groups = factor(c("b", "c")))$compare_1, c("BL", "b"))
  expect_equal(get_comparisons(df_comp, states = "b", sub_groups = factor(c("a")))$compare_2, c("BL", "c"))
  expect_equal(get_comparisons(df_comp, states = "a", sub_groups = NULL)$compare_2, c("b", "c"))
})

test_that("comparison splits work for complex comparisons", {
  df_comp <- data.frame(
    group_var = c("TR_BUNDESLAND=Baden-Wuerttemberg____aohneZWH.vs.zweiteGen.vs.all.group=1____ersteGen.vs.zweiteGen"),
    grouping_var = NA
  )

  #  expect_equal(get_comparisons(df_comp, states = c("Baden-Wuerttemberg"),
  #                   sub_groups = factor(c("aohneZWH", "ersteGen", "zweiteGen"))),
  # )
})


test_that("column renaming works", {
  expect_equal(colnames(build_column(data.frame(col1 = 1), "col1", "col_1")), c("col1", "col_1"))
  expect_error(build_column(data.frame(col1 = 1), "col2", "col1"))
  expect_equal(build_column(data.frame(col1 = 1), NULL, "col2"), data.frame(col1 = 1, col2 = NA))
})


test_that("brace_position is calculated correctly", {
  expect_equal(
    calc_overlap(year_start = c(1, 1, 2, 2), year_end = c(3, 3, 4, 4)),
    c(TRUE)
  )

  expect_equal(
    calc_overlap(year_start = c(1, 1, 2, 2), year_end = c(2, 2, 3, 4)),
    c(TRUE)
  )

  expect_equal(calc_overlap(year_start = c(2011, 2011, 2011, 2011),
                            year_end = c(2016, 2016, 2021, 2021)),
               c(TRUE))

  expect_equal(calc_overlap(year_start = c(2011, 2011, 2016, 2016),
                            year_end = c(2016, 2016, 2021, 2021)),
               c(FALSE))
})


test_that("columns are removed correctly", {
  df_cols <- data.frame("col_1" = 1, "col_2" = 2)
  expect_equal(colnames(remove_columns(df_cols, "col_2")), "col_1")
})

test_that("columns are checked correctly", {
  expect_no_error(check_column(dat = data.frame("a" = 1, "b" = 1), column = "a"))
  expect_no_error(check_column(dat = data.frame("a" = 2, "b" = 1), column = NULL))
  expect_error(check_column(dat = data.frame("a" = 1, "b" = 1), column = "c"), "Variable 'c' not found in data.", fixed = TRUE)
})


test_that("NULL-cols are filled", {
  df <- data.frame(col1 = c(2, 3))
  plot_points <- "col1"
  res_df <- fill_column(df = df, column_name = plot_points, filling = NA)
  expect_equal(res_df$plot_points, c(2, 3))


  df <- data.frame(col1 = c(2, 3))
  plot_points <- NULL
  res_df_null <- fill_column(df = df, column_name = plot_points, filling = NA)
  expect_equal(res_df_null$plot_points, c(NA, NA))
})


test_that("Columns are renamed correctly", {
  expect_equal(
    rename_columns(data.frame("a" = 1, "b" = 1),
      old_names = c("a", "b"),
      new_names = c("c", "d")
    ),
    data.frame("c" = 1, "d" = 1)
  )
})


test_that("Own merge command works correctly", {
  df_1 <- data.frame(
    group = c("a", "b"),
    values = c(1, 2)
  )
  df_2 <- data.frame(
    group = c("a", "c"),
    values = c(1, 2)
  )

  df_null <- NULL
  df_empty <- data.frame()

  expect_equal(merge_2(df_1, df_2, by = "group", all = TRUE)$group, c("a", "b", "c"))
  expect_equal(merge_2(df_null, df_2, by = "group", all = TRUE), data.frame())
  expect_equal(merge_2(df_1, df_empty, by = "group", all = TRUE), df_1)
  expect_equal(merge_2(df_1, df_empty, by = "group"), df_1)
})

test_that("second vs is replaced correctly", {
  x <- c(
    "as", "", NA, "black.vs.white",
    "TR_BUNDESLAND=Baden-Wuerttemberg____aohneZWH.vs.zweiteGen.vs.all.group=1____ersteGen.vs.zweiteGen",
    "TR_BUNDESLAND=Baden-Wuerttemberg____aohneZWH.vs.zweiteGen.VS.all.group=1____ersteGen.vs.zweiteGen"
  )

  expect_equal(replace_VS(x)[c(4, 5, 6)], c("black.VS.white", rep("TR_BUNDESLAND=Baden-Wuerttemberg____aohneZWH.vs.zweiteGen.VS.all.group=1____ersteGen.vs.zweiteGen", 2)))
})



test_that("Linebreaks are counted correctly", {

  test_string <- c("This <br> is <br> a linebreak", "This is not", "This <br> are <br>  more <br>")
  test_list <- list("This <br> is <br> a linebreak" = c(1,2), "This is not" = c("a", "b"))

  expect_equal(count_words(test_string, "<br>"), 3)
  expect_equal(count_words(test_list, "<br>"), 2)
})

test_that("scale breaks are calculated correctly", {
  expect_equal(set_scale_breaks(c(-20, 10)), c(0, -10, -20, 10))
  expect_equal(set_scale_breaks(c(0, 10)), c(0, 10))
})
