test_that("relevant rows are selected", {
  df_raw <- data.frame(
    group_var = c("a", "a", "a"),
    state_var = c("a", "a", "a"),
    competence_var = c("a", "b", "b"),
    grouping_var = factor(c("a", "a", "a")),
    point.example = c("a", "b", "b"),
    sig = c("a", "b", "b"),
    sig_ = c("a", "b", "b"),
    parameter = c("mean", "mean", "x")
  )

  df_cleaned <- clean_data(df_raw,
    all_states = c("a", "b"),
    sub_groups = "a",
    competence = "a"
  )

  expect_true(nrow(df_cleaned) == 1)
})

test_that("NAs are filled up correctly", {
  df_na <- data.frame(
    group_var = c("a.vs.b", "b", NA, "a.vs.a"),
    info_to = c(NA, NA, NA, "b")
  )
  expect_equal(fill_up_na(df_na, info_to = "info_to", filling_groups = c("a", "b"))$info_to, c("a", "b", NA, "b"))
})

test_that("NoGroup and wholeGroup are filled up correctly", {
  df_raw <- data.frame(
    group_var = c("a_wholeGroup.vs.xy", "wholeGroup_xy", NA, "a.vs.a"),
    state_var = c(NA, NA, NA, "b"),
    competence_var = rep("a", 4),
    grouping_var = factor(c(rep(NA, 3), "z")),
    parameter = rep("mean", 4)
  )

  df_cleaned <- clean_data(
    df_raw,
    all_states = "a",
    sub_groups = "xy",
    competence = "a"
  )

  expect_equal(df_cleaned$state_var, c("a", "wholeGroup", NA, "b"))
  expect_equal(df_cleaned$grouping_var, factor(c("xy", "xy", "noGroup", "z"), levels = c("z", "xy", "noGroup")))
})
