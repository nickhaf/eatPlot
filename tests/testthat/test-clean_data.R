test_that("Error messages work correctly", {
  expect_error(clean_data(data.frame("grouping" = "a"),
    states = "a",
    group_var = "groups",
    sub_groups = "a",
    competence = "a"
  ), "group_var: 'groups' not found in dat.", fixed = TRUE)

  expect_error(clean_data(data.frame("group" = "a"),
    states = "a",
    sub_groups = "a",
    competence = "a"
  ), "state_var: 'TR_BUNDESLAND' not found in dat.", fixed = TRUE)

  expect_error(clean_data(data.frame(group = "a", TR_BUNDESLAND = "a"),
    states = "a",
    sub_groups = "a",
    competence = "a"
  ), "competence_var: 'kb' not found in dat.", fixed = TRUE)
})

test_that("relevant rows are selected", {
  df_raw <- data.frame(
    group = c("a", "a", "a"),
    TR_BUNDESLAND = c("a", "a", "a"),
    kb = c("a", "b", "b"),
    my_grouping = c("a", "a", "a"),
    point.example = c("a", "b", "b"),
    sig = c("a", "b", "b"),
    sig_ = c("a", "b", "b"),
    parameter = c("mean", "mean", "x")
  )

  df_cleaned <- clean_data(df_raw, states = c("a", "b"), sub_groups = "a", competence = "a", grouping_var = "my_grouping")

  expect_true(nrow(df_cleaned) == 1)
})

test_that("columns are named correctly", {
  df_raw <- data.frame(
    group = "a",
    TR_BUNDESLAND = "a",
    kb = "a",
    my_grouping = "a",
    point.example = "a",
    sig = "a",
    sig_ = "a"
  )

  df_cleaned <- clean_data(df_raw, states = "a", sub_groups = "a", competence = "a", grouping_var = "my_grouping")

  expect_equal(colnames(df_cleaned), c("group_var", "state_var", "grouping_var", "point_example", "p", "p_"))
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
    kb = rep("a", 4),
    grouping_var = c(rep(NA, 3), "z")
  )

  df_cleaned <- clean_data(df_raw,
    states = "a",
    group_var = "group_var",
    state_var = "state_var",
    sub_groups = "xy",
    competence = "a",
    grouping_var = "my_grouping"
  )

  expect_equal(df_cleaned$state_var, c("a", "wholeGroup", NA, "b"))
  expect_equal(df_cleaned$grouping_var, factor(c("xy", "xy", "noGroup", "z")))
})
