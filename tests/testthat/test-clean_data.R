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
    my_grouping = factor(c("a", "a", "a")),
    point.example = c("a", "b", "b"),
    sig = c("a", "b", "b"),
    sig_ = c("a", "b", "b"),
    parameter = c("mean", "mean", "x")
  )

  df_cleaned <- clean_data(df_raw,
    states = c("a", "b"),
    sub_groups = "a",
    competence = "a",
    grouping_var = "my_grouping"
  )

  expect_true(nrow(df_cleaned) == 1)
})

test_that("columns are named correctly", {
  df_raw <- data.frame(
    group = "a",
    TR_BUNDESLAND = "a",
    kb = "a",
    my_grouping = factor("a"),
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
    grouping_var = factor(c(rep(NA, 3), "z"))
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
  expect_equal(df_cleaned$grouping_var, factor(c("xy", "xy", "noGroup", "z"), levels = c("z", "xy", "noGroup")))
})


test_that("grouping levels are build correctly", {
  expect_warning(
    recode_to_factor(c("a", NA, "z"),
      grouping_var = "my_grouping"
    ),
    "Your grouping variable 'my_grouping' is not a factor. It will be sorted alphabetically, which might result in an unwanted factor order. Please recode your grouping variable into a factor with another level order prior to using this prep-function, if necessary."
  )
  expect_equal(suppressWarnings({
    recode_to_factor(c("a", NA, "z"), grouping_var = "")
  }), factor(c("a", "noGroup", "z"), levels = c("a", "z", "noGroup")))
  expect_equal(
    recode_to_factor(
      factor(c("a", "z", NA),
        levels = c("z", "a")
      ),
      grouping_var = ""
    ),
    factor(c("a", "z", "noGroup"),
      levels = c("z", "a", "noGroup")
    )
  )
  expect_equal(
    recode_to_factor(c(NA, NA), grouping_var = ""),
    factor(c("noGroup", "noGroup"), levels = "noGroup")
  )
})

test_that("grouping_var is build if it is not part of the data", {
  df_raw <- data.frame(
    group_var = c("a_wholeGroup.vs.xy", "wholeGroup_xy", NA, "a.vs.a"),
    state_var = c(NA, NA, NA, "b"),
    kb = rep("a", 4)
  )

  expect_equal(
    clean_data(df_raw,
      states = "a",
      group_var = "group_var",
      state_var = "state_var",
      sub_groups = "xy",
      competence = "a",
      grouping_var = ""
    )[, "grouping_var"],
    factor(c("xy", "xy", "noGroup", "noGroup"), levels = c("xy", "noGroup"))
  )
})
