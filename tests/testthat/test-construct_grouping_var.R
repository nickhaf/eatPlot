test_that("multiple grouping_vars can be merged into one", {
  df_test <- data.frame(
    group_1 = c("a", "b", "a.vs.b", NA, "b"),
    group_2 = c("1", "1", "2", "2", NA),
    group_var = c("a.vs.b", "b.vs.a", "a.vs.wholeGroup", "b.vs.wholeGroup", "Berlin")
  )

  expect_equal(
    paste_grouping_vars(df_test,
      grouping_vars = c("group_1", "group_2"),
      group_var = "group_var"
    )$group_var,
    c("a-1.vs.b-1", "b-1.vs.a-1", "a-2.vs.wholeGroup", "b-2.vs.wholeGroup", "Berlin")
  )

  expect_equal(
    paste_grouping_vars(df_test,
      grouping_vars = c("group_1", "group_2"),
      group_var = "group_var"
    )$grouping_var,
    c("a-1", "b-1", "a-2.vs.b-2", NA, "b")
  )

  expect_equal(
    paste_grouping_vars(df_test,
      grouping_vars = c("group_1"),
      group_var = "group_var"
    )$grouping_var,
    factor(c("a", "b", "a.vs.b", NA, "b"))
  )
})


test_that("grouping_vars are deducted from group-columns", {
  df_raw <- data.frame(
    group_var = c("Berlin_0.vs.Brandenburg", "wholeGroup", NA, "Berlin_1.vs.1", "0.vs.1"),
    state_var = c(NA, NA, NA, "b", NA),
    competence_var = rep("a", 5),
    my_groups = factor(c("0", rep(NA, 2), "1", NA)),
    parameter = rep("mean", 5)
  )

df_grouping_var <- construct_grouping_var(df_raw, grouping_vars = "my_groups", group_var = "group")

expect_equal(df_grouping_var$grouping_var, factor(c("0", "noGroup", "noGroup", "1", "0"), levels = c("0", "1", "noGroup")))

})


test_that("remaining NAs are filled up", {
  df <- data.frame(
    group_1 = c(NA, "a", NA, "a", NA),
    group_2 = c("a", NA, NA, "b", "c"),
    grouping_var = c(NA, NA, NA, NA, "a")
  )

  expect_equal(fill_grouping_na(df, grouping_vars = c("group_1", "group_2"))$grouping_var,
               factor(c("a", "a", NA, NA, "a"))
  )
})

test_that("correct recoding to factor", {
  expect_equal(
    recode_to_factor(
      factor(c("a", "z", NA),
             levels = c("z", "a")
      )
    ),
    factor(c("a", "z", "noGroup"),
           levels = c("z", "a", "noGroup")
    )
  )
  expect_equal(
    recode_to_factor(factor(c(NA, NA))),
    factor(c("noGroup", "noGroup"), levels = "noGroup")
  )
})

