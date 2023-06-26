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


  df_realistic <- data.frame(
    group_var = c(
      "Hamburg_zweiteGen-ohneSPF.vs.zweiteGen-ohneSPF",
      "TR_BUNDESLAND=Baden-Wuerttemberg____ersteGen-alle.vs.zweiteGen-alle.VS.all.group=1____einET-alle.vs.zweiteGen-alle"
    ),
    grouping_var = c(NA, NA)
  )

  expect_equal(
    fill_up_na(df_realistic, info_to = "grouping_var", filling_groups = c("zweiteGen-ohneSPF", "ersteGen-alle", "einET-alle"))$grouping_var,
    c("zweiteGen-ohneSPF", "ersteGen-alle")
  )
})



test_that("NoGroup and wholeGroup are filled up correctly", {
  df_raw <- data.frame(
    group_var = c("Berlin_0.vs.Brandenburg", "wholeGroup", NA, "Berlin_1.vs.1", "0.vs.1"),
    state_var = c(NA, NA, NA, "b", NA),
    competence_var = rep("a", 5),
    grouping_var = factor(c(rep(NA, 3), "1", NA)),
    parameter = rep("mean", 5)
  )

  df_cleaned <- clean_data(
    df_raw,
    all_states = c("Berlin", "Brandenburg"),
    sub_groups = c("1", "0"),
    competence = "a"
  )

  expect_equal(df_cleaned$state_var, c("Berlin", "wholeGroup", NA, "b", "wholeGroup"))
})
