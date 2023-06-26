test_that("complex trend comparisons can be reshaped", {
  df <- data.frame(
    "depVar" = rep("bista", 2),
    "competence_var" = rep("lesen", 2),
    "grouping_var" = rep("a", 2),
    "state_var" = rep("Berlin", 2),
    "year" = rep(2010, 2),
    "group" = c("Berlin____a.vs.b.VS.a.vs.b", "Berlin____a.vs.c.VS.a.vs.b"),
    "compare_1_Comp" = c("a.vs.b", "a.vs.c"),
    "compare_2_Comp" = c("a.vs.b", "a.vs.b"),
    "comparison" = rep("crossDiff", 2),
    "est" = c(1, 2)
  )
  reshape_dat_comp_wide(df, comp = "crossDiff", year_columns = "year")
})
## Can the columns be renamed appropriately?
