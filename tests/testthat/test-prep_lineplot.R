test_that("groups are extracted correctly", {

  expect_equal(get_group(c("a.al", "stb3" ,"s.4.r"), c("a", "4")), c(TRUE, FALSE, TRUE))

})

data_plot <- prep_lineplot(trend_books, grouping_var = "KBuecher_imp3", competence = "GL", sig_niveau = 0.05)
