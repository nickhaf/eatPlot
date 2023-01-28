test_that("groups are extracted correctly", {

  expect_equal(get_group(c("a.al", "stb3" ,"s.4.r"), c("a", "4")), c("a", NA, "4"))
  expect_error(get_group(c("a.a"), "a"), "Multiple groups in your grouping column.", fixed = TRUE)

})

