test_that("groups are extracted correctly", {

  expect_equal(get_group(c("a.al", "stb3" ,"s.4.r"), c("a", "4")), c(TRUE, FALSE, TRUE))

})


test_that("grouping levels are build correctly", {
  expect_equal(
    recode_to_factor(
      factor(c("a", "z", NA),
             levels = c("z", "a")
      )),
    factor(c("a", "z", "noGroup"),
           levels = c("z", "a", "noGroup")
    )
  )
  expect_equal(
    recode_to_factor(factor(c(NA, NA))),
    factor(c("noGroup", "noGroup"), levels = "noGroup")
  )
})


