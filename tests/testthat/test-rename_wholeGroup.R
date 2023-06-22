test_that("wholeGroup can be renamed", {
  plot_dat <- list(dat_1 = data.frame(col_1 = c("wholeGroup", "notwholeGroup"),
                                      col_2 = c("wholeGroup", NA)
  ),
  dat_2 = data.frame(col_1 = c("wholeGroup", "notwholeGroup"),
                     col_2 = c("wholeGroup", NA)
  )
  )

  expect_equal(rename_wholeGroup(plot_dat),
               list(dat_1 = data.frame(col_1 = c("Deutschland", "notDeutschland"),
                                       col_2 = c("Deutschland", NA)
               ),
               dat_2 = data.frame(col_1 = c("Deutschland", "notDeutschland"),
                                  col_2 = c("Deutschland", NA)
               ))
  )


  expect_equal(rename_wholeGroup(plot_dat[[1]]),
               data.frame(col_1 = c("Deutschland", "notDeutschland"),
                                       col_2 = c("Deutschland", NA)
               )
  )
})
