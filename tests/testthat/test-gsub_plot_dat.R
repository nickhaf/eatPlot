test_that("wholeGroup can be renamed", {
  plot_dat <- list(dat_1 = data.frame(col_1 = c("wholeGroup", "notwholeGroup"),
                                      col_2 = c("wholeGroup", NA),
                                      col_3 = c(1, 2)
  ),
  dat_2 = data.frame(col_1 = c("wholeGroup", "notwholeGroup"),
                     col_2 = c("wholeGroup", NA),
                     col_3 = c(TRUE, FALSE)
  )
  )

  expect_equal(gsub_plot_dat(plot_dat),
               list(dat_1 = data.frame(col_1 = c("Deutschland", "notDeutschland"),
                                       col_2 = c("Deutschland", NA),
                                       col_3 = c(1, 2)
               ),
               dat_2 = data.frame(col_1 = c("Deutschland", "notDeutschland"),
                                  col_2 = c("Deutschland", NA),
                                  col_3 = c(TRUE, FALSE)
               ))
  )


  expect_equal(gsub_plot_dat(plot_dat[[1]]),
               data.frame(col_1 = c("Deutschland", "notDeutschland"),
                          col_2 = c("Deutschland", NA),
                          col_3 = c(1, 2)
               )
  )
})
