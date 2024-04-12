test_that("labels are build correctly", {
  df <- data.frame(
    label_est = c(1.34, 2.1221, 3.56, 10.15),
    label_se = c(0.1, 0.23, 0.47, 0.432356),
    label_sig_bold = c(FALSE, TRUE, FALSE, TRUE),
    label_sig_high = c(FALSE, FALSE, TRUE, TRUE)
  )

  expect_equal(
    construct_label(df,
      label_est = "label_est",
      label_se = "label_se",
      label_sig_bold = "label_sig_bold",
      label_sig_high = "label_sig_high",
      round_est = 4, round_se = 5
    )$label,
    c("1.3400 (0.10000)", "**2.1221** (0.23000)", "3.5600<sup>a</sup> (0.47000)", "**10.1500**<sup>a</sup> (0.43236)")
  )


  expect_equal(
    construct_label(df,
      label_est = "label_est",
      label_se = NULL,
      label_sig_bold = "label_sig_bold",
      label_sig_high = "label_sig_high",
      round_est = 4, round_se = 5
    )$label,
    c("1.3400", "**2.1221**", "3.5600<sup>a</sup>", "**10.1500**<sup>a</sup>")
  )


  expect_equal(
    construct_label(df,
      label_est = "label_est",
      label_se = NULL,
      label_sig_bold = NULL,
      label_sig_high = "label_sig_high",
      round_est = 4, round_se = 5
    )$label,
    c("1.3400", "2.1221", "3.5600<sup>a</sup>", "10.1500<sup>a</sup>")
  )
})


test_that("NAs are converted to empty strings", {
  df <- data.frame(
    label_est = c(20, 35, NA, NA),
    label_se = c(NA, 2.3, NA, 0.6),
    p_est = c(TRUE, FALSE, TRUE, FALSE)
  )

  expect_equal(
    construct_label(df,
      label_est = "label_est",
      label_se = "label_se"
    )$label,
    c("20 ()", "35 (2.3)", " ()", " (0.6)")
  )

  expect_equal(
    construct_label(df,
      label_est = "label_est",
      label_se = "label_se",
      label_sig_bold = "p_est",
    )$label,
    c("**20** ()", "35 (2.3)", " ()", " (0.6)")
  )

  expect_equal(
    construct_label(df,
      label_est = "label_est",
      label_se = "label_se",
      label_sig_high = "p_est",
    )$label,
    c("20<sup>a</sup> ()", "35 (2.3)", " ()", " (0.6)")
  )
})