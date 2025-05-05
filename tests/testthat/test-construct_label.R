test_that("labels are build correctly", {
  df <- data.frame(
    est = c(1.34, 2.1221, 3.56, 10.15),
    se = c(0.1, 0.23, 0.47, 0.432356),
    sig_bold = c(FALSE, TRUE, FALSE, TRUE),
    sig_superscript = c(FALSE, FALSE, TRUE, TRUE)
  )

  expect_equal(
    construct_label(
      df,
      column_est = "est",
      column_se = "se",
      column_sig_bold = "sig_bold",
      column_sig_superscript = "sig_superscript",
      sig_superscript_letter = "a",
      round_est = 4,
      round_se = 5
    ),
    c("1.3400<span style='white-space: pre;'> </span> (0.10000)", "**2.1221**<span style='white-space: pre;'> </span> (0.23000)", "3.5600<sup>a</sup><span style='white-space: pre;'> </span> (0.47000)", "**10.1500**<sup>a</sup><span style='white-space: pre;'> </span> (0.43236)")
  )


  # expect_equal(
  #   construct_label(
  #     df,
  #     column_est = "est",
  #     column_se = NULL,
  #     column_sig_bold = "sig_bold",
  #     column_sig_superscript = "sig_superscript",
  #     sig_superscript_letter = "a",
  #     round_est = 4,
  #     round_se = 5
  #   ),
  #   c("1.3400", "**2.1221**", "3.5600<sup>a</sup>", "**10.1500**<sup>a</sup>")
  # )
  #
  #
  # expect_equal(
  #   construct_label(
  #     dat = df,
  #     column_est = "est",
  #     column_se = NULL,
  #     column_sig_bold = NULL,
  #     column_sig_superscript = "sig_superscript",
  #     sig_superscript_letter = "a",
  #     round_est = 4,
  #     round_se = 5
  #   ),
  #   c("1.3400", "2.1221", "3.5600<sup>a</sup>", "10.1500<sup>a</sup>")
  # )
})


test_that("NAs are converted to empty strings", {
  df <- data.frame(
    label_est = c(20, 35, NA, NA),
    label_se = c(NA, 2.3, NA, 0.6),
    p_est = c(TRUE, FALSE, TRUE, FALSE)
  )

  expect_equal(
    construct_label(df,
      column_est = "label_est",
      column_se = "label_se"
    ),
    c("20 ", "35<span style='white-space: pre;'> </span> (2.3)", " ", "<span style='white-space: pre;'> </span> (0.6)")
  )

  expect_equal(
    construct_label(df,
      column_est = "label_est",
      column_se = "label_se",
      column_sig_bold = "p_est",
    ),
    c("**20** ", "35<span style='white-space: pre;'> </span> (2.3)", " ", "<span style='white-space: pre;'> </span> (0.6)")
  )

  expect_equal(
    construct_label(df,
      column_est = "label_est",
      column_se = "label_se",
      column_sig_superscript = "p_est",
      sig_superscript_letter = "a"
    ),
    c("20<sup>a</sup> ", "35<span style='white-space: pre;'> </span> (2.3)", " ", "<span style='white-space: pre;'> </span> (0.6)")
  )
})


test_that("NAs are converted to empty strings without ()", {
  df <- data.frame(
    label_est = c(20, 35, NA, NA),
    label_se = c(NA, 2.3, NA, 0.6),
    p_est = c(TRUE, FALSE, TRUE, FALSE)
  )

  expect_equal(construct_label_2(dat = df, label_se = "label_se", round_est = 1)$label, c("", " (2.3)", "", " (0.6)"))
})
