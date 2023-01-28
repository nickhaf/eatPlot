test_that("Axis are labeled correctly", {
  df <- data.frame(
    x_labels = factor(c("Adjusted", "Adjusted", "y_label", "y_label"), levels = c("y_label", "Adjusted"), ordered = TRUE),
    group = c("Berlin", "Brandenburg", "Berlin", "Brandenburg"),
    value = c("12", "20", "14", "15")
  )

  p1 <- ggplot2::ggplot_build(ggplot2::ggplot(data = df, ggplot2::aes(x = .data$x_labels, y = .data$group, label = .data$value)) +
    build_columns(data_prep = df))

  expect_equal(p1$layout$panel_params[[1]]$x$get_labels(), c("y_label", "Adjusted"))



  df_2 <- data.frame(
    x_labels = factor(c("y_label", "y_label", "col_1", "col_1", "col_2", "col_2", "col_3", "col_3"), levels = c("y_label", "col_3", "col_2", "col_1"), ordered = TRUE),
    group = rep(c("Berlin", "Brandenburg"), 4),
    value = c(NA, NA, 1:6)
  )

  p2 <- ggplot2::ggplot_build(ggplot2::ggplot(data = df_2, ggplot2::aes(x = .data$x_labels, y = .data$group, label = .data$value)) +
    build_columns(data_prep = df_2))

  expect_equal(p2$layout$panel_params[[1]]$x$get_labels(), c("y_label", "col_3", "col_2", "col_1"))
  expect_equal(p2$layout$panel_params[[1]]$y$get_labels(), c("Berlin", "Brandenburg"))
})


# Test graphical output ---------------------------------------------------
skip(test_that("Table plot is still the same", {
prep_dat <- prep_tableplot(adjusted_means, columns = "adjust", competence = "GL", sig_niveau = 0.05)
vdiffr::expect_doppelganger("Table plot", plot_table(prep_dat))
})
)
