test_that("Axis are labeled correctly", {
  df <- data.frame(
    x_labels = factor(c("Adjusted", "Adjusted", "y_label", "y_label"), levels = c("y_label", "Adjusted"), ordered = TRUE),
    group_var = c("Berlin", "Brandenburg", "Berlin", "Brandenburg"),
    value = c("12", "20", "14", "15")
  )

  p1 <- ggplot2::ggplot_build(ggplot2::ggplot(data = df, ggplot2::aes(x = .data$x_labels, y = .data$group_var, label = .data$value)) +
    build_columns(df, x_axis = "group_var"))

  expect_equal(p1$layout$panel_params[[1]]$x$get_labels(), c("y_label", "Adjusted"))
})


# Test graphical output ---------------------------------------------------
test_that("Table plot is still the same", {
  df <- data.frame(
    x_label = factor(c("Adjusted", "Adjusted", "notAdjusted", "notAdjusted"), levels = c("notAdjusted", "Adjusted"), ordered = TRUE),
    state_var = c("Berlin", "Brandenburg", "Berlin", "Brandenburg"),
    y_value = c("12", "20", "14", "15")
  )

  vdiffr::expect_doppelganger("Table plot", plot_table(df,
    x_axis = "x_label",
    y_value = "y_value",
    plot_settings = plotsettings_barplot(default_list = barplot_MinSta)
  ))
})

test_that("example plot is still the same", {
  plot_data <- prep_no_trend(
    dat = adjusted_means,
    grouping_var = "adjust",
    columns = "adjust",
    competence = "GL",
    sig_niveau = 0.05
  )

  vdiffr::expect_doppelganger("Table plot trend_books", plot_table(plot_data[["plot_table"]],
    plot_settings = plotsettings_barplot(default_list = barplot_MinSta)
  ))
})
