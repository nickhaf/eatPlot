test_that("column x coords are calced correctly", {
  expect_equal(
    calc_column_coords(
      plot_borders = c(-10, 10),
      columns_table = c("col_1", "col_2", "col_3"),
      plot_settings = plotsettings_tablebarplot(columns_width = c(0.3, 0.1, 0.2, 0.4))
    ),
    data.frame(
      column = c("bar", "col_3", "col_2", "col_1"),
      left = c(-10, -20, -25, -40),
      middle = c(0.0, -15.0, -22.5, -32.5),
      right = c(10, -10, -20, -25)
    )
  )


  ## Only Columns
  expect_equal(
    calc_column_coords(
      plot_borders = c(0, 0),
      columns_table = c("col_1", "col_2", "col_3"),
      plot_settings = plotsettings_tablebarplot(columns_width = c(0.3, 0.3, 0.4))
    ),
    data.frame(
      column = c("col_3", "col_2", "col_1"),
      left = c(0.6, 0.3, 0),
      middle = c(0.8, 0.45, 0.15),
      right = c(1.0, 0.6, 0.3)
    )
  )

  ## Only Bar
  expect_equal(
    calc_column_coords(
      plot_borders = c(-10, 10),
      columns_table = NULL,
      plot_settings = plotsettings_tablebarplot(columns_width = c(1))
    ),
    data.frame(
      column = c("bar"),
      left = c(-10),
      middle = c(0),
      right = c(10)
    )
  )
})

test_that("column length is checked correctly", {
  column_set <- list("a", "b")
  expect_equal(check_length(column_set, leng = 2), list("a", "b"))

  expect_error(check_length(column_set, 3))
  expect_equal(
    check_length("a", 3, fill = "a"),
    c("a", "a", "a")
  )
})

test_that("continous barplot looks the same", {
  test_data <- data.frame(
    state_var = 1:4,
    x_min = rep(0, 4),
    x_max = c(10, -20, 40, 30),
    est_1 = c(12, 12, 15, 23),
    se_1 = c(12, 10, 8, 4),
    bar_sig = c("TRUE", "FALSE", "TRUE", "FALSE"),
    bar_fill = c("a", "b", "c", "d")
  )

  p_bar <- plot_tablebar(
    dat = test_data,
    bar_label = NULL,
    bar_sig = "bar_sig",
    bar_header = "a barplot",
    bar_fill = "bar_fill",
    columns_headers = list("est_1", "est_2"),
    columns_table = list("est_1", "se_1"),
    columns_table_sig_bold = list(NULL, "bar_sig"),
    columns_table_sig_high = list("bar_sig", "bar_sig"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      background_stripes_colour = c("white", "lightgrey"),
      bar_fill_colour = c("red", "blue", "yellow", "green"),
      bar_pattern_fill_colour = "white",
      bar_pattern_type = c("stripe", "none"),
      bar_sig_type = "pattern",
      columns_width = c(0.3, 0.2, 0.5),
      bar_pattern_width = 0.5,
      bar_pattern_spacing = 0.1
    )
  )

  # wenn nicht benannt, dann benennen der Farbsettings
  vdiffr::expect_doppelganger("Minimal_tablebar", p_bar)
})
