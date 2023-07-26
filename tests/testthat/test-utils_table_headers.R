# Build example data ------------------------------------------------------
test_data <- data.frame(
  x = c(-20, -10, 0, 10),
  y = 0.5:3.5
)

column_x_coords_test <- data.frame(
  column = c("col_1", "col_2", "col_3"),
  left = c(0, -10, -20),
  middle = c(5, -5, -15),
  right = c(10, 0, -10)
)


# Column headers ----------------------------------------------------------
test_that("column_headers can be plotted", {
  p_col_headers <- ggplot2::ggplot(
    test_data,
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point() +
    plot_column_headers(column_x_coords_headers = column_x_coords_test,
                        headers = list("col_1", "col_2", "col_3"), # vector okay?
                        y_axis = c(2, 1),
                        headers_text_y = 1.5,
                        n_table_cols = 3,
                        plot_settings = plotsettings_tablebarplot()
                        )

  vdiffr::expect_doppelganger("Plot with column headers", p_col_headers)

})




# Column spanners ---------------------------------------------------------
test_that("column spanners can be plotted on first level", {
  p_col_spanner_1 <- ggplot2::ggplot(
    test_data,
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point() +
    plot_column_spanners(
      y_axis = c(2, 1),
      spanners = list(
        "spanner_1" = c(1, 2),
        "spanner_2" = 3
      ),
      column_x_coords = column_x_coords_test,
      x_axis_range = 20,
      headers_text_y = 1.5,
      spanner_y = 0.5,
      plot_settings = plotsettings_tablebarplot()
    )

  vdiffr::expect_doppelganger("Plot with column spanners lvl 1", p_col_spanner_1)
})


test_that("column spanners can be plotted on second level", {
  p_col_spanner_2 <- ggplot2::ggplot(
    test_data,
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point() +
    plot_column_spanners(
      y_axis = c(2, 1),
      spanners = list(
        "spanner_1" = c(1, 2),
        "spanner_2" = 3
      ),
      column_x_coords = column_x_coords_test,
      x_axis_range = 20,
      headers_text_y = 1.5,
      spanner_y = 0.5,
      plot_settings = plotsettings_tablebarplot()
    ) +
    plot_column_spanners(
      y_axis = c(2, 1),
      spanners = list(
        "spanner_3" = c(1),
        "spanner_4" = c(2, 3)
      ),
      column_x_coords = column_x_coords_test,
      x_axis_range = 20,
      headers_text_y = 2.5,
      spanner_y = 0.5,
      plot_settings = plotsettings_tablebarplot()
    )

  vdiffr::expect_doppelganger("Plot with column spanners lvl 1 and 2", p_col_spanner_2)
})


test_that("Column spanners work in whole plot", {
test_data <- data.frame(
  state_var = 1:4,
  x_min = rep(0, 4),
  x_max = c(10, -20, 40, 30),
  est_1 = c(12, 12, 15, 23),
  se_1 = c(12, 10, 8, 4),
  bar_sig = c("TRUE", "FALSE", "TRUE", "FALSE"),
  bar_fill = c("a", "b", "c", "d")
)

p_all_headers <- plot_tablebar(
  dat = test_data,
  headers = list("est_1", "est_2", "a barplot"),
  column_spanners = list("spanner_2" = c(2,3), "spanner_1" = 1),
  column_spanners_2 = list("spanner_3" = 3, "spanner_2" = c(1,2)),
  columns_table = list("est_1", "se_1"),
  bar_est = "est_1",
  y_axis = "state_var",
  plot_settings = plotsettings_tablebarplot(headers_font_size = 3)
)

  vdiffr::expect_doppelganger("Plot with all header levels", p_all_headers)

## Lower dimension should always be supplied first:
  expect_error(plot_tablebar(
    dat = test_data,
    headers = list("est_1", "est_2", "a barplot"),
    column_spanners_2 = list("spanner_3" = 3, "spanner_2" = c(1,2)),
    columns_table = list("est_1", "se_1"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(headers_font_size = 3)
  )
  )

})


