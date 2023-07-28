# Build example data ------------------------------------------------------
test_data <- data.frame(
  x = c(-20, -10, 0, 10),
  y = 0.5:3.5
)

test_data_full <- data.frame(
  state_var = 1:4,
  x_min = rep(0, 4),
  x_max = c(10, -20, 40, 30),
  est_1 = c(12, 12, 15, 23),
  se_1 = c(12, 10, 8, 4),
  bar_sig = c("TRUE", "FALSE", "TRUE", "FALSE"),
  bar_fill = c("a", "b", "c", "d")
)

column_x_coords_test <- data.frame(
  column = c("col_1", "col_2", "col_3"),
  left = c(0, -10, -20),
  middle = c(5, -5, -15),
  right = c(10, 0, -10)
)

plot_settings_full <- plotsettings_tablebarplot(
  headers_nudge_y = c(-1, 0, 1),
  headers_row_height = 3,
  column_spanners_row_height = 1,
  column_spanners_2_row_height = 2
)


# Column headers ----------------------------------------------------------
test_that("column_headers can be plotted", {
  plot_settings_headers <- plotsettings_tablebarplot(
    headers_nudge_y = c(-1, 0, 1),
    headers_row_height = 3
  )

  p_col_headers <- ggplot2::ggplot(
    test_data,
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point() +
    plot_column_headers(column_x_coords_headers = column_x_coords_test,
                        headers = list("col_1", "col_2", "col_3"), # vector okay?
                        header_y_coords = set_header_y_coords(y_axis = c(3.5:0.5),
                                                              plot_settings_headers),
                        n_table_cols = 3,
                        plot_settings = plot_settings_headers)

  vdiffr::expect_doppelganger("Plot with column headers", p_col_headers)

})




# Column spanners ---------------------------------------------------------
test_that("column spanners can be plotted on first level", {

  plot_settings_spanners <- plotsettings_tablebarplot(
    column_spanners_row_height = 2,
    column_spanners_2_nudge_y = c(0, 0),
    column_spanners_nudge_y = c(1, 0)
  )

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
      header_y_coords = set_header_y_coords(y_axis = c(3.5:0.5),
                                            plot_settings_spanners),
      plot_settings = plot_settings_spanners
    )

  vdiffr::expect_doppelganger("Plot with column spanners lvl 1", p_col_spanner_1)
})


test_that("column spanners can be plotted on second level", {

  plot_settings_spanners_2 <- plotsettings_tablebarplot(
    column_spanners_2_row_height = 3,
    column_spanners_2_nudge_y = c(0, 0),
    column_spanners_nudge_y = c(0, 0)
  )

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
      header_y_coords = set_header_y_coords(y_axis = c(3.5:0.5), plot_settings = plot_settings_spanners_2),
      plot_settings = plot_settings_spanners_2
    ) +
    plot_column_spanners(
      y_axis = c(2, 1),
      spanners = list(
        "spanner_3" = c(1),
        "spanner_4" = c(2, 3)
      ),
      column_x_coords = column_x_coords_test,
      x_axis_range = 20,
      header_y_coords = set_header_y_coords(y_axis = c(3.5:0.5), plot_settings = plot_settings_spanners_2),
      spanners_2 = TRUE,
      plot_settings = plot_settings_spanners_2
    )

  vdiffr::expect_doppelganger("Plot with column spanners lvl 1 and 2", p_col_spanner_2)
})

test_that("Column spanners work in whole plot", {

p_all_headers <- plot_tablebar(
  dat = test_data_full,
  headers = list("est_1", "est_2", "a barplot"),
  column_spanners = list("spanner_1" = 1, "spanner_2" = c(2,3)),
  column_spanners_2 = list("spanner_2" = c(1,2) ,"spanner_3" = 3),
  columns_table = list("est_1", "se_1"),
  bar_est = "est_1",
  y_axis = "state_var",
  plot_settings = plotsettings_tablebarplot(headers_font_size = 3,
                                            default_list = plot_settings_full)
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

test_that("Column spanners over multiple lines work in whole plot", {
  p_all_headers <- plot_tablebar(
    dat = test_data_full,
    headers = list("est_1 <br> newline <br> new line", "est_2", "a barplot"),
    column_spanners = list("spanner_1" = 1, "spanner_2 <br> new line<br> new line<br> new line<br> new line" = c(2,3)),
    column_spanners_2 = list("spanner_2" = c(1,2), "spanner_3<br> new line<br> new line<br> new line<br> new line<br> new line" = 3),
    columns_table = list("est_1", "se_1"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(headers_font_size = 3,
                                              headers_nudge_y = c(3, 0, 0),
                                              headers_row_height = 1,
                                              column_spanners_row_height = 2,
                                              column_spanners_nudge_y = c(-0.4, 0),
                                              column_spanners_2_row_height = 4,
                                              column_spanners_2_nudge_y = c(-1, 0)
                                              )
  )

  vdiffr::expect_doppelganger("Plot with all header levels and linebreaks", p_all_headers)


})
