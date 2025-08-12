test_data <- data.frame(
  state_var = 1:4,
  x_min = rep(0, 4),
  x_max = c(10, -20, 40, 30),
  est_1 = c(12, 12, 15, 23),
  se_1 = c(12, 10, 8, 4),
  bar_sig = c("TRUE", "FALSE", "TRUE", "FALSE"),
  bar_fill = c("a", "b", "c", "d")
)

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


test_that("simple tablebarplot can be plotted", {
  test_data$est_1 <- test_data$est_1/100
  example_table <- plot_tablebarplot(
    dat = test_data,
    bar_label = NULL,
    bar_sig = "bar_sig",
    bar_fill = "bar_fill",
    headers = list("est_1", "est_2", "a barplot"),
    column_spanners = list("spanner_2" = c(2, 3), "spanner_1" = 1),
    column_spanners_2 = list("spanner_3" = 3, "spanner_2" = c(1, 2)),
    columns_table = list("est_1", "se_1"),
    columns_table_sig_bold = list(NULL, "bar_sig"),
    columns_table_sig_superscript = list("bar_sig", "bar_sig"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(-0.5, 0.5),
      axis_x_stepsize = 0.1,
      background_stripes_border = "background_line_both",
      background_stripes_colour = c("white", "lightgrey"),
      bar_fill_colour = c("red", "blue", "yellow", "green"),
      bar_pattern_fill_colour = "white",
      bar_pattern_type = c("stripe", "none"),
      bar_background_lines = c(0, 0.1, 0.3),
      bar_type = "pattern",
      columns_width = c(0.3, 0.2, 0.5),
      bar_pattern_width = 0.5,
      bar_pattern_spacing = 0.1,
      headers_font_size = 5
    )
  )

  vdiffr::expect_doppelganger("Minimal_tablebarplot", example_table)
  # save_plot(example_table, "/home/nick/Downloads/table.pdf",  height = 226.2 / 3)
})


test_that("continous barplot can have a white space", {
  p_bar <- plot_tablebarplot(
    dat = test_data,
    bar_label = NULL,
    bar_sig = "bar_sig",
    bar_fill = "bar_fill",
    headers = list("est_1", "est_2", "a barplot"),
    column_spanners = list("spanner_2" = c(2, 3), "spanner_1" = 1),
    column_spanners_2 = list("spanner_3" = 3, "spanner_2" = c(1, 2)),
    columns_table = list("est_1", "se_1"),
    columns_table_sig_bold = list(NULL, "bar_sig"),
    columns_table_sig_superscript = list("bar_sig", "bar_sig"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      background_stripes_colour = c("white", "lightgrey"),
      bar_fill_colour = c("red", "blue", "yellow", "green"),
      bar_pattern_fill_colour = "white",
      bar_pattern_type = c("stripe", "none"),
      bar_type = "pattern",
      columns_width = c(0.3, 0.2, 0.5),
      bar_pattern_width = 0.5,
      bar_pattern_spacing = 0.1,
      headers_font_size = 5,
      space_right = 5
    )
  )

  # wenn nicht benannt, dann benennen der Farbsettings
  vdiffr::expect_doppelganger("Tablebar with white space", p_bar)
})

test_that("Vlines are plotted correctly", {
  plot_settings_test <- list(
    axis_x_stepsize = 1,
    bar_background_lines_spanners = list(c(1, 4), c(5, 9)),
    bar_background_lines = "scale_breaks",
    bar_background_lines_linetype = "solid",
    bar_background_lines_colour = "red",
    bar_background_0line_linetype = "solid",
    bar_background_0line_colour = "blue"
  )

  df_test <- data.frame(
    x = c(0:10),
    y = 1:11
  )

  vdiffr::expect_doppelganger(
    "row spanners",
    ggplot2::ggplot(df_test, ggplot2::aes(x, y)) +
      ggplot2::coord_cartesian(ylim = c(0, 11)) +
      add_vlines(plot_settings_test,
        bar_est = "x", plot_borders = c(0, 10), y_axis = 11:1 # has to be inverted
      )
  )


  plot_settings_test_2 <- list(
    axis_x_stepsize = 10,
    bar_background_lines_spanners = NULL,
    bar_background_lines = "border",
    bar_background_lines_linetype = "dashed",
    bar_background_lines_colour = "red",
    bar_background_0line_linetype = "solid",
    bar_background_0line_colour = "blue"
  )


  vdiffr::expect_doppelganger(
    "background lines without row spanners",
    ggplot2::ggplot(df_test, ggplot2::aes(x, y)) +
      ggplot2::coord_cartesian(ylim = c(0, 11)) +
      add_vlines(plot_settings_test_2,
        plot_borders = c(0, 10), bar_est = "x", y_axis = 11:1 # has to be inverted
      )
  )
})


test_that("labels can be plotted in different combination without error", {
  p_bar_label_colour <- plot_tablebarplot(
    dat = test_data,
    bar_label = 'est_1',
    bar_sig = "bar_sig",
    bar_fill = "bar_fill",
    headers = list("a barplot"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      background_stripes_colour = c("white", "lightgrey"),
      bar_fill_colour = c("red", "blue", "yellow", "green"),
      bar_label_colour = c("red", "blue", "yellow", "green"),
      bar_pattern_fill_colour = "white",
      bar_pattern_type = c("stripe", "none"),
      bar_type = "pattern",
      bar_pattern_width = 0.5,
      bar_pattern_spacing = 0.1,
      headers_font_size = 5,
      space_right = 5
    )
  )


  vdiffr::expect_doppelganger(
    "bar_labels get colours",
    p_bar_label_colour
  )

  p_bar_label_1colour <- plot_tablebarplot(
    dat = test_data,
    bar_label = 'est_1',
    bar_sig = "bar_sig",
    bar_fill = "bar_fill",
    headers = list("a barplot"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      background_stripes_colour = c("white", "lightgrey"),
      bar_fill_colour = c("red", "blue", "yellow", "green"),
      bar_label_colour = c("red"),
      bar_pattern_fill_colour = "white",
      bar_pattern_type = c("stripe", "none"),
      bar_type = "pattern",
      bar_pattern_width = 0.5,
      bar_pattern_spacing = 0.1,
      headers_font_size = 5,
      space_right = 5
    )
  )

  vdiffr::expect_doppelganger(
    "bar_labels get 1 colour",
    p_bar_label_colour
  )


})


test_that("stacked barplot can be plotted", {

df_stacked <- data.frame(
  bar_value = c(95, 1, 4, 95, 3, 2, 6, 54, 40),
  parameter = c("a", "b", "c", "a", "b", "c", "a", "b", "c"),
  label = c("95", "1.987", "4", "95", "3", "2.53", "6", "54", "40"),
  y_axis =c(rep(1, 3), rep(2, 3), rep(3, 3))
)

df_stacked$parameter <- factor(df_stacked$parameter, levels = c("b", "a", "c"), ordered = TRUE)

  p_stacked <- plot_tablebarplot(df_stacked,
                                 bar_est = "bar_value",
                                 bar_label = "label",
                                 bar_fill = "parameter",
                                 y_axis = "y_axis",
                                 plot_settings = plotsettings_tablebarplot(
                                   axis_x = FALSE,
                                   axis_x_lims = c(-8, 103),
                                   axis_x_stepsize = 1,
                                   bar_fill_colour = c("a" = "#20D479", "b" = "#8DEBBC", "c" = "#EBFDF3"),
                                   bar_label_colour = c('red', "black", "blue"),
                                   bar_type = "stacked"
                                 )
  )

  ## Scale x already present: Change into same function, currently set two times
})




