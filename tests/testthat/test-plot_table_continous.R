test_that("column x coords are calced correctly", {
  expect_equal(
    calc_column_coords(
      plot_borders = c(-10, 10),
      columns_table = c("col_1", "col_2", "col_3"),
      plot_settings = plotsettings_tablebarplot(columns_width = c(0.1, 0.1, 0.1))
    ),
    data.frame(
      column = c("bar", "col_3", "col_2", "col_1"),
      left = c(-10, -12, -14, -16),
      middle = c(0, -11, -13, -15),
      right = c(10, -10, -12, -14)

    )
  )
})


test_that("column length is checked correctly", {
  column_set <- list("a", "b")
  expect_no_warning(check_length(column_set, leng = 2))

  expect_warning(check_length(column_set, 3))
  expect_equal(suppressWarnings({
    check_length(column_set, 3)
  }), list("a", "b", NULL))
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
    # bar_pattern_fill = "bar_pattern_fill",
    columns_headers = c("est_1", "est_2"),
    columns_table = list("est_1", "se_1"),
    columns_table_sig_bold = list(NULL, "bar_sig"),
    columns_table_sig_high = list("bar_sig", "bar_sig"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      background_stripes_colour = c("white", "lightgrey", "white", "darkgrey"),
      bar_fill_colour = c("red", "blue", "green", "yellow"),
      bar_pattern_fill_colour = "white",
      bar_pattern_type = c("stripe", "none"),
      bar_sig_type = "pattern",
      columns_width = c(0.15, 0.15)
    )
  )

  # wenn nicht benannt, dann benennen der Farbsettings
  vdiffr::expect_doppelganger("Minimal tablebar", p_bar)
})


test_that("Example barplot is plotted correctly", {
  dat_bar <- prep_plot(min_stand,
    competence = "lesen",
    parameter = "1"
  )[["plot_lines"]]


  dat_bar$est_point_start <- dat_bar$est_point_start * 100
  dat_bar <- subset(dat_bar, depVar == "minVerfehlt")
  dat_bar <- subset(dat_bar, year_end == 2021)
  dat_bar <- subset(dat_bar, year_start == 2016)
  dat_bar$sig_point_end[1:10] <- "FALSE"

  p_bar <- plot_tablebar(
    dat = dat_bar,
    bar_label = NULL,
    bar_sig = "sig_point_end",
    bar_header = "a barplot",
    bar_fill = "grouping_var",
    columns_headers = list("state_var", "est_1", "est_2"),
    columns_table = list("state_var", "est_point_start", "est_point_end"),
    columns_table_sig_bold = list(NULL, "sig_point_start", "sig_point_end"),
    columns_table_sig_high = list(NULL, "sig_point_start", "sig_point_end"),
    bar_est = "est_point_start",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(0, 35),
      background_stripes_colour = c(rep(c("white", "lightgrey"), 8), "darkgrey"),
      bar_fill_colour = c("darkblue"),
      bar_frame_linetype = c(`TRUE` = "solid", `FALSE` = "dashed"),
      bar_pattern_fill_colour = c("yellow"),
      bar_pattern_type = c("TRUE" = "stripe", "FALSE" = "none"),
      bar_sig_type = "frame",
      columns_width = c(0.15, 0.05, 0.05)
    )
  )

  vdiffr::expect_doppelganger("MinStandard", p_bar)
})

test_that("Example barplot long format is plotted correctly", {
  dat_bar <- prep_plot(min_stand,
    competence = "lesen",
    parameter = "1"
  )[["plot_tablebar"]]


  dat_bar <- dat_bar[which(dat_bar$year_start == 2011 & dat_bar$year_end == 2016), ]

  dat_bar$est_point_start <- dat_bar$est_point_start * 100
  dat_bar$est_point_end <- dat_bar$est_point_end * 100
  dat_bar$est_trend_no_comp <- dat_bar$est_trend_no_comp * 100
  dat_bar$sig_point_start[1:10] <- "FALSE"
  dat_bar$y_axis_new <- paste0(dat_bar$state_var, dat_bar$depVar)

  p_bar <- plot_tablebar(
    dat = dat_bar,
    bar_label = NULL,
    bar_sig = "sig_trend_no_comp",
    bar_header = "a barplot", # Zu column headers dazu
    bar_fill = "depVar",
    columns_headers = list("Land", " ", "%", "%"),
    column_spanners = list("Land" = c(1,2),
      "2009" = 3,
                           "bar" = c(4,5)),
    columns_table = list("state_var", "depVar", "est_point_start", "est_point_end"),
    columns_table_sig_bold = list(NULL, NULL, "sig_point_start", "sig_point_end"),
    columns_table_sig_high = list(NULL, NULL, "sig_point_start", "sig_point_end"),
    bar_est = "est_trend_no_comp",
    y_axis = "y_axis_new",
    plot_settings = plotsettings_tablebarplot(default_list = barplot_MinSta)
  )


  vdiffr::expect_doppelganger("MinStandard_long", p_bar)
})
