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
  plot_settings = plotsettings_tablebarplot(columns_width = c(1)                                        )
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
  expect_equal(check_length("a", 3, fill = "a")
  , list("a", "a", "a"))
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
    columns_headers = c("est_1", "est_2"),
    columns_table = list("est_1", "se_1"),
    columns_table_sig_bold = list(NULL, "bar_sig"),
    columns_table_sig_high = list("bar_sig", "bar_sig"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      background_stripes_colour = c("white", "lightgrey", "white", "darkgrey"),
      bar_fill_colour = c("red", "blue", "yellow", "green"),
      bar_pattern_fill_colour = "white",
      bar_pattern_type = c("stripe", "none"),
      bar_sig_type = "pattern",
      columns_width = c(0.3, 0.2, 0.5),
      pattern_width = 0.5,
      pattern_spacing = 0.1
    )
  )

  # wenn nicht benannt, dann benennen der Farbsettings
  vdiffr::expect_doppelganger("Minimal_tablebar", p_bar)
})


test_that("Example barplot is plotted correctly", {

  dat_bar <- prep_plot(min_stand,
    competence = "lesen",
    parameter = "1"
  )[["plot_lines"]]


  dat_bar$est_point_end <- dat_bar$est_point_end * 100
  dat_bar <- subset(dat_bar, year_end == 2021)
  dat_bar <- subset(dat_bar, year_start == 2016)

  dat_bar$sig_minstand <- ifelse(dat_bar$sig_point_end == "TRUE" & dat_bar$est_trend_comp < 0,
                                   "below",
                                   ifelse(dat_bar$sig_point_end == "TRUE" & dat_bar$est_trend_comp > 0,
                                          "above",
                                          "no_sig")
  )

# Plot 1 ------------------------------------------------------------------
  dat_bar_1 <- subset(dat_bar, depVar == "minVerfehlt")
  dat_bar_1$sig_point_end[1:10] <- "FALSE"


  p_bar_1 <- plot_tablebar(
    dat = dat_bar_1,
    bar_label = "est_point_end",
    bar_label_sig = "sig_point_end",
    bar_sig = "sig_minstand",
    bar_header = "Mindeststandard nicht erreicht (MSA)",
    columns_headers = list("Land"),
    columns_table = list("state_var"),
    bar_est = "est_point_end",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      columns_width = c(0.2, 0.8),
      default_list = barplot_MinSta
    )
  )

  vdiffr::expect_doppelganger("MinStandard", p_bar_1)
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
  dat_bar$se_trend_no_comp <- dat_bar$se_trend_no_comp * 100
  dat_bar$se_trend_no_comp <- construct_label(dat_bar, label_se = "se_trend_no_comp")

  p_bar <- plot_tablebar(
    dat = dat_bar,
    bar_label = NULL,
    bar_sig = "sig_trend_no_comp",
    bar_header = " ", # Zu column headers dazu
    bar_fill = "depVar",
    columns_headers = list("Land", " ", "%", "%", "%", "(SE)"),
    column_spanners = list("2011" = 3,
                           "2016" = 4,
                           "Differenz 2016 - 2011" = c(5,6)
                           ),
    columns_table = list("state_var",
                         "depVar",
                         "est_point_start",
                         "est_point_end",
                         "est_trend_no_comp",
                         "se_trend_no_comp"),
    columns_table_sig_bold = list(NULL,
                                  NULL,
                                  NULL,
                                  NULL,
                                  "sig_trend_no_comp",
                                  NULL),
    columns_table_sig_high = list(NULL,
                                  NULL,
                                  NULL,
                                  NULL,
                                  "sig_trend_comp",
                                  NULL),
    bar_est = "est_trend_no_comp",
    y_axis = "y_axis_new",
    plot_settings = plotsettings_tablebarplot(default_list = barplot_MinSta_trend)
  )


  vdiffr::expect_doppelganger("MinStandard_long", p_bar)
})
