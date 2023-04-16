test_that("column length is checked correctly", {

  column_set <- list("a", "b")
  expect_no_warning(check_length(column_set, leng = 2))

  expect_warning(check_length(column_set, 3))
  expect_equal(suppressWarnings({check_length(column_set, 3)}), list("a", "b", NULL))
  })

test_that("continous barplot looks the same", {
  test_data <- data.frame(
    state_var = 1:4,
    x_min = rep(0, 4),
    x_max = c(10, -20, 40, 30),
    est_1 = c(12, 12, 15, 23),
    se_1 = c(12, 10, 8, 4),
    bar_sig = c("TRUE", "FALSE", "TRUE", "FALSE"),
    bar_fill = c("a", "b", "c", "a"),
    bar_pattern_fill = c("a", "a", "b", "b")
  )

  p_bar <- plot_tablebar(
    dat = test_data,
    bar_label = NULL,
    bar_sig = "bar_sig",
    bar_header = "a barplot",
    bar_fill = "bar_fill",
    bar_pattern_fill = "bar_pattern_fill",
    columns_headers = c("est_1", "est_2"),
    columns_table = list("est_1", "se_1"),
    columns_table_sig_bold = list(NULL, "bar_sig"),
    columns_table_sig_high = list("bar_sig", "bar_sig"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      background_stripes_colour = c("white", "lightgrey", "white", "darkgrey"),
      bar_fill_colour = c("a" = "red", "b" = "blue", "c" = "green"),
      bar_pattern_fill_colour = c("a" = "black", "b" = "white"),
      bar_pattern_type = c("stripe", "none"),
      bar_sig_type = "pattern",
      columns_width = c(0.05, 0.05)
    )
  )

  vdiffr::expect_doppelganger("Minimal tablebar", p_bar)
})


test_that("Example barplot is plotted correctly", {

  min_stand <- readxl::read_xlsx("Q:/BT2022/BT/60_Bericht/_Probegrafiken/2023-01-26 Vorlagen Balken und Linien/BT2021_Abb3.9.xlsx", sheet = "Daten BT21")

  dat_bar <- prep_trend(min_stand,
                              competence = "lesen",
                              parameter = "1")[["plot_bar"]]

  dat_bar$est_no_comp <- dat_bar$est_no_comp *100
  dat_bar <- subset(dat_bar, depVar == "minVerfehlt")
  dat_bar <- subset(dat_bar, year == 2021)
  dat_bar$sig_point_no_comp[1:10] <- "FALSE"

  p_bar <- plot_tablebar(
    dat = dat_bar,
    bar_label = NULL,
    bar_sig = "sig_point_no_comp",
    bar_header = "a barplot",
    bar_fill = "grouping_var",
    bar_pattern_fill = "grouping_var",
    columns_headers = list("state_var","est_1", "est_2"),
    columns_table = list("state_var", "est_no_comp", "est_comp_whole"),
    columns_table_sig_bold = list(NULL, NULL, "sig_point_no_comp"),
    columns_table_sig_high = list(NULL, "sig_point_no_comp", "sig_point_comp_whole"),
    bar_est = "est_no_comp",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(0, 35),
      background_stripes_colour = c(rep(c("white", "lightgrey"), 8), "darkgrey"),
      bar_fill_colour = c("darkblue"),
      bar_frame_linetype = c(`TRUE` = "solid", `FALSE` = "dashed"),
      bar_pattern_fill_colour = c("yellow"),
      bar_pattern_type = c("TRUE" = "stripe", "FALSE" = "none"),
      bar_sig_type = "frame",
      columns_width = c(0.1, 0.1, 0.2)
    )
  )

  vdiffr::expect_doppelganger("MinStandard", p_bar)


  })




test_that("Example barplot long format is plotted correctly", {

  min_stand <- readxl::read_xlsx("Q:/BT2022/BT/60_Bericht/_Probegrafiken/2023-01-26 Vorlagen Balken und Linien/BT2021_Abb3.9.xlsx", sheet = "Daten BT21")

  dat_bar <- prep_trend(min_stand,
                        competence = "lesen",
                        parameter = "1")[["plot_bar"]]

  dat_bar$est_no_comp <- dat_bar$est_no_comp *100
  #dat_bar <- subset(dat_bar, depVar == "minVerfehlt")
  dat_bar <- subset(dat_bar, year == 2021)
  dat_bar$sig_point_no_comp[1:10] <- "FALSE"
  dat_bar$y_axis_new <- paste0(dat_bar$state_var, dat_bar$depVar)

  p_bar <- plot_tablebar(
    dat = dat_bar,
    bar_label = NULL,
    bar_sig = "sig_point_no_comp",
    bar_header = "a barplot",
    bar_fill = "depVar",
    bar_pattern_fill = "grouping_var",
    columns_headers = list("state_var", "depVar","est_1", "est_2"),
    columns_table = list("state_var", "depVar", "est_no_comp", "est_comp_whole"),
    columns_table_sig_bold = list(NULL, NULL, NULL, "sig_point_no_comp"),
    columns_table_sig_high = list(NULL, NULL, "sig_point_no_comp", "sig_point_comp_whole"),
    bar_est = "est_no_comp",
    y_axis = "y_axis_new",
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(0, 35),
      background_stripes_colour = c(rep(c("white", "white", "white", "lightgrey", "lightgrey", "lightgrey"), 8), rep("darkgrey", 3)),
      bar_fill_colour = c("darkblue", "lightblue", "lightgreen"),
      bar_frame_linetype = c(`TRUE` = "solid", `FALSE` = "dashed"),
      bar_pattern_fill_colour = c("yellow"),
      bar_pattern_type = c("TRUE" = "stripe", "FALSE" = "none"),
      bar_sig_type = "frame",
      bar_width = 0.7,
      headers_nudge_y = 1,
      columns_width = c(0.5, 0.3, 0.2, 0.2)
    )
  )

  vdiffr::expect_doppelganger("MinStandard_long", p_bar)


})

