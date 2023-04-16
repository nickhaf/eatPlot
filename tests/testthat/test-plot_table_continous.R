test_that("continous barplot looks the same", {
  test_data <- data.frame(
    y_axis = 1:4,
    x_min = rep(0, 4),
    x_max = c(10, -20, 40, 30),
    est_1 = c("12", "12", "15", "23"),
    se_1 = c("12", "10", "8", "4"),
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
    column_bar = "est_1",
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

  p_bar <- plot_tablebar(
    dat = dat_bar,
    bar_label = NULL,
    bar_sig = "sig_point_no_comp",
    bar_header = "a barplot",
    bar_fill = "grouping_var",
    bar_pattern_fill = "grouping_var",
    columns_headers = list("est_1", "est_2"),
    columns_table = list("est_no_comp", "est_comp_whole"),
    columns_table_sig_bold = list(NULL, "sig_point_no_comp"),
    columns_table_sig_high = list("sig_point_no_comp", "sig_point_comp_whole"),
    column_bar = "est_no_comp",
    plot_settings = plotsettings_tablebarplot(
      background_stripes_colour = c(rep(c("white", "lightgrey"), 8), "darkgrey"),
      bar_fill_colour = c("lightblue"),
      bar_pattern_fill_colour = c("yellow"),
      bar_pattern_type = c("TRUE" = "stripe", "FALSE" = "none"),
      bar_sig_type = "pattern",
      columns_width = c(0.05, 0.05)
    )
  )

  })
