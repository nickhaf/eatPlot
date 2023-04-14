test_that("continous barplot looks the same", {
  test_data <- data.frame(
    y_axis = 1:4,
    x_min = rep(0, 4),
    x_max = c(1, -2, 0.4, 3),
    est_1 = c("12", "12", "15", "23"),
    se_1 = c("12", "10", "8", "4"),
    sig_2 = c(TRUE, FALSE, TRUE, FALSE)
  )

  p_bar <- plot_tablebar(dat = test_data,
                columns_table = list("est_1", "se_1" ),
                columns_table_sig_bold = list(NULL, "sig_2"),
                columns_table_sig_high = list("sig_2", "sig_2"),
                column_bar = "est_1",
                plot_settings = plotsettings_tablebarplot(background_stripes_colour = c("white", "lightgrey", "white", "darkgrey")))

  vdiffr::expect_doppelganger("Minimal tablebar", p_bar)

})

