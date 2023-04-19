
test_that("example mindeststandard short version", {


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
  bar_sig = "sig_minstand",
  bar_header = "Mindeststandard nicht erreicht (MSA)",
  columns_headers = list("Land"),
  columns_table = list("state_var"),
  bar_est = "est_point_end",
  y_axis = "state_var",
  plot_settings = plotsettings_tablebarplot(
    axis_x_lims = c(0, 35),
    columns_alignment = 0,
    columns_nudge_x = 0,
    columns_width = 0.5,
    headers_alignment = 0,
    headers_nudge_x = 0,
    default_list = barplot_MinSta
  )
)


# Plot 2 ------------------------------------------------------------------

  dat_bar_2 <- subset(dat_bar, depVar == "regErreicht")
  dat_bar_2$sig_point_end[1:10] <- "FALSE"


  p_bar_2 <- plot_tablebar(
    dat = dat_bar_2,
    bar_label = "est_point_end",
    bar_sig = "sig_minstand",
    bar_header = "Regelstandard erreicht oder übertroffen (MSA)",
    bar_est = "est_point_end",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(0, 75),
      default_list = barplot_MinSta
    )
  )



# Plot 3 ------------------------------------------------------------------
  dat_bar_3 <- subset(dat_bar, depVar == "optErreicht")
  dat_bar_3$sig_point_end[1:10] <- "FALSE"


  p_bar_3 <- plot_tablebar(
    dat = dat_bar_3,
    bar_label = "est_point_end",
    bar_sig = "sig_minstand",
    bar_header = "Optimalstandard \n erreicht (MSA)",
    bar_est = "est_point_end",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(0, 30),
      default_list = barplot_MinSta
    )
  )

minsta_plot <- combine_plots(list(p_bar_1, p_bar_2, p_bar_3))

# combine plots -----------------------------------------------------------
vdiffr::expect_doppelganger("Mindeststandards", minsta_plot)


save_plot(minsta_plot, filename = "../minsta_plot.pdf", height = 226.2 / 3)


})
