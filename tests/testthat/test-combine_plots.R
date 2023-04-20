
test_that("example mindeststandard short version", {


dat_bar <- prep_plot(min_stand,
                     competence = "lesen",
                     parameter = "1"
)[["plot_lines"]]


dat_bar$est_point_end <- dat_bar$est_point_end * 100
dat_bar <- subset(dat_bar, year_end == 2021)
dat_bar <- subset(dat_bar, year_start == 2016)

dat_bar$sig_point_end[1:10] <- "FALSE"
dat_bar$sig_minstand <- ifelse(dat_bar$sig_point_end == "TRUE" & dat_bar$est_trend_comp < 0,
                               "below",
                               ifelse(dat_bar$sig_point_end == "TRUE" & dat_bar$est_trend_comp > 0,
                                      "above",
                                      "no_sig")
)

# Plot 1 ------------------------------------------------------------------
dat_bar_1 <- subset(dat_bar, depVar == "minVerfehlt")

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
    axis_x_lims = c(0, 39),
    bar_fill_colour = grDevices::rgb(49, 133, 156, maxColorValue = 255),
    columns_alignment = 0.5,
    columns_width = 0.7,
    headers_alignment = 0,
    default_list = barplot_MinSta
  )
)


# Plot 2 ------------------------------------------------------------------

  dat_bar_2 <- subset(dat_bar, depVar == "regErreicht")

  p_bar_2 <- plot_tablebar(
    dat = dat_bar_2,
    bar_label = "est_point_end",
    bar_label_sig = "sig_point_end",
    bar_sig = "sig_minstand",
    bar_header = "Regelstandard erreicht oder übertroffen (MSA)",
    bar_est = "est_point_end",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(0, 75),
      bar_fill_colour = grDevices::rgb(75, 172, 198, maxColorValue = 255),
      default_list = barplot_MinSta
    )
  )



# Plot 3 ------------------------------------------------------------------
  dat_bar_3 <- subset(dat_bar, depVar == "optErreicht")

  p_bar_3 <- plot_tablebar(
    dat = dat_bar_3,
    bar_label = "est_point_end",
    bar_label_sig = "sig_point_end",
    bar_sig = "sig_minstand",
    bar_header = "Optimalstandard \n erreicht (MSA)",
    bar_est = "est_point_end",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(0, 25),
      default_list = barplot_MinSta
    )
  )

minsta_plot <- combine_plots(list(p_bar_1, p_bar_2, p_bar_3))

# combine plots -----------------------------------------------------------
vdiffr::expect_doppelganger("Mindeststandards", minsta_plot)


#save_plot(minsta_plot, filename = "../Kap3_2022_MSA.pdf", height = 226.2 / 3)


})






test_that("Example barplot long format is plotted correctly", {
  dat_bar <- prep_plot(min_stand,
                       competence = "lesen",
                       parameter = "1"
  )[["plot_tablebar"]]


  dat_bar$est_point_start <- dat_bar$est_point_start * 100
  dat_bar$est_point_end <- dat_bar$est_point_end * 100
  dat_bar$est_trend_no_comp <- dat_bar$est_trend_no_comp * 100
  dat_bar$sig_point_start[1:10] <- "FALSE"
  dat_bar$y_axis_new <- paste0(dat_bar$state_var, dat_bar$depVar)
  dat_bar$se_trend_no_comp <- dat_bar$se_trend_no_comp * 100
  dat_bar$se_trend_no_comp <- construct_label(dat_bar, label_se = "se_trend_no_comp")


# Plot 1 ------------------------------------------------------------------

  dat_bar_1 <- dat_bar[which(dat_bar$year_start == 2011 & dat_bar$year_end == 2016), ]

  p_bar_1 <- plot_tablebar(
    dat = dat_bar_1,
    bar_label = NULL,
    bar_sig = "sig_trend_no_comp",
    bar_header = " ", # Zu column headers dazu
    bar_fill = "depVar",
    columns_headers = list("Land", " ", "%", "%", "%", "*(SE)*"),
    column_spanners = list("**2011**" = 3,
                           "**2016**" = 4,
                           "**Differenz 2016 - 2011**" = c(5,7)
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


# Plot 2 ------------------------------------------------------------------
  dat_bar_2 <- dat_bar[which(dat_bar$year_start == 2016 & dat_bar$year_end == 2021), ]

  p_bar_2 <- plot_tablebar(
    dat = dat_bar_2,
    bar_label = NULL,
    bar_sig = "sig_trend_no_comp",
    bar_header = " ", # Zu column headers dazu
    bar_fill = "depVar",
    columns_headers = list("%", "%", "%", "*(SE)*"),
    column_spanners = list("**2016**" = 1,
                           "**2021**" = 2,
                           "**Differenz 2021 - 2016**" = c(3,5)
    ),
    columns_table = list("est_point_start",
                         "est_point_end",
                         "est_trend_no_comp",
                         "se_trend_no_comp"),
    columns_table_sig_bold = list(
                                  NULL,
                                  NULL,
                                  "sig_trend_no_comp",
                                  NULL),
    columns_table_sig_high = list(
                                  NULL,
                                  NULL,
                                  "sig_trend_comp",
                                  NULL),
    bar_est = "est_trend_no_comp",
    y_axis = "y_axis_new",
    plot_settings = plotsettings_tablebarplot(default_list = barplot_MinSta_trend)
  )


  c_plot <- combine_plots(list(p_bar_1, p_bar_2))

  vdiffr::expect_doppelganger("MinStand_trend", c_plot)

  save_plot(c_plot, filename = "../Kap3_2022_MSA_trend.pdf")

})

