test_that("example mindeststandard short version", {
  dat_bar <- prep_plot(min_stand,
    competence = "lesen",
    parameter = "1"
  )[["plot_tablebar"]]


  dat_bar <- construct_percent(dat_bar, columns = colnames(dat_bar)[grep("est", colnames(dat_bar))])
  for (i in c("2011", "2016", "2021")) {
    dat_bar <- construct_directional_sig(dat_bar, est_column = paste0("est_noTrend_Comp_crossDiff_wholeGroup_", i), sig_column = paste0("sig_noTrend_Comp_crossDiff_wholeGroup_", i))
  }

  # Plot 1 ------------------------------------------------------------------
  dat_bar_1 <- subset(dat_bar, depVar == "minVerfehlt")

  p_bar_1 <- plot_tablebar(
    dat = dat_bar_1,
    bar_label = "est_noTrend_noComp_2021_percent",
    bar_label_sig = "sig_noTrend_noComp_2021",
    bar_sig = "sig_noTrend_Comp_crossDiff_wholeGroup_2021_directional_sig",
    headers = list("Land", "Mindeststandard nicht erreicht (MSA)"),
    columns_table = list("state_var"),
    bar_est = "est_noTrend_noComp_2021_percent",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(0, 39),
      bar_fill_colour = grDevices::rgb(49, 133, 156, maxColorValue = 255),
      columns_alignment = 0,
      columns_width = c(0.3, 0.7),
      headers_alignment = c(0, 0.5),
      default_list = barplot_plot_frame
    )
  )


  # Plot 2 ------------------------------------------------------------------

  dat_bar_2 <- subset(dat_bar, depVar == "regErreicht")

  p_bar_2 <- plot_tablebar(
    dat = dat_bar_2,
    bar_label = "est_noTrend_noComp_2021_percent",
    bar_label_sig = "sig_noTrend_noComp_2021",
    bar_sig = "sig_noTrend_Comp_crossDiff_wholeGroup_2021_directional_sig",
    bar_est = "est_noTrend_noComp_2021_percent",
    headers = list("Regelstandard erreicht oder übertroffen (MSA)"),
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      columns_alignment = 0,
      headers_alignment = 0,
      columns_width = NULL,
      axis_x_lims = c(0, 75),
      bar_fill_colour = grDevices::rgb(75, 172, 198, maxColorValue = 255),
      default_list = barplot_plot_frame
    )
  )



  # Plot 3 ------------------------------------------------------------------
  dat_bar_3 <- subset(dat_bar, depVar == "optErreicht")

  p_bar_3 <- plot_tablebar(
    dat = dat_bar_3,
    bar_label = "est_noTrend_noComp_2021_percent",
    bar_label_sig = "sig_noTrend_noComp_2021",
    bar_sig = "sig_noTrend_Comp_crossDiff_wholeGroup_2021_directional_sig",
    headers = "Optimalstandard erreicht (MSA)",
    bar_est = "est_noTrend_noComp_2021_percent",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(0, 25),
      default_list = barplot_plot_frame
    )
  )

  minsta_plot <- combine_plots(list(p_bar_1, p_bar_2, p_bar_3))

  # combine plots -----------------------------------------------------------
  vdiffr::expect_doppelganger("Mindeststandards", minsta_plot)


  # save_plot(minsta_plot, filename = "../Kap3_2022_MSA_v02.pdf", height = 226.2 / 3)
})

test_that("Example barplot long format is plotted correctly", {
  dat_bar <- prep_plot(min_stand,
    competence = "lesen",
    parameter = "1"
  )[["plot_tablebar"]]


  dat_bar <- construct_percent(dat_bar, columns = colnames(dat_bar)[grep("est_|se_", colnames(dat_bar))])
  dat_bar <- construct_label(dat_bar,
    new_name = "se_Trend_noComp_20112016_percent_label",
    label_se = "se_Trend_noComp_20112016_percent",
    round_se = 1
  )

  dat_bar <- construct_label(dat_bar,
    label_se = "se_Trend_noComp_20162021_percent",
    new_name = "se_Trend_noComp_20162021_percent",
    round_se = 1
  )


  dat_bar$depVar <- gsub("minVerfehlt", "Mindeststandard nicht erreicht", dat_bar$depVar)
  dat_bar$depVar <- gsub("regErreicht", "Regelstandard erreicht", dat_bar$depVar)
  dat_bar$depVar <- gsub("optErreicht", "Optimalstandard erreicht", dat_bar$depVar)

  dat_bar$y_axis_new <- paste0(dat_bar$state_var, dat_bar$depVar)
  dat_bar$y_axis_new <- as.factor(dat_bar$y_axis_new)

  dat_bar <- dat_bar[order(dat_bar$y_axis_new), ]
  dat_bar$state_var <- gsub("-", "-<br>", dat_bar$state_var)

  # Plot 1 ------------------------------------------------------------------
  dat_bar$state_var[duplicated(dat_bar$state_var)] <- " "


  p_bar_1 <- plot_tablebar(
    dat = dat_bar,
    bar_est = "est_Trend_noComp_20112016_percent",
    bar_label = NULL,
    bar_sig = "sig_Trend_noComp_20112016",
    bar_fill = "depVar",
    headers = list("Land", "(MSA)", "%", "%", "%", "*(SE)*", " "),
    column_spanners = list(
      "**2011**" = 3,
      "**2016**" = 4,
      "**Differenz 2016 - 2011**" = c(5, 7)
    ),
    columns_table = list(
      "state_var",
      "depVar",
      "est_noTrend_noComp_2011_percent",
      "est_noTrend_noComp_2016_percent",
      "est_Trend_noComp_20112016_percent",
      "se_Trend_noComp_20112016_percent"
    ),
    columns_round = list(NULL, NULL, 1, 1, 1, NULL),
    columns_table_sig_bold = list(
      NULL,
      NULL,
      NULL,
      NULL,
      "sig_Trend_noComp_20112016",
      NULL
    ),
    columns_table_sig_high = list(
      NULL,
      NULL,
      NULL,
      NULL,
      "sig_Trend_Comp_crossDiff_wholeGroup_20112016",
      NULL
    ),
    y_axis = "y_axis_new",
    plot_settings = plotsettings_tablebarplot(
      columns_alignment = c(0, 0, 1, 1, 1, 1),
      columns_width = c(0.175, 0.35, 0.075, 0.075, 0.075, 0.075, 0.175),
      columns_nudge_x = c(0, 0, -2, -2, -2, -1),
      headers_alignment = c(0, 0, 0.5, 0.5, 0.5, 0.5, 0),
      default_list = barplot_table_plot_pattern
    )
  )


  # Plot 2 ------------------------------------------------------------------
  p_bar_2 <- plot_tablebar(
    dat = dat_bar,
    bar_est = "est_Trend_noComp_20162021_percent",
    bar_label = NULL,
    bar_sig = "sig_Trend_noComp_20162021",
    bar_fill = "depVar",
    headers = list("%", "%", "%", "*(SE)*", NULL),
    column_spanners = list(
      "**2016**" = 1,
      "**2021**" = 2,
      "**Differenz 2021 - 2016**" = c(3, 5)
    ),
    columns_round = list(1, 1, 1, NULL),
    columns_table = list(
      "est_noTrend_noComp_2016_percent",
      "est_noTrend_noComp_2021_percent",
      "est_Trend_noComp_20162021_percent",
      "se_Trend_noComp_20162021_percent"
    ),
    columns_table_sig_bold = list(
      NULL,
      NULL,
      "sig_Trend_noComp_20162021",
      NULL
    ),
    columns_table_sig_high = list(
      NULL,
      NULL,
      "sig_Trend_Comp_crossDiff_wholeGroup_20162021",
      NULL
    ),
    y_axis = "y_axis_new",
    plot_settings = plotsettings_tablebarplot(
      columns_alignment = c(1, 1, 1, 1),
      columns_width = c(0.12, 0.12, 0.12, 0.12, 0.52),
      columns_nudge_x = c(-2, -2, -2, -2),
      headers_alignment = c(0.5, 0.5, 0.5, 0.5, 0),
      bar_pattern_spacing = 0.0125,
      default_list = barplot_table_plot_pattern
    )
  )

  c_plot <- combine_plots(list(p_bar_1, p_bar_2))

  vdiffr::expect_doppelganger("MinStand_trend", c_plot)

  # save_plot(c_plot, filename = "../Kap3_2022_MSA_trend_v02.pdf")
})
