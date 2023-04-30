test_that("settings do something", {
  plot_dat_test <- prep_plot(
    dat = trend_books,
    competence = "GL",
    grouping_var = "KBuecher_imp3"
  )

  plot_dat_test <- filter_rows(plot_dat_test, column_name = "state_var", subsetter = "wholeGroup", remove = TRUE)
  ## Testweise einige PUnkte auf n.s. setzen
  plot_dat_test$plot_points <- plot_dat_test$plot_points[!(plot_dat_test$plot_points$years_Trend == "20112016" & plot_dat_test$plot_points$grouping_var == "TRUE"), ]


  p_line <- plot_lineplot(
    plot_dat = plot_dat_test,
    line_sig = "sig_Trend_CompWhole",
    label_est = "est_Trend_noComp",
    label_sig_bold = "est_Trend_CompWhole",
    label_sig_high = "sig_noTrendEnd_noComp",
    point_values = "est_noTrend_noComp",
    years_lines = list(c(2011, 2016), c(2016, 2021)),
    years_braces = list(c(2011, 2016), c(2016, 2021)),
    background_lines = FALSE,
    plot_settings = plotsettings_lineplot(
      axis_x_background_colour = "red",
      axis_x_background_width_y = 0.08,
      axis_x_label_centralize = 0.15,
      axis_x_label_nudge_y = 0.05,
      axis_x_label_size = 2.4,
      brace_label_gap_y = 0.15,
      brace_label_nudge_x = 0.3,
      brace_label_nudge_y = 0.08,
      brace_label_size = 3,
      brace_line_width = 0.8,
      brace_span_y = 0.16,
      grouping_colours = c("2" = "blue", "1" = "green"),
      line_type = c("TRUE" = "dotted", "FALSE" = "F1"),
      line_width = 1,
      margin_bottom = 0.05,
      margin_left = 0.03,
      margin_right = 0.001,
      margin_top = 0.005,
      n_cols = 6,
      point_label_nudge = FALSE,
      point_label_nudge_direction = list("0" = "+", "1" = "-"),
      point_label_nudge_y = 0.1,
      point_label_size = 1,
      point_shapes = c("TRUE" = 2, "FALSE" = 10),
      point_size = 1,
      split_plot = TRUE,
      split_plot_gap_width = 0.01,
      y_axis = FALSE
    )
  )

  vdiffr::expect_doppelganger("lineplot random settings", p_line)
})

test_that("correct states are extracted", {
  test_plot_2 <- list(
    plot_points = data.frame(
      state_var = c("a", "b", "noGroup"),
      grouping_var = factor(c("group1", "group1", "group1", "group2", "group2", "group2"), levels = c("group1", "group2")),
      year = rep(c(1, 2, 3), 2),
      est_point = c(200, 210, 220, 205, 215, 225),
      sig_noTrend_noComp = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
    ),
    plot_lines = data.frame(
      state_var = c("a", "a", "b", "b"),
      grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1, 2, 2),
      year_end = c(2, 2, 3, 3),
      est_noTrendStart_noComp = c(200, 205, 210, 215),
      est_noTrendEnd_noComp = c(210, 215, 220, 225),
      sig_trend = c(TRUE, FALSE, FALSE, TRUE)
    ),
    plot_background_lines = data.frame(
      state_var = rep("wholeGroup", 4),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_noTrendStart_noComp = c(190, 225),
      est_noTrendEnd_noComp = c(225, 230),
      sig_trend = c(TRUE, FALSE)
    ),
    plot_braces = data.frame(
      state_var = c("a", "a", "b", "b"),
      grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1, 2, 2),
      year_end = c(2, 2, 3, 3),
      est_label = c(10, 20, 30, 40),
      se_label = c(1, 2, 3, 4),
      sig_label_1 = c(TRUE, FALSE, FALSE, TRUE),
      sig_label_2 = c(FALSE, TRUE, FALSE, TRUE)
    )
  )

  expect_equal(filter_rows(test_plot_2, column_name = "state_var", subsetter = "a")$plot_points$state_var, c("a", "a"))
  expect_equal(filter_rows(test_plot_2, column_name = "state_var", subsetter = "a")$plot_lines$state_var, c("a", "a"))
})

test_that("lineplot chpt_4 with one group is still the same", {
  trend_books_changed <- trend_books[trend_books$KBuecher_imp3 != "0" | is.na(trend_books$KBuecher_imp3), ]

  plot_dat_test <- prep_plot(
    dat = trend_books_changed,
    competence = "GL",
    grouping_var = "KBuecher_imp3"
  )

  plot_dat_test <- filter_rows(plot_dat_test, column_name = "state_var", subsetter = "wholeGroup", remove = TRUE)
  ## Testweise einige PUnkte auf n.s. setzen
  plot_dat_test$plot_points <- plot_dat_test$plot_points[!(plot_dat_test$plot_points$years_Trend == "20112016" & plot_dat_test$plot_points$grouping_var == "TRUE"), ]


  p_line <- plot_lineplot(
    plot_dat = plot_dat_test,
    point_sig = "sig_noTrend_CompWhole",
    point_values = "est_noTrend_noComp",
    line_values = c("est_noTrendStart_noComp", "est_noTrendEnd_noComp"),
    line_sig = "sig_Trend_noComp",
    label_sig_high = "sig_noTrendEnd_noComp",
    years_lines = list(c(2011, 2016), c(2016, 2021)),
    years_braces = list(c(2011, 2016), c(2016, 2021)),
    plot_settings = plotsettings_lineplot(default_list = lineplot_chpt_4)
  )
  vdiffr::expect_doppelganger("lineplot_chpt_4_1group", p_line)
  # save_plot(p_line, filename = "../split_lineplot_trend_books-v02.pdf")
})

test_that("lineplot chpt_4 with two groups is still the same", {
  plot_dat_test <- prep_plot(
    dat = trend_books,
    competence = "GL",
    grouping_var = "KBuecher_imp3"
  )

  plot_dat_test <- filter_rows(plot_dat_test, column_name = "state_var", subsetter = "wholeGroup", remove = TRUE)
  ## Testweise einige PUnkte auf n.s. setzen
  plot_dat_test$plot_points <- plot_dat_test$plot_points[!(plot_dat_test$plot_points$years_Trend == "20112016" & plot_dat_test$plot_points$grouping_var == "TRUE"), ]


  p_line <- plot_lineplot(
    plot_dat = plot_dat_test,
    line_sig = "sig_Trend_noComp",
    label_sig_high = "sig_noTrendEnd_noComp",
    years_lines = list(c(2011, 2016), c(2016, 2021)),
    years_braces = list(c(2011, 2016), c(2016, 2021)),
    plot_settings = plotsettings_lineplot(default_list = lineplot_chpt_4)
  )

  vdiffr::expect_doppelganger("lineplot_chpt_4_2groups", p_line)
  # save_plot(p_line, filename = "../split_lineplot_2_books.pdf")
})

test_that("lineplot chpt. 4 with 3 groups is still the same", {
  trend_books_2 <- trend_books[trend_books$KBuecher_imp3 == "0" & !is.na(trend_books$KBuecher_imp3) & trend_books$parameter == "mean", ]
  trend_books_2$KBuecher_imp3 <- rep("Drei", nrow(trend_books_2))
  trend_books_2[, 9:ncol(trend_books_2)] <- trend_books_2[, 9:ncol(trend_books_2)] + 15

  books_3 <- rbind(trend_books, trend_books_2)
  books_3$KBuecher_imp3 <- as.factor(books_3$KBuecher_imp3)

  plot_dat_3 <- prep_plot(
    dat = books_3,
    competence = "GL",
    grouping_var = "KBuecher_imp3"
  )

  plot_dat_3 <- filter_rows(plot_dat_3, column_name = "state_var", subsetter = "wholeGroup", remove = TRUE)
  ## Testweise einige PUnkte auf n.s. setzen
  plot_dat_3$plot_points <- plot_dat_3$plot_points[!(plot_dat_3$plot_points$years_Trend == "20112016" & plot_dat_3$plot_points$grouping_var == "TRUE"), ]

  p_line <- plot_lineplot(
    plot_dat = plot_dat_3,
    point_sig = "sig_noTrend_noComp",
    line_sig = "sig_Trend_noComp",
    label_sig_high = "sig_noTrendEnd_noComp",
    years_lines = list(c(2011, 2016), c(2016, 2021)),
    years_braces = list(c(2011, 2016), c(2016, 2021)),
    plot_settings = plotsettings_lineplot(
      default_list = lineplot_chpt_4
    )
  )

  vdiffr::expect_doppelganger("lineplot_chpt_4_3groups", p_line)

  #  save_plot(p_line, filename = "../split_lineplot_3_books.pdf")
})

test_that("competence_vars can be used as tiles", {
  trend_books_2 <- trend_books[trend_books$kb %in% c("DHW", "GL", "GM", "hoeren", "lesen"), ]
  trend_books_2$KBuecher_imp3 <- factor(trend_books_2$KBuecher_imp3, levels = c("1", "0", "0.vs.1"))
  trend_books_2$kb <- gsub("DHW", "Deutsch Lesen", trend_books_2$kb)
  trend_books_2$kb <- gsub("GL", "Deutsch Zuhören", trend_books_2$kb)
  trend_books_2$kb <- gsub("GM", "Deutsch Orthographie", trend_books_2$kb)
  trend_books_2$kb <- gsub("hoeren", "Englisch Leseverstehen", trend_books_2$kb)
  trend_books_2$kb <- gsub("lesen", "Englisch Hörverstehen", trend_books_2$kb)

  plot_dat_test <- prep_plot(
    dat = trend_books_2,
    grouping_var = "KBuecher_imp3",
    states = "wholeGroup"
  )


  p_line <- plot_lineplot(
    plot_dat = plot_dat_test,
    seperate_plot_var = "competence_var",
    line_sig = "sig_Trend_noComp",
    label_sig_high = NULL,
    years_lines = list(c(2011, 2016), c(2016, 2021)),
    years_braces = list(c(2011, 2016), c(2016, 2021)),
    plot_settings = plotsettings_lineplot(
      default_list = lineplot_chpt_4_germany
    )
  )


  vdiffr::expect_doppelganger("lineplot_chpt_4_kb_tiles", p_line)

  # save_plot(p_line, filename = "../split_lineplot_kb_books.pdf", height = 226.2 / 2)
})

test_that("competence_vars with 3 groups", {
  trend_books_2 <- trend_books[trend_books$KBuecher_imp3 == "0" & !is.na(trend_books$KBuecher_imp3) & trend_books$parameter == "mean", ]
  trend_books_2$KBuecher_imp3 <- rep("Drei", nrow(trend_books_2))
  trend_books_2[, c("est_2011", "est_2016", "est_2021")] <- trend_books_2[, c("est_2011", "est_2016", "est_2021")] + 15

  books_3 <- rbind(trend_books, trend_books_2)
  books_3 <- books_3[books_3$kb %in% c("DHW", "GL", "GM", "hoeren", "lesen"), ]
  books_3$KBuecher_imp3 <- factor(books_3$KBuecher_imp3, levels = c("1", "Drei", "0", "0.vs.1"))

  books_3$kb <- gsub("DHW", "Deutsch Lesen", books_3$kb)
  books_3$kb <- gsub("GL", "Deutsch Zuhören", books_3$kb)
  books_3$kb <- gsub("GM", "Deutsch Orthographie", books_3$kb)
  books_3$kb <- gsub("hoeren", "Englisch Leseverstehen", books_3$kb)
  books_3$kb <- gsub("lesen", "Englisch Hörverstehen", books_3$kb)



  plot_dat_test <- prep_plot(
    dat = books_3,
    grouping_var = "KBuecher_imp3",
    states = "wholeGroup")

  p_line <- plot_lineplot(
    plot_dat = plot_dat_test,
    point_sig = "sig_noTrend_noComp",
    seperate_plot_var = "competence_var",
    line_sig = "sig_Trend_noComp",
    label_sig_high = NULL,
    years_lines = list(c(2011, 2016), c(2016, 2021)),
    years_braces = list(c(2011, 2016), c(2016, 2021)),
    plot_settings = plotsettings_lineplot(
      point_label_nudge_direction = list("1" = "+", "Drei" = "+", "0" = "-"),
      default_list = lineplot_chpt_4_germany
    )
  )

  vdiffr::expect_doppelganger("lineplot_chpt_4_kb_tiles_3groups", p_line)

  #save_plot(p_line, filename = "../split_lineplot_kb_long.pdf", height = 226.2 / 1.5) #2 + 10)
})

test_that("adjusted means states", {
  plot_dat_test <- prep_plot(
    dat = trend_books,
    competence = "GL",
    grouping_var = "KBuecher_imp3"
  )

  plot_dat_test <- filter_rows(plot_dat_test, column_name = "state_var", subsetter = "wholeGroup", remove = TRUE)
  ## Testweise einige PUnkte auf n.s. setzen
  plot_dat_test$plot_points <- plot_dat_test$plot_points[!(plot_dat_test$plot_points$years_Trend == "20112016" & plot_dat_test$plot_points$grouping_var == "TRUE"), ]


  p_line <- plot_lineplot(
    plot_dat = plot_dat_test,
    point_sig = NULL,
    line_sig = "sig_Trend_noComp",
    label_sig_high = "sig_noTrendEnd_noComp",
    years_lines = list(c(2016, 2021)),
    years_braces = list(c(2016, 2021)),
    background_lines = FALSE,
    plot_settings = plotsettings_lineplot(margin_bottom = 0.03, default_list = lineplot_chpt_4)
  )

  vdiffr::expect_doppelganger("lineplot_chpt_4_adj_means", p_line)


  # save_plot(p_line, filename = "../adjusted_means_states.pdf")
})

test_that("adjusted means for whole group", {
  trend_books_2 <- trend_books[trend_books$kb %in% c("DHW", "GL", "GM", "hoeren", "lesen"), ]
  trend_books_2$KBuecher_imp3 <- factor(trend_books_2$KBuecher_imp3, levels = c("1", "0", "0.vs.1"))
  trend_books_2$kb <- gsub("DHW", "Deutsch Lesen", trend_books_2$kb)
  trend_books_2$kb <- gsub("GL", "Deutsch Zuhören", trend_books_2$kb)
  trend_books_2$kb <- gsub("GM", "Deutsch Orthographie", trend_books_2$kb)
  trend_books_2$kb <- gsub("hoeren", "Englisch Leseverstehen", trend_books_2$kb)
  trend_books_2$kb <- gsub("lesen", "Englisch Hörverstehen", trend_books_2$kb)

  plot_dat_test_kb <- prep_plot(
    dat = trend_books_2,
    states = "wholeGroup",
    grouping_var = "KBuecher_imp3"
  )

  p_line_deutschland <- plot_lineplot(
    plot_dat = plot_dat_test_kb,
    seperate_plot_var = "competence_var",
    point_sig = NULL,
    line_sig = "sig_Trend_noComp",
    label_sig_high = "sig_noTrendEnd_noComp",
    years_lines = list(c(2016, 2021)),
    years_braces = list(c(2016, 2021)),
    background_lines = FALSE,
    plot_settings = plotsettings_lineplot(default_list = lineplot_chpt_4_germany)
  )

  vdiffr::expect_doppelganger("lineplot_chpt_4_adjusted_ger", p_line_deutschland)
  # save_plot(p_line_deutschland, filename = "../adjusted_means_ger.pdf", height = 226.2 / 2 + 10)
})
