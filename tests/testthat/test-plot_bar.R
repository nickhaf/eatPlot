test_that("Barplot is still the same", {
  df_bar <- data.frame(
    x = 1:4,
    y = rep("a", 4),
    bar_pattern = c(TRUE, TRUE, FALSE, FALSE),
    bar_fill = c("a", "b", "c", "d"),
    bar_pattern_fill = c("a", "a", "b", "b")
  )


  vdiffr::expect_doppelganger(
    "Bar plot",
    plot_bar(df_bar,
      x_value = "x", y_value = "y",
      bar_sig = "bar_pattern",
      bar_fill = "bar_fill",
      bar_pattern_fill = "bar_pattern_fill",
      bar_fill_setting = c(
        "a" = "red",
        "b" = "blue",
        "c" = "green",
        "d" = "grey"
      ),
      bar_pattern_fill_setting = c(
        "a" = "yellow",
        "b" = "orange"
      ),
      plot_settings = plotsettings_barplot(
        background_stripes_colour = c("red", "blue"),
        default_list = barplot_MinSta
      )
    )
  )
})

test_that("Example barplot is still the same", {
  plot_data <- prep_no_trend(
    dat = adjusted_means,
    columns = "adjust",
    grouping_var = "adjust",
    competence = "GL",
    sig_niveau = 0.05
  )

  vdiffr::expect_doppelganger(
    "Bar plot for trend_books",
    plot_bar(
      plot_data[["plot_bar"]],
      plot_settings = plotsettings_barplot(background_stripes_colour = c("grey", "white"))
    )
  )
})

test_that("Example barplot can be plotted with different frames", {
  plot_data <- prep_no_trend(
    dat = adjusted_means,
    columns = "adjust",
    grouping_var = "adjust",
    competence = "GL",
    sig_niveau = 0.05
  )

  vdiffr::expect_doppelganger("Bar plot with frames for trend_books",
    plot_bar(plot_data[["plot_bar"]],
      bar_sig_type = "frame",
    plot_settings = plotsettings_barplot(background_stripes_colour = c("grey", "white"))
    )
  )
})
