df_backgroundlines <- data.frame(
  TR_BUNDESLAND = rep("wholeGroup", 2),
  year_start = c(2011, 2013),
  year_end = c(2013, 2016),
  est_noTrendStart_noComp_wholeGroup = c(10:11),
  est_noTrendEnd_noComp_wholeGroup = c(11:12),
  se_noTrendStart_noComp_wholeGroup = c(0.5, 1.2),
  se_noTrendEnd_noComp_wholeGroup = c(0.1, 0.9),
  years_Trend = c("20112013", "20132016")
)

line_values <- c("est_noTrendStart_noComp_wholeGroup", "est_noTrendEnd_noComp_wholeGroup")
line_se <- c("se_noTrendStart_noComp_wholeGroup", "se_noTrendEnd_noComp_wholeGroup")

test_that("backgroundlines is still the same", {
  vdiffr::expect_doppelganger("Plotting backgroundlines",
                              ggplot2::ggplot() +
                                plot_background_lines(
                                  dat = df_backgroundlines,
                                  line_values = line_values,
                                  line_se = line_se
                                ))
})

test_that("backgroundlines are also plotted if no SE labels are provided, but are in the dataset", {
  vdiffr::expect_doppelganger("Plotting backgroundlines with no provided SE labels",
                              ggplot2::ggplot() +
                                plot_background_lines(
                                  dat = df_backgroundlines,
                                  line_values = line_values,
                                  line_se = NULL
                                ))
})

test_that("backgroundlines are not plotted if no SEs are available in the dataset", {
  df_backgroundlines_noSE <- within(df_backgroundlines, {
    se_noTrendStart_noComp_wholeGroup <- NULL
    se_noTrendEnd_noComp_wholeGroup <- NULL
  })

  vdiffr::expect_doppelganger("Plotting backgroundlines with no available SE labels",
                              ggplot2::ggplot() +
                                plot_background_lines(
                                  dat = df_backgroundlines_noSE,
                                  line_values = line_values,
                                  line_se = line_se
                                ))

  expect_message(ggplot2::ggplot() +
                   plot_background_lines(
                     dat = df_backgroundlines_noSE,
                     line_values = line_values,
                     line_se = line_se
                   ))
})
