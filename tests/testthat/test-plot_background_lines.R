df_backgroundlines <- data.frame(
  point_est = c(320, 600, 600, 550),
  se_point = c(10, 50, 50, 80),
  year = c(2010, 2011, 2011, 2012),
  trend = c("2010_2011", "2010_2011", "2011_2012", "2011_2012"),
  line_se = c(32, 60, 60, 55)
)

test_that("backgroundlines with SE", {
  vdiffr::expect_doppelganger(
    "Plotting backgroundlines",
    ggplot2::ggplot() +
      plot_background_lines(
        dat = df_backgroundlines
      )
  )
})

# test_that("backgroundlines with deducting SE", {
#   vdiffr::expect_doppelganger(
#     "backgroundlines deductiong SE",
#     ggplot2::ggplot() +
#       plot_background_lines(
#         dat = df_backgroundlines,
#         line_values = line_values,
#         line_se = NULL
#       )
#   )
# })

# test_that("backgroundlines without SEs", {
#   df_backgroundlines_noSE <- within(df_backgroundlines, {
#     se_noTrendStart_noComp_wholeGroup <- NULL
#     se_noTrendEnd_noComp_wholeGroup <- NULL
#   })
#
#   vdiffr::expect_doppelganger(
#     "backgroundlines without SE",
#     ggplot2::ggplot() +
#       plot_background_lines(
#         dat = df_backgroundlines_noSE,
#         line_values = line_values,
#         line_se = line_se
#       )
#   )
#
#   expect_message(ggplot2::ggplot() +
#     plot_background_lines(
#       dat = df_backgroundlines_noSE,
#       line_values = line_values,
#       line_se = line_se
#     ))
# })
