test_that("Barplot is still the same", {
test_bar <- prep_barplot(adjusted_means, sub_groups = "adjust", sig_niveau = 0.05)
test_bar <- test_bar[test_bar$kb == "GL", ]

vdiffr::expect_doppelganger("Bar plot", plot_bar(test_bar))
})

# plot_data <- prep_no_trend(data = adjusted_means, grouping_var = "adjust", columns = "adjust", competence = "GL", sig_niveau = 0.05)
# plot_bar(plot_data[["plot_bar"]])
