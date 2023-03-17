# uneq_groups <- read.csv2("p:/Methoden/99_Arbeitsordner/eatRep_problem/eatRep_output_3mzp_unequalGroups.csv", sep = ";")
# uneq_groups$mh <- as.factor(uneq_groups$mh)
#
# plot_dat <- prep_trend(dat = uneq_groups,
#                        competence = "GL",
#                        grouping_var = "mh")
# range_est <- range(plot_dat[["plot_points"]][, "est_point"], na.rm = TRUE)
#
#
# ## Build two seperate data sets, so each side of the plot is seen as one.
# plot_dat_test <- prep_trend(dat = uneq_groups,
#                        competence = "GL",
#                        grouping_var = "mh",
#                        x_years = list(c(2011, 2016), c(2016, 2021)),
#                        x_braces = list(c(2011, 2016), c(2016, 2021))
#                        )
#
#
# p_line <- plot_lineplot(plot_data = plot_dat_test,
#               split_plot = TRUE,
#               line_sig = "sig_trend_no_comp",
#               label_sig_high = "sig_trend_no_comp",
#               y_axis = TRUE
#               )

# save_plot(p_line, filename = "../split_lineplot.pdf", height = 20)
