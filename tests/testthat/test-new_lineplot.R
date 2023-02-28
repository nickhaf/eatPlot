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
# plot_dat_2016 <- prep_trend(dat = uneq_groups,
#                        competence = "GL",
#                        grouping_var = "mh",
#                        x_years = list(c(2011, 2016)),
#                        x_braces = list(c(2011, 2016))
#                        )
# plot_dat_2021 <- prep_trend(dat = uneq_groups,
#                             competence = "GL",
#                             grouping_var = "mh",
#                             x_years = list(c(2016, 2021)),
#                             x_braces = list(c(2016, 2021))
# )
#
# plot_dat_state_2016 <- lapply(plot_dat_2016[c("plot_points", "plot_lines", "plot_braces")], function(x){
#   x[x$state_var == "Berlin", ]
# })
# plot_dat_state_2016[["plot_background_lines"]] <- plot_dat_2016[["plot_background_lines"]]
#
# plot_dat_state_2021 <- lapply(plot_dat_2021[c("plot_points", "plot_lines", "plot_braces")], function(x){
#   x[x$state_var == "Berlin", ]
# })
# plot_dat_state_2021[["plot_background_lines"]] <- plot_dat_2021[["plot_background_lines"]]
#
#
# ## Plot with same y axis
# p_2016 <- ggplot2::ggplot() +
#   plot_single_lineplot(plot_dat_state_2016, line_sig = "sig_trend_no_comp") +
#   ggplot2::scale_y_continuous(breaks = seq(from = round(range_est[1]-10, -1), to = round(range_est[2], -1), by = 20)) +
#   ggplot2::labs(title = ggplot2::element_blank())
#
# p_2021 <- ggplot2::ggplot() +
#   plot_single_lineplot(plot_dat_state_2021, line_sig = "sig_trend_no_comp") +
#   ggplot2::scale_y_continuous(breaks = seq(from = round(range_est[1]-10, -1), to = round(range_est[2], -1), by = 20))+
#   ggplot2::labs(title = ggplot2::element_blank())
#
# # align.
# patchwork::wrap_plots(p_2016, p_2021) &
#   patchwork::plot_annotation(title = 'Berlin') &
#   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
#
# ## alignen, dafür wird eine gemeinsame y-Scale benötigt.
