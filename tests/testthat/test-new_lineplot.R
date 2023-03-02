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
#
# plot_lineplot(plot_data = plot_dat,
#               left_plot_data = plot_dat_2016,
#               right_plot_data = plot_dat_2021,
#               line_sig = "sig_trend_no_comp")

### Problem: Die patchwork-plots werden alle als einzelne Plots behandelt.Kann man die z.B. erst mit cowplot zusammenfügen?
### Facetting schwierig wegen der braces.


coords <- calc_coords(y_range)
brace_coords <- calc_brace_coords(data_plot_braces, coords)
plot_data <- test_plot_r

ggplot2::ggplot() +
  plot_points(plot_data[["plot_points"]],
              point_values = "est_point",
              point_sig = "sig_point"
  ) +
  plot_lines(plot_data[["plot_lines"]],
             line_values = c("est_point_start", "est_point_end"),
             line_sig = "sig_trend"
  ) +
  # ggbrace::geom_brace(data = brace_coords, ggplot2::aes(x = year, y = brace_y)) +
  plot_braces(plot_data[["plot_braces"]],
              y_range = y_range,
              BL = unique(plot_data[["plot_braces"]]$state_var),
              label_est = "est_trend_no_comp",
              label_se = "se_trend_no_comp",
              label_sig_high = "sig_trend_comp_whole",
              label_sig_bold = "sig_trend_no_comp"
  ) +
  ggplot2::facet_wrap(~grouping_var)



