# uneq_groups <- read.csv2("p:/Methoden/99_Arbeitsordner/eatRep_problem/eatRep_output_3mzp_unequalGroups.csv", sep = ";")
# uneq_groups$mh <- as.factor(uneq_groups$mh)
# uneq_groups <- uneq_groups[uneq_groups$mh != "TRUE" | is.na(uneq_groups$mh), ]
#
# plot_dat_test <- prep_trend(dat = uneq_groups,
#                        competence = "GL",
#                        grouping_var = "mh",
#                        x_years = list(c(2011, 2016), c(2016, 2021)),
#                        x_braces = list(c(2011, 2016), c(2016, 2021))
#                        )
#
# plot_dat_test <- get_state(plot_dat_test, state = "wholeGroup", remove = TRUE)
# ## Testweise einige PUnkte auf n.s. setzen
# plot_dat_test$plot_points$sig_point[1:10] <- FALSE
# plot_dat_test$plot_points <- plot_dat_test$plot_points[!(plot_dat_test$plot_points$trend == "20112016" & plot_dat_test$plot_points$grouping_var == "TRUE"), ]
#
#
# p_line <- plot_lineplot(plot_data = plot_dat_test,
#               split_plot = TRUE,
#               line_sig = "sig_trend_no_comp",
#               label_sig_high = "sig_trend_no_comp"
#               )
#
# save_plot(p_line, filename = "../split_lineplot.pdf")
#
#
#
#
#
# # two groups --------------------------------------------------------------
#
# uneq_groups <- read.csv2("p:/Methoden/99_Arbeitsordner/eatRep_problem/eatRep_output_3mzp_unequalGroups.csv", sep = ";")
# uneq_groups$mh <- as.factor(uneq_groups$mh)
#
# plot_dat_test <- prep_trend(dat = uneq_groups,
#                        competence = "GL",
#                        grouping_var = "mh",
#                        x_years = list(c(2011, 2016), c(2016, 2021)),
#                        x_braces = list(c(2011, 2016), c(2016, 2021))
#                        )
#
# plot_dat_test <- get_state(plot_dat_test, state = "wholeGroup", remove = TRUE)
# ## Testweise einige PUnkte auf n.s. setzen
# plot_dat_test$plot_points$sig_point[1:10] <- FALSE
# plot_dat_test$plot_points <- plot_dat_test$plot_points[!(plot_dat_test$plot_points$trend == "20112016" & plot_dat_test$plot_points$grouping_var == "TRUE"), ]
#
#
# p_line <- plot_lineplot(plot_data = plot_dat_test,
#               split_plot = TRUE,
#               line_sig = "sig_trend_no_comp",
#               label_sig_high = "sig_trend_no_comp"
#               )
#
# save_plot(p_line, filename = "../split_lineplot_2.pdf")
#
#
#
#
#
# # Three groups ------------------------------------------------------------
#
# uneq_groups <- read.csv2("p:/Methoden/99_Arbeitsordner/eatRep_problem/eatRep_output_3mzp_unequalGroups.csv", sep = ";")
# uneq_groups_2 <- uneq_groups[uneq_groups$mh == FALSE & !is.na(uneq_groups$mh) & uneq_groups$parameter == "mean" , ]
# uneq_groups_2$mh <- rep("Drei", nrow(uneq_groups_2))
# uneq_groups_2[, 9:ncol(uneq_groups_2)] <- uneq_groups_2[, 9:ncol(uneq_groups_2)] + 15
#
# uneq_3 <- rbind(uneq_groups, uneq_groups_2)
# uneq_3$mh <- as.factor(uneq_3$mh)
#
# plot_dat_3 <- prep_trend(dat = uneq_3,
#                        competence = "GL",
#                        grouping_var = "mh",
#                        x_years = list(c(2011, 2016), c(2016, 2021)),
#                        x_braces = list(c(2011, 2021), c(2016, 2021))
#                        )
#
# plot_dat_3 <- get_state(plot_dat_3, state = "wholeGroup", remove = TRUE)
# ## Testweise einige PUnkte auf n.s. setzen
# plot_dat_3$plot_points$sig_point[1:10] <- FALSE
# plot_dat_3$plot_points <- plot_dat_3$plot_points[!(plot_dat_3$plot_points$trend == "20112016" & plot_dat_3$plot_points$grouping_var == "TRUE"), ]
#
# p_line <- plot_lineplot(plot_data = plot_dat_3,
#               split_plot = FALSE,
#               line_sig = "sig_trend_no_comp",
#               label_sig_high = "sig_trend_no_comp"
#               )
#
# save_plot(p_line, filename = "../split_lineplot_3.pdf")


# # whole Group --------------------------------------------------------------
#
# uneq_groups <- read.csv2("p:/Methoden/99_Arbeitsordner/eatRep_problem/eatRep_output_3mzp_unequalGroups.csv", sep = ";")
# uneq_groups$mh <- as.factor(uneq_groups$mh)
#
#
# ## Geht nicht, weil irgendwo nach wholeGroup gefiltert wird: Weniger spezifisch machen, eher "tiles" oder so benenenen statt states:
# plot_dat_test <- prep_trend(dat = uneq_groups,
#                        competence = "Berlin",
#                        competence_var = "TR_BUNDESLAND",
#                        state_var = "kb",
#                        grouping_var = "mh",
#                        x_years = list(c(2011, 2016), c(2016, 2021)),
#                        x_braces = list(c(2011, 2016), c(2016, 2021))
#                        )
#
#
# plot_dat_test <- get_state(plot_dat_test, state = "wholeGroup", remove = TRUE)
# ## Testweise einige PUnkte auf n.s. setzen
# plot_dat_test$plot_points$sig_point[1:10] <- FALSE
# plot_dat_test$plot_points <- plot_dat_test$plot_points[!(plot_dat_test$plot_points$trend == "20112016" & plot_dat_test$plot_points$grouping_var == "TRUE"), ]
#
#
# p_line <- plot_lineplot(plot_data = plot_dat_test,
#               split_plot = TRUE,
#               line_sig = "sig_trend_no_comp",
#               label_sig_high = "sig_trend_no_comp"
#               )
#
# save_plot(p_line, filename = "../split_lineplot_4.pdf")
#
