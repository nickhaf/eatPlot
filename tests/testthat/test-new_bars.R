#
# # Umsetzung der neuen Plots -----------------------------------------------
# # df_min <- data.frame(land = factor(c("Berlin", "Bremen", "Hessen")),
# #                      min_nicht = c(19, 14, 15),
# #                                  reg_erreicht = c(60, 50, 45),
# #                                  sig_reg_erreicht = c("over", "same", "under")
# #                                  )
#
# min_stand <- readxl::read_xlsx("Q:/BT2022/BT/60_Bericht/_Probegrafiken/2023-01-26 Vorlagen Balken und Linien/BT2021_Abb3.9.xlsx", sheet = "Daten BT21")
#
#
# data_plot_new <- prep_trend(min_stand, competence = "lesen", parameter = "1")
#
#
# data_bar <- data_plot_new[["plot_bar"]]
# data_bar_l <- data_bar[data_bar$depVar == "minVerfehlt" & data_bar$year == "2021", ]
# data_bar_r <- data_bar[data_bar$depVar == "regErreicht" & data_bar$year == "2021", ]
#
#
# ## How to deal with the sig erreicht? Muss ja ein beidseitiger test sein, da muuss die Signifikanz anders berechnet werden
# ## Neue spalte dafür, wenn beidseitig getestet wird mit drei "below", "equal", "above"
#
# data_bar_l$est_no_comp <- data_bar_l$est_no_comp *100
#
# plot_l <- plot_bar(data_bar_l,
#          x_value = "est_no_comp",
#          y_value = "state_var",
#          bar_label = "est_no_comp",
#          bar_sig = "sig_point_comp_whole",
#          bar_fill = NULL,
#          bar_pattern_fill = "grouping_var",
#          plot_settings = plotsettings_barplot(
#            axis_x_lims = c(0, 35),
#            background_stripes_colour = c("lightgrey", "#00000000"),
#            bar_fill_colour = grDevices::rgb(49, 133, 156, maxColorValue = 255),
#            bar_sig_type = "frame",
#                                               default_list = barplot_MinSta)
#          )
#
# data_bar_r$est_no_comp <- data_bar_r$est_no_comp *100
#
# plot_r <- plot_bar(data_bar_r,
#          x_value = "est_no_comp",
#          y_value = "state_var",
#          bar_label = "est_no_comp",
#          bar_sig = "sig_point_comp_whole",
#          bar_fill = NULL,
#          bar_pattern_fill = "grouping_var",
#          plot_settings = plotsettings_barplot(
#            background_stripes_colour = c("lightgrey", "#00000000"),
#            bar_fill_colour = grDevices::rgb(75, 172, 198, maxColorValue = 255),
#            bar_sig_type = "frame",
#            default_list = barplot_MinSta)
# )
#
#
# p_merged <- plot_table_bar(plot_l, plot_r)
#
#
# # Tabelle -----------------------------------------------------------------
# data_t <- data_bar_l
# data_t$x_label <- rep("Land", nrow(data_t))
# y_value <- "state_var"
#
# y_axis_plot <- ggplot2::ggplot() +
#   plot_column(vec = unique(data_bar_l$state_var)) +
#   theme_y_axis()
#
# patchwork::wrap_plots(y_axis_plot,
#                       plot_l,
#                       plot_r,
#                       widths = c(0.3, 1, 1.5)) &
#   ggplot2::theme(
#     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
#     # As margin is not perfectly eliminated
#     axis.ticks.length.y = ggplot2::unit(0, "pt")
#   )
#



# Plot 2 -----------------------------------------------------------------

# Idee um nur das erste Bundesland  zu plotten: Alle außer dem ersten in den Daten durch "" ersetzten
# Bei zu vielen Gruppenkonstelationen neue Variable bilden.

# data_plot_new <- prep_trend(min_stand, competence = "lesen", parameter = "1")
#
# data_plot_new[["plot_bar"]]$x_label <- as.factor(data_plot_new[["plot_bar"]]$year)
# dat_p <- data_plot_new[["plot_bar"]]
#
#
#
# p1 <- ggplot2::ggplot(dat_p,
#                       ggplot2::aes(x = .data$x_label,
#                                    y = .data$keyword_no_comp, #state_var
#                                    group = .data$depVar,
#                                    label = .data$est_point_no_comp)) +
#   ggstats::geom_stripped_rows(
#     odd = grDevices::rgb(219, 238, 244, maxColorValue = 255),
#     even = "#00000000") +
#   #plot_column(unique(dat_p$keyword_no_comp)) +
#   build_columns(data_plot_table = dat_p, x_axis = "grouping_var") +
#   theme_table()
#
#
# ## barplot
# data_p_2 <- data_plot_new[["plot_lines"]]
# data_p_2$est_trend_no_comp <- data_p_2$est_trend_no_comp * 100
# data_p_2 <- data_p_2[data_p_2$year_start == 2011 & data_p_2$year_end == 2016 ,]
#
# plot_bar(data_p_2,
#          x_value = "est_trend_no_comp",
#          y_value = "depVar",
#          grouping = "sig_trend_comp", ##Wurde das umgestellt? Oder wird das extra dafür erstellt?
#          bar_fill = "sig_trend_comp")
#
# plot_borders <- calc_plot_borders(data_p_2$est_trend_no_comp)
# scale_breaks <- seq(plot_borders[1], plot_borders[2], by = 10)
# bar_fill_setting = c(
#   "minVerfehlt" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
#   "regErreicht" = grDevices::rgb(33, 89, 104, maxColorValue = 255),
#   "optErreicht" = "white"
# )
#
#
#
# # Pattern settings --------------------------------------------------------
# sig_pattern <- c(
#   "TRUE" = "none",
#   "FALSE" = "stripe"
# )
#
# adj_pattern_fill <- c(
#   "minVerfehlt" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
#   "regErreicht" = grDevices::rgb(33, 89, 104, maxColorValue = 255),
#   "optErreicht" = "white"
# )
#
# # Fill settings -----------------------------------------------------------
# ## Muss weiß sein, Streifen raufsollen --> eigene Spalte bilden
# stand_fill <- c(
#   "minVerfehlt" = "white",
#   "regErreicht" = "white",
#   "optErreicht" = "white"
# )
#
# stand_pattern_fill <-c(
#   "minVerfehlt" = "red",
#   "regErreicht" = "blue",
#   "optErreicht" = "green"
# )
#
# p2 <- plot_bar(data_p_2,
#                x_value = "est_trend_no_comp",
#                y_value = "keyword_no_comp",
#                grouping = "sig_trend_comp",
#                bar_fill = "depVar",
#                bar_fill_setting = stand_fill,
#                bar_pattern_setting = sig_pattern,
#                bar_pattern_fill = "depVar",
#                bar_pattern_fill_setting = stand_pattern_fill
# ) +
#   ggstats::geom_stripped_rows(
#     odd = "darkgrey",
#     even = "#00000000")
#
#
#
#
# patchwork::wrap_plots(p1, p2) &
#   ggplot2::theme(
#     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
#     # As margin is not perfectly eliminated
#     axis.ticks.length.y = ggplot2::unit(0, "pt")
#   )
#
#
# ## Stripes:
# ggplot2::ggplot(data_p_2, ggplot2::aes(x = est_trend_no_comp,
#                      y = keyword_no_comp)) +
#   ggplot2::geom_rect(ggplot2::aes(ymin = keyword_no_comp,
#                 ymax = dplyr::lead(keyword_no_comp),
#                 xmin = -0.5,
#                 xmax = Inf,
#                 fill = state_var))
#
#
# ggplot2::ggplot(
#   data = data_p_2,
#   mapping = ggplot2::aes(
#     x = est_trend_no_comp,
#     y = keyword_no_comp,
#     fill = depVar,
#     # TODO: maybe find another interface for this argument?
#     # TODO: linetype and pattern aes only works when specified here
#     #   - when specified on geom, the first bar colors in the test plot
#     #     for frame is exchanged!
#     pattern = sig_trend_no_comp
#   )) +
#   geom_rect(
#     ggplot2::aes(
#       ymin = keyword_no_comp,
#       ymax = dplyr::lead(keyword_no_comp),
#       xmin = -100,
#       xmax = Inf,
#       fill = state_var),
#     inherit.aes = FALSE
#   ) +
#
#   #   ggforestplot::geom_stripes( data = data_p_2,
#   #                                ggplot2::aes(y = keyword_no_comp, group = state_var, colour = state_var),
#   #                               size = 6,
#   #                                inherit.aes = FALSE) +
#   # ggstats::geom_stripped_rows(
#   #   data = data_p_2,
#   #   ggplot2::aes(y = keyword_no_comp, group = state_var, colour = state_var),
#   #   inherit.aes = FALSE) +
#   # NULL
#
# ggplot2::geom_vline(
#   xintercept = scale_breaks,
#   linetype = "dashed", colour = "darkgrey"
# ) +
#   ggplot2::geom_vline(
#     xintercept = 0,
#     colour = "darkgrey"
#   ) +
#   ggplot2::scale_x_continuous(breaks = scale_breaks) +
#   #ggplot2::scale_fill_manual(values = stand_fill) +
#   ## This chunk only works together with the ggpattern::scale-specifications.
#   ggpattern::geom_col_pattern(
#     mapping = ggplot2::aes(pattern_fill = depVar,
#                            linetype = NULL),
#     position = ggplot2::position_dodge(width = 0.8),
#     color = "black",
#     linewidth = 0.6,
#     pattern_colour = "white",
#     pattern_angle = -45,
#     pattern_density = 0.4, # Streifenbreite
#     pattern_spacing = 0.01, # Abstand
#     pattern_key_scale_factor = 0.6,
#     width = 0.4
#   ) +
#   ggpattern::scale_pattern_manual(values = sig_pattern) +
#   ggpattern::scale_pattern_fill_manual(values = stand_pattern_fill) +
#   theme_table_bar() +
#   NULL
#
#
#
# patchwork::wrap_plots(p1, p2) &
#   ggplot2::theme(
#     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
#     # As margin is not perfectly eliminated
#     axis.ticks.length.y = ggplot2::unit(0, "pt")
#   )
#
