# # #
# # # # Umsetzung der neuen Plots -----------------------------------------------
# # # # df_min <- data.frame(land = factor(c("Berlin", "Bremen", "Hessen")),
# # # #                      min_nicht = c(19, 14, 15),
# # # #                                  reg_erreicht = c(60, 50, 45),
# # # #                                  sig_reg_erreicht = c("over", "same", "under")
# # # #                                  )
# # #
# # min_stand <- readxl::read_xlsx("Q:/BT2022/BT/60_Bericht/_Probegrafiken/2023-01-26 Vorlagen Balken und Linien/BT2021_Abb3.9.xlsx", sheet = "Daten BT21")
# #
# # #
# # # data_plot_new <- prep_plot(min_stand, competence = "lesen", parameter = "1")
# # #
# # #
# # # data_bar <- data_plot_new[["plot_bar"]]
# # # data_bar_l <- data_bar[data_bar$depVar == "minVerfehlt" & data_bar$year == "2021", ]
# # # data_bar_r <- data_bar[data_bar$depVar == "regErreicht" & data_bar$year == "2021", ]
# # #
# # #
# # # ## How to deal with the sig erreicht? Muss ja ein beidseitiger test sein, da muuss die Signifikanz anders berechnet werden
# # # ## Neue spalte dafür, wenn beidseitig getestet wird mit drei "below", "equal", "above"
# # #
# # # data_bar_l$est_no_comp <- data_bar_l$est_no_comp *100
# # # data_bar_l$tempvar <- "Title"
# # #
# # #
# # # plot_l <- plot_bar(data_bar_l,
# # #          x_value = "est_no_comp",
# # #          y_value = "state_var",
# # #          bar_label = "est_no_comp",
# # #          bar_sig = "sig_point_comp_whole",
# # #          bar_fill = NULL,
# # #          bar_pattern_fill = "grouping_var",
# # #          plot_settings = plotsettings_tablebarplot(
# # #            axis_x_lims = c(0, 35),
# # #            background_stripes_colour = c("lightgrey", "#00000000"),
# # #            bar_fill_colour = grDevices::rgb(49, 133, 156, maxColorValue = 255),
# # #            bar_sig_type = "frame",
# # #                                               default_list = barplot_MinSta_trend)
# # #          ) +
# # #   ggplot2::facet_grid(. ~ tempvar) +
# # #   ggplot2::theme(strip.background.x = ggplot2::element_rect(fill = "lightblue", colour = "lightblue"))
# # #
# # # data_bar_r$est_no_comp <- data_bar_r$est_no_comp *100
# # # #data_bar_r[nrow(data_bar_r)+1,] <- NA
# # # #data_bar_r[nrow(data_bar_r), "state_var"] <- "Land"
# # # #data_bar_r$state_var <- factor(data_bar_r$state_var, levels = c(unique(data_bar$state_var[data_bar_r$state_var != "Land"]), "Land"))
# # # data_bar_r$tempvar <- "Title"
# # #
# # # plot_r <- plot_bar(data_bar_r,
# # #          x_value = "est_no_comp",
# # #          y_value = "state_var",
# # #          bar_label = "est_no_comp",
# # #          bar_sig = "sig_point_comp_whole",
# # #          bar_fill = NULL,
# # #          bar_pattern_fill = "grouping_var",
# # #          plot_settings = plotsettings_tablebarplot(
# # #            background_stripes_colour = c("lightgrey", "#00000000"),
# # #            bar_fill_colour = grDevices::rgb(75, 172, 198, maxColorValue = 255),
# # #            bar_sig_type = "frame",
# # #            default_list = barplot_MinSta_trend)
# # #            ) +
# # #   ggplot2::facet_grid(. ~ tempvar) +
# # #   ggplot2::theme(strip.background.x = ggplot2::element_rect(fill = "lightblue", colour = "lightblue"))
# # #
# # #  # ggplot2::annotate("tile", x = 35, y = "Land", width = 10, fill = "lightblue") +
# # # #ggplot2::annotate("text", x = 35, y = "Land", label = "Mindeststandard") +
# # # #ggplot2::annotate("segment", x = 35, xend = 45, y = "Land", yend = "Land") +
# # # #ggplot2::annotate("rect", ymin = "Land", ymax = Inf, xmin = -Inf, xmax = Inf, colour = "red")
# # # NULL
# # # #
# # # p_merged <- plot_table_bar(plot_l, plot_r)
# # # #
# # # #
# # # # # # Tabelle -----------------------------------------------------------------
# # #
# # # y_axis_plot <- plot_column(vec = unique(data_bar_l$state_var), plot_settings = plotsettings_tablebarplot(default_list = barplot_MinSta_trend)) +
# # #   theme_table_col()
# # #
# # # patchwork::wrap_plots(y_axis_plot,
# # #                       plot_l,
# # #                       plot_r,
# # #                       widths = c(0.3, 1, 1.5)) &
# # #   ggplot2::theme(
# # #     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
# # #     # As margin is not perfectly eliminated
# # #     axis.ticks.length.y = ggplot2::unit(0, "pt")
# # #   )
# #
# # ## wrap_plot der x-Achse.
# #
# #
# # # Plot 2 -----------------------------------------------------------------
# #
# # Idee um nur das erste Bundesland  zu plotten: Alle außer dem ersten in den Daten durch "" ersetzten
# # Bei zu vielen Gruppenkonstelationen neue Variable bilden.
#
# data_plot_new <- prep_plot(min_stand, competence = "lesen", parameter = "1")
#
# data_plot_new[["plot_bar"]]$x_label <- as.factor(data_plot_new[["plot_bar"]]$year)
# dat_p_2 <- data_plot_new[["plot_bar"]]
#
# dat_p_subset <- subset(dat_p_2, x_label == 2016)
# dat_p_subset$x_label <- "y_axis_grouping"
# dat_p_subset$est_point_no_comp <- dat_p_subset$keyword_no_comp
#
# dat_p <- rbind(dat_p_2, dat_p_subset)
#
# dat_p_subset$x_label <- "land"
#
# land_labels <- dat_p_subset$state_var
# land_labels[duplicated(land_labels)] <-  ""
#
# dat_p_subset$est_point_no_comp <- land_labels
#
# dat_p <- rbind(dat_p, dat_p_subset)
# dat_p$x_label <- factor(dat_p$x_label, levels = c("land", "y_axis_grouping", "2011", "2016", "2021"))
#
#
#
# # Eine Spalte mit Inhalt, eine Spalte y-Achse und eine Spalte x-Achse
# settings_list <- list(col_adjustment = c("left", "left", "center", "center", "center"),
#                       col_nudge_x = c(-0.55, -0.55, 0, 0, 0))
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
#   build_columns_2(data_plot_table = dat_p, x_axis = "x_label", plot_settings = settings_list) +
#   theme_table()
#
#
# ## Alternative
# p1 <- plot_column(vec = dat_p[dat_p$x_label == "2016", "state_var"])
#
#
#
# y_axis_land <- plot_column(vec = dat_p$state_var)
#
#
# ## Take data.frame, use the data to plot y-axis and geom_text, plot every column like it is in the dataframe. Dafür müsste der data.frame ins wide format gebracht werden.
#
# plot_data_frame_table <- function(df, y_axis){
#
#   ## column names are the x-axis. You could also provide an column that is the x-axis already.
#   ## Provide a column that is the y-axis.
#   ## Provide columns that should be plotted.
#
#
# }
#
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
#          bar_sig = "sig_trend_comp", ##Wurde das umgestellt? Oder wird das extra dafür erstellt?
#          bar_fill = "sig_trend_comp",
#          bar_label = NULL,
#          plot_settings = plotsettings_tablebarplot(default_list = barplot_MinSta_trend))
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
# stand_fill <-c(
#   "minVerfehltFALSE" = "white",
#   "minVerfehltTRUE" = "darkblue",
#   "regErreichtFALSE" = "white",
#   "regErreichtTRUE" = "blue",
#   "optErreichtFALSE" = "white",
#   "optErreichtTRUE" = "lightblue"
# )
#
# stand_pattern_fill <-c(
#   "minVerfehlt" = "red",
#   "regErreicht" = "blue",
#   "optErreicht" = "green"
# )
#
# data_p_2$pattern_fill <- paste0(data_p_2$depVar, data_p_2$sig_trend_comp)
#
#
# p2 <- plot_bar(data_p_2,
#                x_value = "est_trend_no_comp",
#                y_value = "keyword_no_comp",
#                bar_fill = "pattern_fill",
#                bar_label = NULL,
#                bar_sig = "sig_trend_comp",
#                bar_pattern_fill = "depVar",
#                plot_settings = plotsettings_tablebarplot(bar_pattern_fill_colour = stand_pattern_fill,
#                                                     bar_fill_colour = stand_fill,
#                                                     default_list = barplot_MinSta_trend)) #+
#   # ggstats::geom_stripped_rows(
#   #   odd = "darkgrey",
#   #   even = "#00000000")
#
# ## Evtl. mein geom_annotate für Hintergrund bei allen drei Reihen
#
#
# save_patch <- patchwork::wrap_plots(p1, p2) &
#   ggplot2::theme(
#     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
#     # As margin is not perfectly eliminated
#     axis.ticks.length.y = ggplot2::unit(0, "pt"),
#     plot.title = element_text()
#   )
#
# # ## Stripes:
# # # ggplot2::ggplot(data_p_2, ggplot2::aes(x = est_trend_no_comp,
# # #                      y = keyword_no_comp)) +
# # #   ggplot2::geom_rect(ggplot2::aes(ymin = keyword_no_comp,
# # #                 ymax = dplyr::lead(keyword_no_comp),
# # #                 xmin = -0.5,
# # #                 xmax = Inf,
# # #                 fill = state_var))
# # #
# # #
# # # ggplot2::ggplot(
# # #   data = data_p_2,
# # #   mapping = ggplot2::aes(
# # #     x = est_trend_no_comp,
# # #     y = keyword_no_comp,
# # #     fill = depVar,
# # #     # TODO: maybe find another interface for this argument?
# # #     # TODO: linetype and pattern aes only works when specified here
# # #     #   - when specified on geom, the first bar colors in the test plot
# # #     #     for frame is exchanged!
# # #     pattern = sig_trend_no_comp
# # #   )) +
# # #   geom_rect(
# # #     ggplot2::aes(
# # #       ymin = keyword_no_comp,
# # #       ymax = dplyr::lead(keyword_no_comp),
# # #       xmin = -100,
# # #       xmax = Inf,
# # #       fill = state_var),
# # #     inherit.aes = FALSE
# # #   ) +
# # #
# # #   #   ggforestplot::geom_stripes( data = data_p_2,
# # #   #                                ggplot2::aes(y = keyword_no_comp, group = state_var, colour = state_var),
# # #   #                               size = 6,
# # #   #                                inherit.aes = FALSE) +
# # #   # ggstats::geom_stripped_rows(
# # #   #   data = data_p_2,
# # #   #   ggplot2::aes(y = keyword_no_comp, group = state_var, colour = state_var),
# # #   #   inherit.aes = FALSE) +
# # #   # NULL
# # #
# # # ggplot2::geom_vline(
# # #   xintercept = scale_breaks,
# # #   linetype = "dashed", colour = "darkgrey"
# # # ) +
# # #   ggplot2::geom_vline(
# # #     xintercept = 0,
# # #     colour = "darkgrey"
# # #   ) +
# # #   ggplot2::scale_x_continuous(breaks = scale_breaks) +
# # #   #ggplot2::scale_fill_manual(values = stand_fill) +
# # #   ## This chunk only works together with the ggpattern::scale-specifications.
# # #   ggpattern::geom_col_pattern(
# # #     mapping = ggplot2::aes(pattern_fill = depVar,
# # #                            linetype = NULL),
# # #     position = ggplot2::position_dodge(width = 0.8),
# # #     color = "black",
# # #     linewidth = 0.6,
# # #     pattern_colour = "white",
# # #     pattern_angle = -45,
# # #     pattern_density = 0.4, # Streifenbreite
# # #     pattern_spacing = 0.01, # Abstand
# # #     pattern_key_scale_factor = 0.6,
# # #     width = 0.4
# # #   ) +
# # #   ggpattern::scale_pattern_manual(values = sig_pattern) +
# # #   ggpattern::scale_pattern_fill_manual(values = stand_pattern_fill) +
# # #   theme_table_bar() +
# # #   NULL
# # #
# # #
# # #
# # # patchwork::wrap_plots(p1, p2) &
# # #   ggplot2::theme(
# # #     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
# # #     # As margin is not perfectly eliminated
# # #     axis.ticks.length.y = ggplot2::unit(0, "pt")
# # #   )
# #
