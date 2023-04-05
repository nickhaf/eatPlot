## code to prepare `lineplot_chpt_4` dataset goes here

lineplot_chpt_4 <- plotsettings(n_cols = 4,
                                nudge_x_axis = 0.155,
                                split_plot = TRUE,
                                y_axis = FALSE)

usethis::use_data(lineplot_chpt_4, overwrite = TRUE)
