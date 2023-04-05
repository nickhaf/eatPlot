## code to prepare `lineplot_chpt_4` dataset goes here

lineplot_chpt_4 <- plotsettings(
  margin_bottom = 0.2,
  margin_top = 0.2,
  n_cols = 4,
  nudge_brace_labels_x = 0.155,
  nudge_x_axis = 0.05,
  split_plot = TRUE,
  y_axis = FALSE
)

usethis::use_data(lineplot_chpt_4, overwrite = TRUE)
