test_data <- data.frame(
  state_var = 1:4,
  x_min = rep(0, 4),
  x_max = c(10, -20, 40, 30),
  est_1 = c(12, 12, 15, 23),
  se_1 = c(12, 10, 8, 4),
  bar_sig = c("TRUE", "FALSE", "TRUE", "FALSE"),
  bar_fill = c("a", "b", "c", "d")
)

test_that("column x coords are calced correctly", {
  expect_equal(
    calc_column_coords(
      plot_borders = c(-10, 10),
      columns_table = c("col_1", "col_2", "col_3"),
      plot_settings = plotsettings_tablebarplot(columns_width = c(0.3, 0.1, 0.2, 0.4))
    ),
    data.frame(
      column = c("bar", "col_3", "col_2", "col_1"),
      left = c(-10, -20, -25, -40),
      middle = c(0.0, -15.0, -22.5, -32.5),
      right = c(10, -10, -20, -25)
    )
  )


  ## Only Columns
  expect_equal(
    calc_column_coords(
      plot_borders = c(0, 0),
      columns_table = c("col_1", "col_2", "col_3"),
      plot_settings = plotsettings_tablebarplot(columns_width = c(0.3, 0.3, 0.4))
    ),
    data.frame(
      column = c("col_3", "col_2", "col_1"),
      left = c(0.6, 0.3, 0),
      middle = c(0.8, 0.45, 0.15),
      right = c(1.0, 0.6, 0.3)
    )
  )

  ## Only Bar
  expect_equal(
    calc_column_coords(
      plot_borders = c(-10, 10),
      columns_table = NULL,
      plot_settings = plotsettings_tablebarplot(columns_width = c(1))
    ),
    data.frame(
      column = c("bar"),
      left = c(-10),
      middle = c(0),
      right = c(10)
    )
  )
})

test_that("column length is checked correctly", {
  column_set <- list("a", "b")
  expect_equal(check_length(column_set, leng = 2), list("a", "b"))

  expect_error(check_length(column_set, 3))
  expect_equal(
    check_length("a", 3, fill = "a"),
    c("a", "a", "a")
  )
})


test_that("simple tablebarplot can be plotted", {
  example_table <- p_bar <- plot_tablebarplot(
    dat = test_data,
    bar_label = NULL,
    bar_sig = "bar_sig",
    bar_fill = "bar_fill",
    headers = list("est_1", "est_2", "a barplot"),
    column_spanners = list("spanner_2" = c(2, 3), "spanner_1" = 1),
    column_spanners_2 = list("spanner_3" = 3, "spanner_2" = c(1, 2)),
    columns_table = list("est_1", "se_1"),
    columns_table_sig_bold = list(NULL, "bar_sig"),
    columns_table_sig_superscript = list("bar_sig", "bar_sig"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      background_stripes_colour = c("white", "lightgrey"),
      bar_fill_colour = c("red", "blue", "yellow", "green"),
      bar_pattern_fill_colour = "white",
      bar_pattern_type = c("stripe", "none"),
      bar_type = "pattern",
      columns_width = c(0.3, 0.2, 0.5),
      bar_pattern_width = 0.5,
      bar_pattern_spacing = 0.1,
      headers_font_size = 5
    )
  )

  vdiffr::expect_doppelganger("Minimal_tablebarplot", example_table)
  # save_plot(example_table, "/home/nick/Downloads/table.pdf",  height = 226.2 / 3)
})


test_that("continous barplot can have a white space", {
  p_bar <- plot_tablebarplot(
    dat = test_data,
    bar_label = NULL,
    bar_sig = "bar_sig",
    bar_fill = "bar_fill",
    headers = list("est_1", "est_2", "a barplot"),
    column_spanners = list("spanner_2" = c(2, 3), "spanner_1" = 1),
    column_spanners_2 = list("spanner_3" = 3, "spanner_2" = c(1, 2)),
    columns_table = list("est_1", "se_1"),
    columns_table_sig_bold = list(NULL, "bar_sig"),
    columns_table_sig_superscript = list("bar_sig", "bar_sig"),
    bar_est = "est_1",
    y_axis = "state_var",
    plot_settings = plotsettings_tablebarplot(
      background_stripes_colour = c("white", "lightgrey"),
      bar_fill_colour = c("red", "blue", "yellow", "green"),
      bar_pattern_fill_colour = "white",
      bar_pattern_type = c("stripe", "none"),
      bar_type = "pattern",
      columns_width = c(0.3, 0.2, 0.5),
      bar_pattern_width = 0.5,
      bar_pattern_spacing = 0.1,
      headers_font_size = 5,
      space_right = 5
    )
  )

  # wenn nicht benannt, dann benennen der Farbsettings
  vdiffr::expect_doppelganger("Tablebar with white space", p_bar)
})

test_that("Vlines are plotted correctly", {
  plot_settings_test <- list(
    bar_background_lines_spanners = list(c(1, 4), c(5, 9)),
    bar_background_lines = "scale_breaks",
    bar_background_lines_linetype = "solid",
    bar_background_lines_colour = "red",
    bar_background_0line_linetype = "solid",
    bar_background_0line_colour = "blue"
  )

  df_test <- data.frame(
    x = c(0:10),
    y = 1:11
  )

  vdiffr::expect_doppelganger(
    "row spanners",
    ggplot2::ggplot(df_test, ggplot2::aes(x, y)) +
      ggplot2::coord_cartesian(ylim = c(0, 11)) +
      add_vlines(plot_settings_test,
        bar_est = "x", plot_borders = c(0, 10), y_axis = 11:1 # has to be inverted
      )
  )


  plot_settings_test_2 <- list(
    bar_background_lines_spanners = NULL,
    bar_background_lines = "border",
    bar_background_lines_linetype = "dashed",
    bar_background_lines_colour = "red",
    bar_background_0line_linetype = "solid",
    bar_background_0line_colour = "blue"
  )


  vdiffr::expect_doppelganger(
    "background lines without row spanners",
    ggplot2::ggplot(df_test, ggplot2::aes(x, y)) +
      ggplot2::coord_cartesian(ylim = c(0, 11)) +
      add_vlines(plot_settings_test_2,
        plot_borders = c(0, 10), bar_est = "x", y_axis = 11:1 # has to be inverted
      )
  )
})

test_that("Stacked barplot works", {
  prep_tablebarplot(anteile, subgroup_var = "Kgender", parameter = c("niedrig", "mittel", "hoch")) ## Gar nicht nötig?

  ## Zur not muss man sich die benötigten Parameter selbst zusammenfiltern und dann cbinden

  dat <- anteile$plain[anteile$plain$parameter != "Ncases", ]
  dat$subgroup_var <- dat$Kgender
  dat$est <- dat$est * 100
  dat$label <- paste0(dat$est, "%")




dat$subgroup_var <- gsub("weiblich", "Mädchen", dat$subgroup_var)
dat$subgroup_var <- gsub("maennlich", "Jungen", dat$subgroup_var)
dat$subgroup_var <- gsub("total", "Gesamt", dat$subgroup_var)
dat$subgroup_var <- factor(dat$subgroup_var, levels = c("Gesamt",  "Mädchen",  "Jungen"), ordered = TRUE)
dat$var <- gsub('Selbstkonzept Deutsch', 'Selbstkonzept', dat$var)
#Currently, the order is steered by the bar_fill argument.
dat$parameter <- factor(dat$parameter, levels = c("niedrig", "mittel", "hoch"), ordered = TRUE)
dat <- dat[order(dat$subgroup_var), ]
dat[duplicated(dat$var ), 'var'] <- NA


na_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(dat)))
colnames(na_row) <- colnames(dat)
dat <- rbind(na_row, dat)
dat[1, "var"] <- "**Deutsch**"


dat$y_axis <- c(4, rep(1, 3), rep(2, 3), rep(3, 3))
# dat <- dat[order(dat$y_axis, decreasing = TRUE), ]
dat$mean <- c(NA, rep(1, 3), rep(2, 3), rep(3, 3))
dat$sd <- c(NA, rep(1, 3), rep(2, 3), rep(3, 3))
dat$diff <- c(NA, NA, NA, NA, 0.18, 0.18, 0.18, NA, NA, NA)
dat$se <- c(NA, NA, NA, NA, 0.08, 0.08, 0.08, NA, NA, NA)
dat$d <- c(NA, NA, NA, NA, 0.28, 0.28, 0.28, NA, NA, NA)
dat$sig <- c(NA, NA, NA, NA, 0.04, 0.04, 0.04, NA, NA, NA)


dat_eng <- dat
dat_eng[1, "var"] <-  "**Englisch**"
dat_eng$y_axis <- c(8, rep(5, 3), rep(6, 3), rep(7, 3))

dat <- rbind(dat, dat_eng)

## Everything that is in a stacked group needs to be duplicated per group and therefore, duplicates can be removed.
#
# column_widths_stand <- standardize_column_width(
#   column_widths = list(
#     p1 = c(0.25, 0.25, NA),
#     p2 = c(0.25, 0.25),
#   ),
#   plot_ranges = c(40, 50, 20) # Range of the x-axes of both plots set in 'axis_x_lims'.
# )

library(ggview)
p_stacked <- plot_tablebarplot(dat,
                    bar_est = "est",
                    bar_label = "label",
                    bar_fill = "parameter",
                    columns_table = c("var", "subgroup_var"),
                    headers = c("**Merkmal**", "", ""),
                    y_axis = "y_axis",
                    plot_settings = plotsettings_tablebarplot(
                      bar_type = "stacked",
                      bar_fill_colour = c("niedrig" =  "#EBFDF3", "mittel" = "#8DEBBC", "hoch" = "#20D479"),
                      columns_alignment = c(0, 0.5),
                      columns_width = c(0.1, 0.1, 0.8),
                      ## noch nicht optimal:
                      background_stripes_colour =  rep(c("white", rep("#EBFDF3", 3), rep("white", 3), rep("#EBFDF3", 3)), columns_width = c(0.2, 0.15, 0.65),2),
                      bar_label_size = 1.4,
                      bar_label_nudge_x =rep(0.5, 20),
                    default_list = barplot_table_plot_pattern)) +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

## Remove duplicates in relevant rows
dat_table <- dat[!duplicated(dat[,  c("mean", "sd", "diff", "se", "d", "y_axis")]), c("y_axis","mean", "sd", "diff", "se", "d")]

p_table <- plot_tablebarplot(
  dat_table,
  columns_table = c("mean", "sd", "diff", "se", "d"),
  headers = c("***M***", "***SD***", "***M<sub>M</sub> - M<sub>J</sub>***", "***(SE)***", "***d***"),
  columns_table_se = list(NULL, NULL, NULL, "se", NULL),
  y_axis = "y_axis",
  columns_round = c(2, 2, 2, 2,2),
  columns_table_sig_bold = c(NULL, NULL, 'sig', NULL, 'sig'),
  plot_settings = plotsettings_tablebarplot(
      columns_alignment = c(0.5, 0.5, 0.5, 0.5, 0.5),
    columns_width = rep(0.2, 5),
    background_stripes_colour =  rep(c("white", "#EBFDF3", "white",  "#EBFDF3"),2),
    columns_nudge_y = c(0, 0, -0.5, -0.5, -0.5),
  default_list = barplot_table_plot_pattern))



## This doesn't work because thhe axis of both plots is not the same because the second one doesn't have a
## bar
## 1) Check how the axis is set, if no bar is present.

## Great, just by providing plot_width, I can set the width manually.
## In the function for calculating the table widths, I can also implement an optional argument, where the manually set plot_width can be provided to standardize the column widths.
p_combined <- combine_plots(list(p_stacked, p_table), plot_widths = c(0.8, 0.2))  +
  canvas(210, 297.2/4,  units = "mm")

# save_plot(p_combined, "C:/Users/hafiznij/Downloads/stacked.pdf",  height = 297.4/4, width = 210)


vdiffr::expect_doppelganger("Stacked barplot", p_stacked)



})


