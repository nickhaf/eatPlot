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


  ## Einstellungen noch unter "stacked" = TRUE zusammenführen
  dat_prepped <- prep_tablebarplot(anteile, subgroup_var = "Kgender", parameter = NULL, names_from_none = c("year"),
                                   names_from_comp = c("comparison_split", "trend"), total_subgroup = "total") ## set to total!?

  dat_prepped_means <- prep_tablebarplot(means, subgroup_var = "Kgender", parameter = c("mean", "sd"), total_subgroup = "total") ## set to total!?


  # dat <- anteile$plain[anteile$plain$parameter != "Ncases", ]
  # dat$subgroup_var <- dat$Kgender
  # dat$est <- dat$est * 100
  #
  #

## If I wanted to keep var, I could put it into a merged subgroup column for now!

dat_prepped$var <- rep('Selbstkonzept Deutsch', nrow(dat_prepped))


  dat_prepped$subgroup_var <- gsub("weiblich", "Mädchen", dat_prepped$subgroup_var)
  dat_prepped$subgroup_var <- gsub("maennlich", "Jungen", dat_prepped$subgroup_var)
  dat_prepped$subgroup_var <- gsub("total", "Gesamt", dat_prepped$subgroup_var)

  # Currently, the order is steered by the bar_fill argument.
  dat_prepped <- subset(dat_prepped, parameter != "Ncases")
  dat_prepped$parameter <- factor(  dat_prepped$parameter, levels = c("niedrig", "mittel", "hoch"), ordered = TRUE)
  dat_prepped <-   dat_prepped[order(  dat_prepped$subgroup_var), ]
  dat_prepped[duplicated(  dat_prepped$var ), 'var'] <- NA


na_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(dat_prepped)))
colnames(na_row) <- colnames(dat_prepped)
dat_prepped <- rbind(na_row, dat_prepped)
dat_prepped[1, "var"] <- "**Deutsch**"
dat_prepped$label <- paste0(dat_prepped$est_NA_comp_none, "%")
dat_prepped <- dat_prepped[dat_prepped$state_var == "total", ]




dat_prepped_other_groups <- subset(dat_prepped[-1, ], subgroup_var != "Gesamt")
dat_prepped_other_groups$subgroup_var <- gsub("Jungen","Max. 100 Bücher",  dat_prepped_other_groups$subgroup_var)
dat_prepped_other_groups$subgroup_var <- gsub("Mädchen", "Mehr als 100 Bücher", dat_prepped_other_groups$subgroup_var)

dat_prepped_other_groups2 <- subset(dat_prepped[-1, ], subgroup_var != "Gesamt")
dat_prepped_other_groups2$subgroup_var <- gsub( "Jungen", "ZWH", dat_prepped_other_groups2$subgroup_var)
dat_prepped_other_groups2$subgroup_var <- gsub( "Mädchen", "ohne ZWH", dat_prepped_other_groups2$subgroup_var)

dat_prepped_3 <- rbind(dat_prepped, dat_prepped_other_groups, dat_prepped_other_groups2)

dat_prepped_3$y_axis <- c(8, rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3), rep(5, 3), rep(6, 3), rep(7, 3))
dat_prepped_3[1, "var"] <- "**Deutsch**"

dat_eng <- dat_prepped_3
dat_eng[1, "var"] <-  "**Englisch**"
dat_eng$y_axis <- c(16, rep(9, 3), rep(10, 3), rep(11, 3), rep(12, 3), rep(13, 3), rep(14, 3), rep(15, 3))


dat_eng_2 <- dat_eng
dat_eng[1, "var"] <-  "**Biologie**"
dat_eng$y_axis <- c(24, rep(17, 3), rep(18, 3), rep(19, 3), rep(20, 3), rep(21, 3), rep(22, 3), rep(23,3))

dat_eng_3 <- dat_eng
dat_eng[1, "var"] <-  "**Mathe**"
dat_eng$y_axis <- c(32, rep(25, 3), rep(26, 3), rep(27, 3), rep(28, 3), rep(29,3), rep(30, 3), rep(31,3))



dat <- rbind(dat_prepped_3, dat_eng, dat_eng_2, dat_eng_3)

#dat$subgroup_var <- factor(dat$subgroup_var, levels = c("Gesamt",  "Mädchen",  "Jungen", ""), ordered = TRUE)


## Everything that is in a stacked group needs to be duplicated per group and therefore, duplicates can be removed.

dat$est_NA_comp_none <- dat$est_NA_comp_none * 100

### Abbildung 9.1

library(ggview)
p_stacked <- plot_tablebarplot(dat,
                    bar_est = "est_NA_comp_none",
                    bar_label = "label",
                    bar_fill = "parameter",
                    columns_table = c("var", "subgroup_var"),
                    headers = c("**Merkmal**", "", ""),
                    y_axis = "y_axis",
                    plot_settings = plotsettings_tablebarplot(
                      axis_x = FALSE,
                      bar_type = "stacked",
                      bar_background_lines = "none",
                      bar_fill_colour = c("niedrig" =  "#20D479", "mittel" = "#8DEBBC", "hoch" = "#EBFDF3"),
                      bar_label_colour = c("niedrig" =  "white", "mittel" = "black", "hoch" = "black"),
                      columns_alignment = c(0, 0),
                      columns_width = c(0.1, 0.1, 0.8),
                      ## noch nicht optimal:
                      background_stripes_colour =  rep("white", nrow(dat)),
                      bar_label_size = 1.4,
                      bar_label_nudge_x =rep(0.5, nrow(dat)),
                    default_list = barplot_table_plot_pattern))

## The means are from a seperate analysis. Its important that the resulting table has the same number of rows as the stacked one.
## Because in the stacked table we multiple rows in the data frame per row in the plot, we have to deal with the data accordingly:

dat_prepped_means <- subset(dat_prepped_means, state_var == "total")

## To be safe, we merge. This will keep the order:



dat_prepped_means$subgroup_var <- gsub("weiblich", "Mädchen", dat_prepped_means$subgroup_var)
dat_prepped_means$subgroup_var <- gsub("maennlich", "Jungen", dat_prepped_means$subgroup_var)
dat_prepped_means$subgroup_var <- gsub("total", "Gesamt", dat_prepped_means$subgroup_var)
dat_prepped_means$subgroup_var <- factor(dat_prepped_means$subgroup_var, levels = c("Gesamt",  "Mädchen",  "Jungen"), ordered = TRUE)

dat_table_2 <- subset(dat_prepped_means, subgroup_var != "Gesamt")

dat_table_3 <- subset(dat_prepped_means, subgroup_var != "Gesamt")

dat_table_2$subgroup_var <- gsub( "Jungen", "ZWH", dat_table_2$subgroup_var)
dat_table_2$subgroup_var <- gsub( "Mädchen", "ohne ZWH", dat_table_2$subgroup_var)

dat_table_3$subgroup_var <- gsub("Jungen","Max. 100 Bücher", dat_table_3$subgroup_var)
dat_table_3$subgroup_var <- gsub("Mädchen", "Mehr als 100 Bücher", dat_table_3$subgroup_var)

dat2 <- rbind(dat_prepped_means, dat_table_2, dat_table_3)

dat2

# dat$subgroup_var[is.na(dat$subgroup_var)] <- ""

## Using join to preserve the order
dat_table <- dat |>
  dplyr::left_join(dat2[, c("subgroup_var", "est_mean_comp_none_NA", "est_sd_comp_none_NA", "est_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA", "sig_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA", "se_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA", "es_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA" )])

## remove duplicates
dat_table_2 <- dat_table[, c("subgroup_var", "est_mean_comp_none_NA", "est_sd_comp_none_NA", "est_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA", "sig_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA", "se_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA", "es_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA", "var", "y_axis")]

dat_table_u <- unique(dat_table_2)


## Round auf zwei Stellen:
dat_table_u$se_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA

p_table <- plot_tablebarplot(
  dat_table_u,
  columns_table = c("est_mean_comp_none_NA", "est_sd_comp_none_NA", "est_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA", "se_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA", "es_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA"),
  headers = c("***M***", "***SD***", "***M<sub>M</sub>-M<sub>J</sub>***", "***(SE)***", "***d***"),
  columns_round = c(2, 2, 2, 2, 2),
  columns_table_se = list(NULL, NULL, NULL, "se_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA", NULL),
  y_axis = "y_axis",
  columns_table_sig_bold = list(NULL, NULL, 'sig_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA', NULL, 'sig_mean_comp_groupDiff_sameFacet_maennlichSubgroup_NA'),
  plot_settings = plotsettings_tablebarplot(
    bar_background_lines = NULL,
    columns_alignment = c(0.5, 0.5, 0.5, 0.5, 0.5),
    columns_width = rep(0.2, 5),
    background_stripes_colour =  rep("white", nrow(dat_table_u)),
    columns_nudge_y = c(0, 0, 0.5, 0.5, 0.5),
  default_list = barplot_table_plot_pattern))


p_combined <- combine_plots(list(p_stacked, p_table), plot_widths = c(0.8, 0.2))  #+
  #canvas(210, 297.2/4,  units = "mm")

save_plot(p_combined, "C:/Users/hafiznij/Downloads/stacked.pdf",  height = 297.4, width = 210)


vdiffr::expect_doppelganger("Stacked barplot", p_stacked)


})


