test_that("simple data preperation works", {
  dat_prep <- prep_tablebarplot(trend_2, facet_var = "country", parameter = "mean")

  dat_out <- data.frame(
    depVar = "score",
    subgroup_var = rep("total", 4),
    state_var = c("countryA", "countryB", "countryC", "total"),
    est_mean_comp_none_NA = c(511.563, 508.601, 534.234, 522.668),
    se_mean_comp_none_NA = c(3.505, 4.671, 3.257, 2.183),
    p_mean_comp_none_NA = c(0, 0, 0, 0),
    sig_mean_comp_none_NA = c(TRUE, TRUE, TRUE, TRUE),
    y_axis = 1:4
  )

  rownames(dat_prep) <- NULL
  rownames(dat_out) <- NULL

  expect_equal(dat_prep, dat_out)
})

test_that("estimates are prepped correctly", {
  dat_lineplot_1 <- prep_lineplot(
    trend_gender[[1]],
    subgroup_var = "Kgender" ## Leave this argument if you have only one subgroup
  )

  dat_comp <- trend_gender[[1]]$plain

  dat_berlin <- dat_lineplot_1[dat_lineplot_1$state_var == "Berlin" & dat_lineplot_1$trend == "2009_2015" & dat_lineplot_1$year == 2009 & dat_lineplot_1$subgroup_var == "female", ]

  subset(dat_comp, TR_BUNDESLAND == "Berlin" & Kgender == "female - total" & comparison == "crossDiff" & parameter == "mean" & year == 2009, select = est)

  expect_equal(dat_berlin$est_mean_comp_none, subset(dat_comp, TR_BUNDESLAND == "Berlin" & Kgender == "female" & parameter == "mean" & comparison == "none" & year == 2009, select = est)[[1]])

  expect_equal(
    dat_berlin$est_mean_comp_crossDiff_sameFacet_totalSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin" & Kgender == "female - total" & comparison == "crossDiff" & parameter == "mean" & year == 2009, select = est)[[1]]
  )

  expect_equal(
    dat_berlin$est_mean_comp_crossDiff_totalFacet_sameSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin - total" & Kgender == "female" & comparison == "crossDiff" & parameter == "mean" & year == 2009, select = est)[[1]]
  )

  expect_equal(
    dat_berlin$est_mean_comp_crossDiff_of_groupDiff_totalFacet_maleSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin - total" & Kgender == "female - male" & comparison == "crossDiff_of_groupDiff" & parameter == "mean" & year == 2009, select = est)[[1]]
  )

  expect_equal(
    dat_berlin$est_mean_comp_trend_crossDiff_totalFacet_totalSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin - total" & Kgender == "female - total" & comparison == "trend_crossDiff" & parameter == "mean" & year == "2015 - 2009", select = est)[[1]]
  )

  expect_equal(
    dat_berlin$est_mean_comp_trend_crossDiff_totalFacet_sameSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin - total" & Kgender == "female" & comparison == "trend_crossDiff" & parameter == "mean" & year == "2015 - 2009", select = est)[[1]]
  )

  expect_equal(
    dat_berlin$est_mean_comp_trend_crossDiff_of_groupDiff_totalFacet_maleSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin - total" & Kgender == "female - male" & comparison == "trend_crossDiff_of_groupDiff" & parameter == "mean" & year == "2015 - 2009", select = est)[[1]]
  )
})



test_that("p-values are prepped correctly", {
  dat_lineplot_1 <- prep_lineplot(
    trend_gender[[1]],
    subgroup_var = "Kgender" ## Leave this argument if you have only one subgroup
  )

  dat_comp <- trend_gender[[1]]$plain

  dat_berlin <- dat_lineplot_1[dat_lineplot_1$state_var == "Berlin" & dat_lineplot_1$trend == "2015_2022" & dat_lineplot_1$year == 2015 & dat_lineplot_1$subgroup_var == "female", ]


  expect_equal(dat_berlin$p_mean_comp_none, subset(dat_comp, TR_BUNDESLAND == "Berlin" & Kgender == "female" & parameter == "mean" & comparison == "none" & year == 2015, select = p)[[1]])

  expect_equal(
    dat_berlin$p_mean_comp_crossDiff_sameFacet_totalSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin" & Kgender == "female - total" & comparison == "crossDiff" & parameter == "mean" & year == 2015, select = p)[[1]]
  )

  expect_equal(
    dat_berlin$p_mean_comp_crossDiff_totalFacet_sameSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin - total" & Kgender == "female" & comparison == "crossDiff" & parameter == "mean" & year == 2015, select = p)[[1]]
  )

  expect_equal(
    dat_berlin$p_mean_comp_crossDiff_of_groupDiff_totalFacet_maleSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin - total" & Kgender == "female - male" & comparison == "crossDiff_of_groupDiff" & parameter == "mean" & year == 2015, select = p)[[1]]
  )

  expect_equal(
    dat_berlin$p_mean_comp_trend_crossDiff_totalFacet_totalSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin - total" & Kgender == "female - total" & comparison == "trend_crossDiff" & parameter == "mean" & year == "2022 - 2015", select = p)[[1]]
  )

  expect_equal(
    dat_berlin$p_mean_comp_trend_crossDiff_totalFacet_sameSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin - total" & Kgender == "female" & comparison == "trend_crossDiff" & parameter == "mean" & year == "2022 - 2015", select = p)[[1]]
  )

  expect_equal(
    dat_berlin$p_mean_comp_trend_crossDiff_of_groupDiff_totalFacet_maleSubgroup,
    subset(dat_comp, TR_BUNDESLAND == "Berlin - total" & Kgender == "female - male" & comparison == "trend_crossDiff_of_groupDiff" & parameter == "mean" & year == "2022 - 2015", select = p)[[1]]
  )
})



test_that("p-values are prepped correctly for total gropu", {
  dat_lineplot_1 <- prep_lineplot(
    trend_gender[[1]],
    subgroup_var = "Kgender" ## Leave this argument if you have only one subgroup
  )

  dat_comp <- trend_gender[[1]]$plain

  dat_total <- dat_lineplot_1[dat_lineplot_1$state_var == "total" & dat_lineplot_1$trend == "2015_2022" & dat_lineplot_1$year == 2015 & dat_lineplot_1$subgroup_var == "female", ]


  expect_equal(dat_total$p_mean_comp_none, subset(dat_comp, TR_BUNDESLAND == "total" & Kgender == "female" & parameter == "mean" & comparison == "none" & year == 2015, select = p)[[1]])

  expect_equal(dat_total$est_mean_comp_crossDiff_totalFacet_totalSubgroup, as.numeric(NA))
  expect_equal(
    dat_total$est_mean_comp_crossDiff_totalFacet_sameSubgroup,
    as.numeric(NA)
  )
  expect_equal(dat_total$p_mean_comp_crossDiff_sameFacet_totalSubgroup, subset(dat_comp, TR_BUNDESLAND == "total" & Kgender == "female - total" & parameter == "mean" & comparison == "crossDiff" & year == 2015, select = p)[[1]])
  expect_equal(dat_total$est_mean_comp_crossDiff_sameFacet_totalSubgroup, subset(dat_comp, TR_BUNDESLAND == "total" & Kgender == "female - total" & parameter == "mean" & comparison == "crossDiff" & year == 2015, select = est)[[1]])
})



## Hier gabs ein paar Probleme bei Jules Datenaufbereitung
# ## multiple subgroups
#
# dat <- readRDS("Q:/BT2024/BT/60_Bericht/05_Geschlechtsbezogene_Disparitaeten/04_Rechnungen/Hauptrechnungen_Outputs/report2_02_alleKb_Deu_Gy_BT24.rds")[[1]]
#
# library(tidyverse)
# dat$group <- dat$group %>%
#   filter()
# ## Wenn zwei subgroup_vars enthalten sind brauchen wir auch noch die Info auf welche subgroup_var es sich bezieht in
# ## den column names
# ## Also: Wenn ein Vektor an subgroup_vars provided wird, müssen BEIDE so behandelt werden wie subgroup_vars
#
# res <- prep_tablebarplot(dat, facet_var = "TR_Gymnasium_DUMMY", subgroup_var = "TR_SEX")

