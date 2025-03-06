test_that("Value in the prepped columns can be traced back to plain data frame (table)", {
  prepped_dat <- prep_plot(trend_gender[[1]],
            subgroup_var = "Kgender",
            plot_type = "table",
            parameter = c("mean", "sd"))


  prepped_dat_berlin <- prepped_dat[grep("Berlin", prepped_dat$state_var), ]
  original_dat_berlin <- subset(trend_gender[[1]]$plain, grepl("Berlin", trend_gender[[1]]$plain$TR_BUNDESLAND))



  expect_equal(nrow(prepped_dat_berlin), 3)
  expect_equal(subset(prepped_dat_berlin, subgroup_var == "weiblich", select = est_mean_comp_crossDiff_totalFacet_totalSubgroup_2009)[[1]],
               subset(original_dat_berlin,
                      comparison == "crossDiff" &
                      TR_BUNDESLAND == "Berlin - total" &
                      parameter == "mean" &
                      Kgender == "weiblich - total" &
                      year == 2009,
                      select = "est"
                      )[[1]])

  expect_equal(subset(prepped_dat_berlin, subgroup_var == "total", select = est_sd_comp_crossDiff_totalFacet_sameSubgroup_2015)[[1]],
               subset(original_dat_berlin,
                      comparison == "crossDiff" &
                      TR_BUNDESLAND == "Berlin - total" &
                        parameter == "sd" &
                        Kgender == "total" &
                        year == 2015,
                      select = "est"
               )[[1]])


  expect_equal(subset(prepped_dat_berlin, subgroup_var == "maennlich", select = se_mean_comp_groupDiff_sameFacet_weiblichSubgroup_2022)[[1]],
               subset(original_dat_berlin,
                      comparison == "groupDiff" &
                        TR_BUNDESLAND == "Berlin" &
                        parameter == "mean" &
                        Kgender == "maennlich - weiblich" &
                        year == 2022,
                      select = "se"
               )[[1]])

  expect_equal(subset(prepped_dat_berlin, subgroup_var == "weiblich", select = se_mean_comp_groupDiff_sameFacet_maennlichSubgroup_2022)[[1]],
               subset(original_dat_berlin,
                      comparison == "groupDiff" &
                        TR_BUNDESLAND == "Berlin" &
                        parameter == "mean" &
                        Kgender == "maennlich - weiblich" &
                        year == 2022,
                      select = "se"
               )[[1]])


  expect_equal(subset(prepped_dat_berlin, subgroup_var == "maennlich", select = p_mean_comp_trend_groupDiff_sameFacet_weiblichSubgroup_2009_2022)[[1]],
               subset(original_dat_berlin,
                      comparison == "trend_groupDiff" &
                        TR_BUNDESLAND == "Berlin" &
                        parameter == "mean" &
                        Kgender == "maennlich - weiblich" &
                        year == "2022 - 2009",
                      select = "p"
               )[[1]])


  expect_equal(subset(prepped_dat_berlin, subgroup_var == "maennlich", select = est_mean_comp_trend_crossDiff_of_groupDiff_totalFacet_weiblichSubgroup_2009_2015)[[1]],
               subset(original_dat_berlin,
                      comparison == "trend_crossDiff_of_groupDiff" &
                        TR_BUNDESLAND == "Berlin - total" &
                        parameter == "mean" &
                        Kgender == "maennlich - weiblich" &
                        year == "2015 - 2009",
                      select = "est"
               )[[1]])

  expect_true(is.na(subset(prepped_dat_berlin, subgroup_var == "total", select = est_mean_comp_trend_crossDiff_of_groupDiff_totalFacet_weiblichSubgroup_2009_2015)[[1]]))


})
