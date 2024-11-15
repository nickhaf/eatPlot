#' Prepare lineplot data.
#'
#' @param eatRep_dat Object returned by `eatRep`.
#' @param line_sig Character string. Contains the variable in column `comparison` that is used for plotting significances of the lines.
#' @param parameter Character string. Contains the value in column `parameter` that is used for plotting the lines.  Defaults to `"mean"`.
#' @param years_lines  List of numeric vectors containing the start and end year, between which a trend line should be plotted. Per default, lines are drawn from every year to the next consecutive year.
#' @param years_braces List of numeric vectors containing the start and end year, between which a brace should be plotted. Per default, braces are drawn from the last year to every other year included in the data.
#' @return Data prepared for plotting the BT-lineplots.
#' @export
#'
#' @examples # tbd
prep_lineplot <- function(eatRep_dat, line_sig, point_sig, brace_label_est, parameter, years_lines, years_braces) {
  check_eatRep_dat(eatRep_dat)

  eatRep_dat$plain <- NULL
  eatRep_dat$estimate <- eatRep_dat$estimate[eatRep_dat$estimate$parameter == parameter, ]

  eatRep_dat$group_estimates <- merge(eatRep_dat$group,
    eatRep_dat$estimate,
    by = "id",
    all.x = TRUE
  )

  eatRep_dat$comp_estimates <- merge(eatRep_dat$comparisons,
    eatRep_dat$estimate,
    by = "id",
    all.x = TRUE
  )

  eatRep_dat_long <- tidyr::pivot_longer(
      eatRep_dat$comp_estimates,
      cols = starts_with("unit"),
      names_to = "unit",
      values_to = "group"
    )


  ## By the way! Not done here, still have to deal with the comparisons of comparisons.
  # trend_crossDiff has another comparison in the group column

  eatRep_dat_long <- eatRep_dat_long[eatRep_dat_long$comparison %in% c(line_sig, brace_label_est), c("id", "comparison", "group", "est", "p")]

# nested comparisons ------------------------------------------------------
  # trend_crossDiff has another comparison in the group column
  # Goal: only have groups in the group column.

  eatRep_dat_nested_comps <- eatRep_dat_long %>%
    filter(str_detect(group, "comp_")) %>%
    left_join(eatRep_dat$comp_estimates[, c("id", "unit_1", "unit_2")], join_by(group == id)) %>%
    mutate(id = paste(id, group, sep = "_")) %>%
    dplyr::select(-group) %>%
    pivot_longer(cols = c("unit_1", "unit_2"),
                 names_to = "unit_b",
                 values_to = "group") %>%
    filter(str_detect(group, "comp_", negate = TRUE)) %>%
    select(id, comparison, group, est, p)



eatRep_dat_long <- rbind(eatRep_dat_long %>% filter(str_detect(group, "comp_", negate = TRUE)) , eatRep_dat_nested_comps)
###############


  eatRep_dat_merged <- merge(eatRep_dat_long,
                             eatRep_dat$group_estimates,
                             by.x = "group",
                             by.y = "id",
                             all.x = TRUE,
                             suffixes = c("_comp", "_point"))



  # Split the data frame by 'id', apply the function, and then combine the results
  plot_dat <- do.call(rbind, lapply(split(eatRep_dat_merged, eatRep_dat_merged$id), create_trend))
  rownames(plot_dat) <- NULL


  plot_dat$line_sig <- ifelse(plot_dat$p_comp < 0.05, TRUE, FALSE)
  plot_dat$point_sig <- ifelse(plot_dat$p_point < 0.05, TRUE, FALSE)


  grouping_var = "mhg"
  ## Only into factor, if not already a factor:
  grouping_var_lvls <- levels(factor(plot_dat[, grouping_var]))


  plot_lims <- calc_plot_lims(plot_dat, plot_settings_expanded)
  brace_coordinates <- calc_brace_coords(plot_dat,
                                         grouping_var_lvls,
                                         plot_lims$coords,
                                         plot_settings = plot_settings_expanded
  )

  brace_dat <- prep_brace(plot_dat, brace_coords = brace_coordinates)


  # The multiplicator here is not that easy to understand, possiby take something else.
  plot_lims$y_lims_total <- c(min(brace_coordinates$group_labels$label_pos_y) - diff(range(plot_lims$coords)) * 0.06, max(plot_lims$coords)) # a bit smaller, so the labels don't get cut off

  # Add until last brace coord to the plot lims. This is actually done in plot_lims.
  # Would be easier to just take the min value in the brace_coordinates and maybe add a small nudge value in the end.
  # This range would then probably be used in plot_single_lineplot()  in ylim of coord_cartesian.
  #Also: As soon as I have the full coordinates of the whole plot nothing should shange about those anymore. So for all nudging
  ## etc., use this range as multiplicor
## Best to do one after the other, but togehter.

  list_final <- list(plot_dat = plot_dat, brace_dat = brace_dat, plot_lims = plot_lims)

  return(list_final)
}

create_trend <- function(df) {
  df$trend <- paste(df$year[1], df$year[2], sep = "_")
  return(df)
}

