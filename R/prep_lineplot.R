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
prep_lineplot <- function(
    eatRep_dat, subgroup_var, line_sig, point_sig, point_est, brace_label_est, brace_label_se, brace_label_sig_high, brace_label_sig_bold, parameter, years_lines, years_braces, facet_var = "TR_BUNDESLAND",
    background_level, background_group, plot_settings) {
  # Check input -------------------------------------------------------------
  check_eatRep_dat(eatRep_dat)

  years_list <- prep_years_list(years_lines, years_braces)


  # Filtering ---------------------------------------------------------------
  eatRep_dat$plain <- NULL
  eatRep_dat$estimate <- eatRep_dat$estimate[eatRep_dat$estimate$parameter == parameter, ]
  eatRep_dat$group$year <- as.numeric(eatRep_dat$group$year)

  used_comps <- c(line_sig, brace_label_est, brace_label_se, brace_label_sig_high, brace_label_sig_bold)
  eatRep_dat$comparisons <- eatRep_dat$comparisons[eatRep_dat$comparisons$comparison %in% used_comps, ]

  # Merge Data --------------------------------------------------------------
  plot_dat <- build_plot_dat(eatRep_dat) |>
    build_column(subgroup_var, "subgroup_var") |>
    build_column(facet_var, "facet_var")

  if (is.na(background_group)) {
    background_group <- "noGroup"
    plot_dat[, "subgroup_var"][is.na(plot_dat$subgroup_var)] <- background_group
  }

  ## Only into factor, if not already a factor:
  subgroup_lvls <- levels(factor(plot_dat$subgroup_var))


  # Coordinates -------------------------------------------------------------
  ## Some coordinates are needed to set the plot into the correct margins:
  plot_lims <- calc_plot_lims(plot_dat, subgroup_lvls, years_list, plot_settings = plot_settings)


  # Wide Format -------------------------------------------------------------
  plot_dat_wide <- tidyr::pivot_wider(plot_dat,
    names_from = "comparison",
    values_from = c("est_comp", "se_comp", "sig_comp")
  ) |>
    as.data.frame()



  ######### Hier nohc eine Schleife rum, sodass theoretisch auch andere Comparisons geplottet werden können.
  plot_dat_wide$point_est <- plot_dat_wide$est_point
  plot_dat_wide$point_sig <- plot_dat_wide$sig_point
  ############
  plot_dat_wide$line_sig <- plot_dat_wide[, paste0("sig_comp_", line_sig)]
  plot_dat_wide$brace_label_est <- plot_dat_wide[, paste0("est_comp_", brace_label_est)]
  plot_dat_wide$brace_label_se <- plot_dat_wide[, paste0("se_comp_", brace_label_se)]
  plot_dat_wide$brace_label_sig_high <- plot_dat_wide[, paste0("sig_comp_", brace_label_sig_high)]
  plot_dat_wide$brace_label_sig_bold <- plot_dat_wide[, paste0("sig_comp_", brace_label_sig_bold)]

  for (i in colnames(plot_dat_wide)[grep("sig", colnames(plot_dat_wide))]) {
    plot_dat_wide[, i] <- ifelse(is.na(plot_dat_wide[, i]), FALSE, plot_dat_wide[, i])
  }


  background_line_dat <- subset(plot_dat_wide,
                                facet_var == background_level &
                                  subgroup_var == background_group)
  plot_dat_wide <- subset(plot_dat_wide, subgroup_var != background_group)
  plot_dat_wide <- subset(plot_dat_wide, plot_dat_wide$facet_var != background_level)



  line_dat <- plot_dat_wide


  brace_dat_list <- prep_brace(plot_dat_wide, plot_lims, plot_settings)
  brace_dat <- brace_dat_list$brace_dat
  brace_coordinates <- brace_dat_list$brace_coords


  # The multiplicator here is not that easy to understand, possiby take something else.
  plot_lims$y_lims_total <- c(min(brace_coordinates$group_labels$label_pos_y) - diff(range(plot_lims$coords)) * 0.06, max(plot_lims$coords)) # a bit smaller, so the labels don't get cut off

  line_dat <- filter_years(line_dat, years = years_lines)
  background_line_dat <- filter_years(background_line_dat, years = years_lines)

  brace_dat <- filter_years(brace_dat, years = years_braces)

  if (!checkmate::test_subset(vapply(years_lines, paste0, collapse = "_", FUN.VALUE = character(1)), choices = line_dat$trend)) {
    stop("Some of the trends you provided in 'years_lines' are not in the data.")
  }

  if (!checkmate::test_subset(vapply(years_braces, paste0, collapse = "_", FUN.VALUE = character(1)), choices = line_dat$trend)) {
    stop("Some of the trends you provided in 'years_braces' are not in the data.")
  }


  list_final <- list(plot_dat = line_dat, brace_dat = brace_dat_list, background_line_dat = background_line_dat, plot_lims = plot_lims, plot_settings = plot_settings)

  return(list_final)
}

create_trend <- function(df) {
  df$trend <- paste(df$year[1], df$year[2], sep = "_")
  return(df)
}



build_plot_dat <- function(eatRep_dat) {
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

  # nested comparisons ------------------------------------------------------
  # trend_crossDiff has another comparison in the group column
  # Goal: only have groups in the group column.

  ## Make into a function and apply until there is no comp in the group column any more

  if (any(grep("comp_", eatRep_dat_long$group))) {
    eatRep_dat_nested_comps <- eatRep_dat_long %>%
      filter(str_detect(group, "comp_")) %>%
      left_join(eatRep_dat$comp_estimates[, c("id", "unit_1", "unit_2")], join_by(group == id)) %>%
      mutate(id = paste(id, group, sep = "_")) %>%
      dplyr::select(-group) %>%
      pivot_longer(
        cols = c("unit_1", "unit_2"),
        names_to = "unit_b",
        values_to = "group"
      ) %>%
      filter(str_detect(group, "comp_")) %>%
      left_join(eatRep_dat$comp_estimates[, c("id", "unit_1", "unit_2")], join_by(group == id)) %>%
      mutate(id = paste(id, group, sep = "_")) %>%
      dplyr::select(-group) %>%
      pivot_longer(
        cols = c("unit_1", "unit_2"),
        names_to = "unit_c",
        values_to = "group"
      ) %>%
      select(id, comparison, group, est, se, p)



    eatRep_dat_long <- rbind(eatRep_dat_long %>%
      filter(str_detect(group, "comp_", negate = TRUE)), eatRep_dat_nested_comps)
  }

  eatRep_dat_long <- eatRep_dat_long[, c("id", "comparison", "group", "est", "se", "p")]


  ###############
  eatRep_dat_merged <- merge(eatRep_dat_long,
    eatRep_dat$group_estimates,
    by.x = "group",
    by.y = "id",
    all.y = TRUE,
    suffixes = c("_comp", "_point")
  )


  plot_dat <- do.call(rbind, lapply(split(eatRep_dat_merged, eatRep_dat_merged$id), create_trend))
  rownames(plot_dat) <- NULL


  plot_dat$sig_comp <- ifelse(plot_dat$p_comp < 0.05, TRUE, FALSE)

  plot_dat$sig_point <- ifelse(plot_dat$p_point < 0.05, TRUE, FALSE)

  return(plot_dat)
}

prep_years_list <- function(years_lines, years_braces) {
  years_list <- lapply(list(years_lines, years_braces), prep_years)
  names(years_list) <- c("years_lines", "years_braces")
  return(years_list)
}
