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

  eatRep_dat_long <- eatRep_dat_long[eatRep_dat_long$comparison %in% c(line_sig, brace_label_est), c("id", "group", "est", "p")]

  ## By the way! Not done here, still have to deal with the comparisons of comparisons.
  eatRep_dat_merged <- merge(eatRep_dat_long,
                             eatRep_dat$group_estimates,
                             by.x = "group",
                             by.y = "id",
                             all.x = TRUE,
                             suffixes = c("_comp", "_point"))


  # Split the data frame by 'id', apply the function, and then combine the results
  dat_final <- do.call(rbind, lapply(split(eatRep_dat_merged, eatRep_dat_merged$id), create_trend))
  rownames(dat_final) <- NULL


  dat_final$line_sig <- ifelse(dat_final$p_comp < 0.05, TRUE, FALSE)
  dat_final$point_sig <- ifelse(dat_final$p_point < 0.05, TRUE, FALSE)


  grouping_var = "mhg"
  ## Only into factor, if not already a factor:
  grouping_var_lvls <- levels(factor(dat_final[, grouping_var]))


  plot_lims <- calc_plot_lims(dat_final, plot_settings_expanded)
  brace_coordinates <- calc_brace_coords(dat_final,
                                         grouping_var_lvls,
                                         plot_lims$coords,
                                         plot_settings = plot_settings_expanded
  )

  brace_dat <- prep_brace(dat_final, brace_coords = brace_coordinates)

  list_final <- list(dat_final = dat_final, brace_dat = brace_dat, plot_lims = plot_lims)

  return(list_final)
}

create_trend <- function(df) {
  df$trend <- paste(df$year[1], df$year[2], sep = "_")
  return(df)
}

paste_trend_years <- function(years_list) {
  vapply(years_list, paste0, collapse = "_", FUN.VALUE = character(1))
}
