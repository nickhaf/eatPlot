#' Prepare lineplot data.
#'
#' @param eatRep_dat Object returned by `eatRep`.
#' @param line_sig Character string. Contains the variable in column `comparison` that is used for plotting significances of the lines.
#' @param parameter Character string. Contains the value in column `parameter` that is used for plotting the lines.  Defaults to `"mean"`.
#' @param years_lines  List of numeric vectors containing the start and end year, between which a trend line should be plotted. Per default, lines are drawn from every year to the next consecutive year.
#' @param years_braces List of numeric vectors containing the start and end year, between which a brace should be plotted. Per default, braces are drawn from the last year to every other year included in the data.
#' @param facet_var Character string of the name of the variable that should be used for faceting the plot. Defaults to `"TR_BUNDESLAND"`.
#' @param total_group Character string of the name of the total groups containing all other groups of the facet var. Defaults to `"total"`.
#' @param sig_niveau Numeric indicating the border below which p-values will be considered significant. Defaults to `0.05`.
#'
#' @return Data prepared for plotting the BT-lineplots.
#' @export
#'
#' @examples # tbd
prep_lineplot <- function(eatRep_dat, parameter, facet_var = "TR_BUNDESLAND", total_group = "total", sig_niveau = 0.05, comparisons = NULL) {

  # Check input -------------------------------------------------------------
  check_eatRep_dat(eatRep_dat)
  check_columns(eatRep_dat$estimate, cols = c("p"))

  # Filtering ---------------------------------------------------------------
  eatRep_dat <- rename_comparisons_total(eatRep_dat, total_group, facet_var = facet_var)
  eatRep_dat$plain <- NULL
  eatRep_dat$estimate <- eatRep_dat$estimate[eatRep_dat$estimate$parameter == parameter, ]
  eatRep_dat$group$year <- as.numeric(eatRep_dat$group$year)
  if(!is.null(comparisons)){
    dat <- dat[dat$comparison %in% c(comparisons, paste0(comparisons, "Total")), ]
  }

  # Merge Data --------------------------------------------------------------
  check_no_columns(eatRep_dat$estimate, cols = "sig")
  eatRep_dat$estimate$sig <- ifelse(eatRep_dat$estimate$p < sig_niveau, TRUE, FALSE)
  plot_dat <- build_plot_dat(eatRep_dat)
  plot_dat_wide <- tidyr::pivot_wider(plot_dat,
    names_from = "comparison",
    values_from = c("est_comparison", "se_comparison", "sig_comparison")
  ) |>
    as.data.frame()

  for (i in colnames(plot_dat_wide)[grep("sig", colnames(plot_dat_wide))]) {
    plot_dat_wide[, i] <- ifelse(is.na(plot_dat_wide[, i]), FALSE, plot_dat_wide[, i])
  }


# Order columns -----------------------------------------------------------
  auxillary_colnames <- grep("^est_|^es_|^p_|^sig_|^se_", colnames(plot_dat_wide), invert = TRUE, value = TRUE)
  value_colnames <- sort(grep("^est_|^es_|^p_|^sig_|^se", colnames(plot_dat_wide), value = TRUE))
  plot_dat_wide <- plot_dat_wide[, c(auxillary_colnames, value_colnames)]

  return(plot_dat_wide)
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
      select(id, comparison, group, est, se, p, sig, es)


    eatRep_dat_no_nested_comps <- eatRep_dat_long %>%
      dplyr::filter(stringr::str_detect(group, "comp_", negate = TRUE)) %>%
      select(c("id", "comparison", "group", "est", "se", "p", "sig", "es"))

    eatRep_dat_long <- rbind(eatRep_dat_no_nested_comps, eatRep_dat_nested_comps[,c("id", "comparison", "group", "est", "se", "p", "sig", "es")])
  }

  eatRep_dat_long <- eatRep_dat_long[, c("id", "comparison", "group", "est", "se", "p", "sig", "es")]


  ###############
  eatRep_dat_merged <- merge(eatRep_dat_long,
    eatRep_dat$group_estimates,
    by.x = "group",
    by.y = "id",
    all.y = TRUE,
    suffixes = c("_comparison", "_comparison_none")
  )

  plot_dat <- do.call(rbind, lapply(split(eatRep_dat_merged, eatRep_dat_merged$id), create_trend))
  rownames(plot_dat) <- NULL

  return(plot_dat)
}

prep_years_list <- function(years_lines, years_braces) {
  years_list <- lapply(list(years_lines, years_braces), prep_years)
  names(years_list) <- c("years_lines", "years_braces")
  return(years_list)
}
