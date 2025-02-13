#' Prepare lineplot data.
#'
#' @param eatRep_dat Object returned by `eatRep`.
#' @param subgroup_var Character string of the column in `eatPlot_dat$group` or `eatPlot_dat$plain` containing the subgroup mapping. Each supgroup will receive it's own line. IF there are subgroups within the data, this needs to be set, otherwise the data preparation might fail.  Defaults to `NULL`.
#' @param total_subgroup Character string indicating the subgroup containing all other groups of the subgroup_var. Defaults to `"total"`.
#' @param parameter Character string. Contains the value in column `parameter` that is used for plotting the lines.  Defaults to `"mean"`.
#' @param facet_var Character string of the name of the variable that should be used for faceting the plot. Defaults to `"TR_BUNDESLAND"`.
#' @param total_facet Character string of the name of the total groups containing all other groups of the facet var. Defaults to `"total"`.
#' @param sig_niveau Numeric indicating the border below which p-values will be considered significant. Defaults to `0.05`.
#' @param comparisons Character string that can be used to filter the needed comparions. This can drastically reduce the output of the prepared data. Defaults to `c("none", "crossDiff", "trend", "trend_crossDiff")`.
#' @return Data prepared for plotting the BT-lineplots.
#' @export
#'
#' @examples # tbd
prep_lineplot <- function(eatRep_dat, subgroup_var = NULL, total_subgroup = "total",
                          facet_var = "TR_BUNDESLAND", total_facet = "total", parameter = "mean",
                          sig_niveau = 0.05) {

  dat_wide <- prep_plot(eatRep_dat = eatRep_dat,
                        subgroup_var = subgroup_var,
                        parameter = parameter,
                        facet_var = facet_var,
                        total_facet = total_facet,
                        sig_niveau = sig_niveau,
                        total_subgroup = total_subgroup,
                        names_from_none = c("parameter_comp_none"),
                        names_from_comp = c("comparison_split", "parameter_comp"),
                        plot_type = "line"
                        )
dat_wide$year <- as.numeric(dat_wide$year)

  return(dat_wide)
}










create_trend <- function(df) {
  df$trend <- ifelse(length(unique(df$year)) == 2,
    paste(min(unique(df$year)), max(unique(df$year)), sep = "_"),
    ifelse(length(unique(df$year)) == 1,
      unique(df$year),
      stop("There are more than two years in the data.")
    )
  )
  return(df)
}

build_plot_dat <- function(eatRep_dat, facet_var, total_facet, total_subgroup) { ## facet_var schon außerhalb erzeugen

  eatRep_dat$group_estimates <- merge(eatRep_dat$group,
    eatRep_dat$estimate,
    by = "id"
  )
  eatRep_dat$comp_estimates <- merge(eatRep_dat$comparisons,
    eatRep_dat$estimate,
    by = "id"
  )

  eatRep_dat_long <- tidyr::pivot_longer(
    eatRep_dat$comp_estimates,
    cols = tidyr::starts_with("unit"),
    names_to = "unit",
    values_to = "group"
  )


  # nested comparisons ------------------------------------------------------
  eatRep_dat_unnested <- unnest_eatRep(eatRep_dat_long, eatRep_dat)

  eatRep_dat_merged <- merge(
    eatRep_dat_unnested,
    eatRep_dat$group_estimates,
    by.x = "unit",
    by.y = "id",
    suffixes = c("_comparison", "_comparison_none")
  )

  plot_dat <- do.call(rbind, lapply(split(eatRep_dat_merged, eatRep_dat_merged$id), create_trend))
  rownames(plot_dat) <- NULL



  ## Ansosnten hier irgendwo die Comparisons gruppieren, nach Unit = 2 Filtern und dann schauen ob die entsprechenden total label drin sind
  ## Everywhere where the comparison is against total, the total facet var can be removed, because it is duplicated. Same for subgroups.
  plot_dat <- plot_dat[!(plot_dat[, facet_var] == "total" & grepl("Total", plot_dat$comparison)), ]
  plot_dat <- plot_dat[!(plot_dat[, "subgroup_var"] == "total" & grepl("_subgroupTotal", plot_dat$comparison)), ]


  noTrend <- plot_dat[grep("_", plot_dat$trend, invert = TRUE), ]
  trend <- plot_dat[grep("_", plot_dat$trend), ]


  return(list(noTrend = noTrend, trend = trend))
}

prep_years_list <- function(years_lines, years_braces) {
  years_list <- lapply(list(years_lines, years_braces), prep_years)


  names(years_list) <- c("years_lines", "years_braces")

  return(years_list)
}



remove_comparisons_without_group <- function(eatRep_dat) {
  ## Schrittweise durch: erst schauen ob groups drin sind.
  ## Dann schauen ob die Comps noch drin sind.
  eatRep_dat$comparisons <- eatRep_dat$comparisons[grepl("group_", eatRep_dat$comparisons$unit_1) & eatRep_dat$comparisons$unit_1 %in% eatRep_dat$group$id & grepl("group_", eatRep_dat$comparisons$unit_2) & eatRep_dat$comparisons$unit_2 %in% eatRep_dat$group$id, ]
}
