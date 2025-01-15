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
                          sig_niveau = 0.05, comparisons = c("none", "crossDiff", "trend", "trend_crossDiff")) {
  # Check input -------------------------------------------------------------
  check_eatRep_dat(eatRep_dat)
  check_columns(eatRep_dat$estimate, cols = c("p"))

  unsupported_comp <- comparisons[!(comparisons %in% c("none", "crossDiff", "trend", "trend_crossDiff"))]
  if (length(unsupported_comp) > 0) {
    stop(paste("Comparison '", unsupported_comp, "' is not supported. Please contact the package author."))
  }

  if (is.null(subgroup_var)) {
    message("Are you sure you don't have a subgroup_var? If you do,  please set it.")
  }

  eatRep_dat$group <- build_column(eatRep_dat$group, old = subgroup_var, new = "subgroup_var", fill_value = "total") ## set to total so the background_line can be plotted
  eatRep_dat$plain <- build_column(eatRep_dat$plain, old = subgroup_var, new = "subgroup_var", fill_value = "total") ## set to total so the background_line can be plotted



  # Filtering ---------------------------------------------------------------
  if (!is.null(comparisons)) {
    eatRep_dat$comparisons <- eatRep_dat$comparisons[eatRep_dat$comparisons$comparison %in% c(comparisons, paste0(comparisons, "Total")), ]
  }
  eatRep_dat <- rename_comparisons_total(eatRep_dat,
    facet_var = facet_var,
    total_facet = total_facet,
    total_subgroup = total_subgroup
  )
  eatRep_dat$plain <- NULL
  eatRep_dat$estimate <- eatRep_dat$estimate[eatRep_dat$estimate$parameter == parameter, ]

  eatRep_dat$group$year <- as.numeric(eatRep_dat$group$year)

  # Merge Data --------------------------------------------------------------
  check_no_columns(eatRep_dat$estimate, cols = "sig")
  eatRep_dat$estimate$sig <- ifelse(eatRep_dat$estimate$p < sig_niveau, TRUE, FALSE)

  plot_dat_wide <- build_plot_dat(eatRep_dat, facet_var, total_facet, total_subgroup)



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

  noTrend_wide <- tidyr::pivot_wider(
    unique(noTrend[, !colnames(noTrend) %in% c("group", "id")]),
    names_from = "comparison",
    values_from = c("est_comparison", "se_comparison", "sig_comparison", "p_comparison", "es_comparison")
  ) |>
    as.data.frame()

  trend_wide <- tidyr::pivot_wider(
    unique(trend[, !colnames(trend) %in% c("group", "id")]),
    names_from = "comparison",
    values_from = c("est_comparison", "se_comparison", "sig_comparison", "p_comparison", "es_comparison")
  ) |>
    as.data.frame()

  plot_dat <- merge(
    noTrend_wide[, !colnames(noTrend_wide) %in% c("group", "id", "trend")],
    trend_wide,
    all = TRUE
  )


  return(plot_dat)
}

prep_years_list <- function(years_lines, years_braces) {
  years_list <- lapply(list(years_lines, years_braces), prep_years)


  names(years_list) <- c("years_lines", "years_braces")

  return(years_list)
}


unnest_eatRep <- function(comparison_dat, eatRep_dat) {
  eatRep_unnested <- data.frame()
  ## comparison_dat now contains all comparisons that are made in the data.
  ## The group column contains the groups that belong to that comparison. Can either other comparisons, or groups.
  ## So,as long as there is a comparison in the group column, some nested comparison is in there:
  comp_groups <- comparison_dat$group[grep("comp_", comparison_dat$group)]
  comparison_dat$unit <- comparison_dat$group

  column_order <- c("id", "group", "unit")
  comparison_dat <- comparison_dat[, c(column_order, colnames(comparison_dat)[(!(colnames(comparison_dat) %in% column_order))])]
  ## The others are already unnested and can be saved:
  eatRep_no_comp_group <- comparison_dat[!(comparison_dat$group %in% comp_groups), ]
  eatRep_unnested <- rbind(eatRep_unnested, eatRep_no_comp_group)

  while (length(comp_groups > 0)) {
    eatRep_nested <- comparison_dat[comparison_dat$unit %in% comp_groups, ]
    eatRep_nested_m <- merge(eatRep_nested,
      eatRep_dat$comp_estimates[, c("id", "unit_1", "unit_2")],
      by.x = "unit",
      by.y = "id",
      all.x = TRUE
    )
    # Hier vom Format noch richtig! Jetzt lang machen.

    eatRep_nested_m$unit_merged <- paste(eatRep_nested_m$unit_1, eatRep_nested_m$unit_2, sep = "_")

    # Build new column --------------------------------------------------
    eatRep_nested_m$unit <- NULL
    eatRep_nested_m$unit_b <- NULL
    eatRep_nested_m <- tidyr::pivot_longer(
      eatRep_nested_m,
      cols = c("unit_1", "unit_2"),
      names_to = "unit_b",
      values_to = "unit"
    )

    ## Now do the stuff we did before the loop started again.
    ## By this, the comparisons should be unnested until all done!

    comp_groups <- eatRep_nested_m$unit[grep("comp_", eatRep_nested_m$unit)]
    eatRep_no_comp_group <- eatRep_nested_m[!(eatRep_nested_m$unit %in% comp_groups), ]
    comparison_dat <- eatRep_nested_m[eatRep_nested_m$unit %in% comp_groups, ]

    eatRep_unnested <- rbind(
      eatRep_unnested[, c("id", "group", "unit", "comparison", "est", "se", "p", "sig", "es")],
      eatRep_no_comp_group[, c("id", "group", "unit", "comparison", "est", "se", "p", "sig", "es")]
    )
  }

  return(eatRep_unnested)
}

remove_comparisons_without_group <- function(eatRep_dat) {
  ## Schrittweise durch: erst schauen ob groups drin sind.
  ## Dann schauen ob die Comps noch drin sind.
  eatRep_dat$comparisons <- eatRep_dat$comparisons[grepl("group_", eatRep_dat$comparisons$unit_1) & eatRep_dat$comparisons$unit_1 %in% eatRep_dat$group$id & grepl("group_", eatRep_dat$comparisons$unit_2) & eatRep_dat$comparisons$unit_2 %in% eatRep_dat$group$id, ]
}
