#' Prepare lineplot data.
#'
#' @param eatRep_dat Object returned by `eatRep`.
#' @param parameter Character string. Contains the value in column `parameter` that is used for plotting the lines.  Defaults to `"mean"`.
#' @param facet_var Character string of the name of the variable that should be used for faceting the plot. Defaults to `"TR_BUNDESLAND"`.
#' @param total_group Character string of the name of the total groups containing all other groups of the facet var. Defaults to `"total"`.
#' @param sig_niveau Numeric indicating the border below which p-values will be considered significant. Defaults to `0.05`.
#' @param comparisons Character string that can be used to filter the needed comparions. This can drastically reduce the output of the prepared data. Defaults to `NULL`.
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
  if (!is.null(comparisons)) {
    eatRep_dat$comparison <- eatRep_dat$comparison[eatRep_dat$comparison$comparison %in% c(comparisons, paste0(comparisons, "Total")), ]
  }

  # Merge Data --------------------------------------------------------------
  check_no_columns(eatRep_dat$estimate, cols = "sig")
  eatRep_dat$estimate$sig <- ifelse(eatRep_dat$estimate$p < sig_niveau, TRUE, FALSE)
  plot_dat <- build_plot_dat(eatRep_dat)

  plot_dat_wide <- tidyr::pivot_wider(
    unique(plot_dat),
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
    cols = tidyr::starts_with("unit"),
    names_to = "unit",
    values_to = "group"
  )

  # nested comparisons ------------------------------------------------------

  eatRep_dat_unnested <- unnest_eatRep(eatRep_dat_long, eatRep_dat)

  eatRep_dat_merged <- merge(
    eatRep_dat_unnested,
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


unnest_eatRep <- function(comparison_dat, eatRep_dat){

  eatRep_unnested <- data.frame()
  ## comparison_dat now contains all comparisons that are made in the data.
  ## The group column contains the groups that belong to that comparison. Can either other comparisons, or groups.
  ## So,as long as there is a comparison in the group column, some nested comparison is in there:
  comp_groups <- comparison_dat$group[grep("comp_", comparison_dat$group)]

  ## The others are already unnested and can be saved:
  eatRep_no_comp_group <- comparison_dat[!(comparison_dat$group %in% comp_groups), ]
  eatRep_unnested <- rbind(eatRep_unnested, eatRep_no_comp_group)



  while(length(comp_groups > 0)){

    eatRep_nested <- comparison_dat[comparison_dat$group %in% comp_groups, ]

    eatRep_nested_m <- merge(eatRep_nested,
                           eatRep_dat$comp_estimates[, c("id", "unit_1", "unit_2")],
                           by.x = "group",
                           by.y = "id",
                           all.x = TRUE)

    eatRep_nested_m$id <- paste(eatRep_nested_m$id, eatRep_nested_m$group, sep = "_")

# Build new group-column --------------------------------------------------
    eatRep_nested_m$group <- NULL
    eatRep_nested_m <- tidyr::pivot_longer(
      eatRep_nested_m,
      cols = c("unit_1", "unit_2"),
      names_to = "unit_b",
      values_to = "group"
    )

  ## Now do the stuff we did before the loop started again.
  ## By this, the comparisons should be unnested until all done!

    comp_groups <- eatRep_nested_m$group[grep("comp_", eatRep_nested_m$group)]
    eatRep_no_comp_group <- eatRep_nested_m[!(eatRep_nested_m$group %in% comp_groups), ]

    eatRep_unnested <- rbind(
      eatRep_unnested[, c("id", "comparison", "group", "est", "se", "p", "sig", "es")],
      eatRep_no_comp_group[, c("id", "comparison", "group", "est", "se", "p", "sig", "es")])
  }

return(eatRep_unnested)

}
