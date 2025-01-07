rename_comparisons_total <- function(eatRep_dat, total_group, facet_var) {

#  Aufpassen: Nur, wenn gegen "total" verglichen wird, also nicht beide Gruppen in der Comparison "total" sind.
  total_group_ids <- eatRep_dat$group[eatRep_dat$group[, facet_var] == total_group, "id"]
  total_comparisons <- eatRep_dat$comparison[
    (eatRep_dat$comparison$unit_1 %in% total_group_ids) != (eatRep_dat$comparison$unit_2 %in% total_group_ids),
    "id"
  ]


  total_comparisons_nested <- c(eatRep_dat$comparisons[eatRep_dat$comparisons$unit_1 %in% total_comparisons, "id"], eatRep_dat$comparisons[eatRep_dat$comparisons$unit_2 %in% total_comparisons, "id"])

  total_comparisons <- c(total_comparisons, total_comparisons_nested)

  while (length(total_comparisons_nested) > 0) {
    total_comparisons_nested <- c(eatRep_dat$comparisons[eatRep_dat$comparisons$unit_1 %in% total_comparisons_nested, "id"], eatRep_dat$comparisons[eatRep_dat$comparisons$unit_2 %in% total_comparisons_nested, "id"])

    total_comparisons <- c(total_comparisons, total_comparisons_nested)
  }

  eatRep_dat$comparisons[eatRep_dat$comparisons$id %in% total_comparisons, "comparison"] <- paste0(eatRep_dat$comparisons[eatRep_dat$comparisons$id %in% total_comparisons, "comparison"], "Total")
  eatRep_dat$plain[eatRep_dat$plain$id %in% total_comparisons, "comparison"] <- paste0(eatRep_dat$plain[eatRep_dat$plain$id %in% total_comparisons, "comparison"], "Total")

  return(eatRep_dat)
}


prepare_comp <- function(dat, year_columns) {
  comp_trend <- data.frame()

  for (comp in c("crossDiff", "groupDiff", "crossDiffofgroupDiff", "trendDiffgroup", "trendDiffcross")) { # unique(dat$comparison)) {

    if (!comp %in% c("crossDiff", "groupDiff", "crossDiffofgroupDiff", "trendDiffgroup", "trendDiffcross")) {
      stop(paste0("The comparison '", comp, "' has not been implemented yet. Please contact the package author."))
    }

    dat_comp <- dat[!is.na(dat$comparison) & dat$comparison == comp, ]

    comp_wide <- reshape_dat_comp_wide(dat_comp, comp, year_columns)

    comp_trend <- merge_2(
      comp_wide,
      comp_trend,
      by = c("depVar", "competence_var", "grouping_var", "state_var", year_columns),
      all = TRUE
    )
  }

  return(comp_trend)
}

reshape_dat_comp_wide <- function(dat_comp, comp, year_columns) {
  if (nrow(dat_comp) > 0) {
    if ("compare_1_Trend_Comp" %in% colnames(dat_comp)) {
      dat_comp <- rename_columns(dat_comp, "compare_1_Trend_Comp", "compare_1_Comp")
      dat_comp <- rename_columns(dat_comp, "compare_2_Trend_Comp", "compare_2_Comp")
    } else if ("compare_1_noTrend_Comp" %in% colnames(dat_comp)) {
      dat_comp <- rename_columns(dat_comp, "compare_1_noTrend_Comp", "compare_1_Comp")
      dat_comp <- rename_columns(dat_comp, "compare_2_noTrend_Comp", "compare_2_Comp")
    }

    ## Build an unique identifier for the column names of the comparisons

    dat_comp$compare_2_Comp <- paste0(comp, "_", dat_comp$compare_2_Comp)

    dat_comp <- remove_columns(dat_comp, c("comparison", "compare_1_Comp"))

    dat_comp_wide <- stats::reshape(dat_comp,
      direction = "wide",
      idvar = c("depVar", "competence_var", "grouping_var", "state_var", year_columns),
      timevar = c("compare_2_Comp"),
      sep = "_"
    )
    return(dat_comp_wide)
  } else {
    return(data.frame())
  }
}
