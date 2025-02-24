rename_comparisons_total <- function(eatRep_dat, facet_var, total_facet, total_subgroup = NULL) {
  # #  Aufpassen: Nur, wenn gegen "total" verglichen wird, also nicht beide Gruppen in der Comparison "total" sind.
  #   total_facet_ids <- eatRep_dat$group[eatRep_dat$group[, facet_var] == total_facet, "id"]
  #   total_facet_comparisons <- eatRep_dat$comparison[
  #     !(eatRep_dat$comparison$unit_1 %in% total_facet_ids) & (eatRep_dat$comparison$unit_2 %in% total_facet_ids),
  #     "id"
  #   ]

  # total_facet_comparisons_nested <- c(eatRep_dat$comparisons[eatRep_dat$comparisons$unit_1 %in% total_facet_comparisons, "id"], eatRep_dat$comparisons[eatRep_dat$comparisons$unit_2 %in% total_facet_comparisons, "id"])

  # total_comparisons <- c(total_facet_comparisons, total_comparisons_nested)
  total_facet_comparisons <- unique(eatRep_dat$plain$id[grep(paste0("- ", total_facet, "$"), eatRep_dat$plain[, facet_var])])
  total_subgroup_comparisons <- unique(eatRep_dat$plain$id[grep(paste0("- ", total_subgroup, "$"), eatRep_dat$plain[, "subgroup_var"])])

  ## If there is none, than every comparison is against the own group and against Total.
  ## There should be only one group for this to be correct.
  ## in this case duplicate and name one with totalgroup and one with sameGroup:
#   if(length(total_subgroup_comparisons) == 0 & length(unique(eatRep_dat$group$subgroup_var)) == 1){
# total_subgroup_comparisons <- total_facet_comparisons
#     }

  # while (length(total_comparisons_nested) > 0) {
  #   total_comparisons_nested <- c(eatRep_dat$comparisons[eatRep_dat$comparisons$unit_1 %in% total_comparisons_nested, "id"], eatRep_dat$comparisons[eatRep_dat$comparisons$unit_2 %in% total_comparisons_nested, "id"])
  #
  #   total_comparisons <- c(total_comparisons, total_comparisons_nested)
  # }

  eatRep_dat$comparisons[eatRep_dat$comparisons$id %in% total_facet_comparisons, "comparison"] <- paste0(eatRep_dat$comparisons[eatRep_dat$comparisons$id %in% total_facet_comparisons, "comparison"], "Total")
  eatRep_dat$plain[eatRep_dat$plain$id %in% total_facet_comparisons, "comparison"] <- paste0(eatRep_dat$plain[eatRep_dat$plain$id %in% total_facet_comparisons, "comparison"], "Total")

    eatRep_dat$comparisons[eatRep_dat$comparisons$id %in% total_subgroup_comparisons, "comparison"] <- paste0(eatRep_dat$comparisons[eatRep_dat$comparisons$id %in% total_subgroup_comparisons, "comparison"], "_subgroupTotal")
    eatRep_dat$plain[eatRep_dat$plain$id %in% total_subgroup_comparisons, "comparison"] <- paste0(eatRep_dat$plain[eatRep_dat$plain$id %in% total_subgroup_comparisons, "comparison"], "_subgroupTotal")

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
