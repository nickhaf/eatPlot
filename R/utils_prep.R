prepare_comp <- function(dat, year_columns) {
  comp_trend <- data.frame()

  for (comp in c("crossDiff", "groupDiff", "crossDiffofgroupDiff", "trendDiffgroup", "trendDiffcross")) { # unique(dat$comparison)) {

    if (!comp %in% c("crossDiff", "groupDiff", "crossDiffofgroupDiff", "trendDiffgroup", "trendDiffcross")) {
      stop(paste0("The comparison '", comp, "' has not been implemented yet. Please contact the package author."))
    }

    dat_comp <- dat[!is.na(dat$comparison) & dat$comparison == comp, ]
    comp_wide <- reshape_dat_comp_wide(dat_comp, comp, year_columns)

    ## This comparison type is grouping_var independent, and has only one line per country:
    if(comp == "crossDiffofgroupDiff"){
      comp_trend <- merge_2(
        comp_wide[, !colnames(comp_wide) == "grouping_var"],
        comp_trend,
        by = c("depVar", "competence_var", "state_var", year_columns),
        all.x = FALSE,
        all.y = TRUE
      )
    }else{
    comp_trend <- merge_2(
      comp_wide,
      comp_trend,
      by = c("depVar", "competence_var", "grouping_var", "state_var", year_columns),
      all = TRUE
    )
    }
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
    if (any(grepl("\\.vs\\.", dat_comp$compare_1_Comp))) {
      dat_comp$compare_2_Comp <- paste0(comp, "_", dat_comp$compare_1_Comp, ".VS.", dat_comp$compare_2_Comp)
    } else {
      dat_comp$compare_2_Comp <- paste0(comp, "_", dat_comp$compare_2_Comp) ## müsste wholeGroup drin stehen
    }

    # Renaming necessary, so the 0.vs.1 group appears in the column headers
if(all(dat_comp$comparison == "crossDiffofgroupDiff")){
  dat_comp$compare_2_Comp <- paste0("crossDiffofgroupDiff_", gsub("_", "", dat_comp$compare_1_Comp),  "_vs", gsub("crossDiffofgroupDiff", "", dat_comp$compare_2_Comp))
}
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
