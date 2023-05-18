prepare_noTrend <- function(list_building_blocks, merging_columns) {
  comp_noTrend <- prepare_comp_noTrend(list_building_blocks$noTrend_Comp)

  comp_wholeGroup_noTrend <- list_building_blocks[["noTrend_Comp"]][list_building_blocks[["noTrend_Comp"]]$compare_2 == "wholeGroup", ]
  comp_wholeGroup_noTrend <- add_suffix(comp_wholeGroup_noTrend, merging_columns = merging_columns, suffix = "CrossDiffWhole")
  comp_state_noTrend <- list_building_blocks[["noTrend_Comp"]][list_building_blocks[["noTrend_Comp"]]$compare_2 == "BL" | list_building_blocks[["noTrend_Comp"]]$compare_1 == "_groupingVar", ]
  comp_state_groups <- list_building_blocks[["noTrend_Comp"]][grepl("__groupingVar", list_building_blocks[["noTrend_Comp"]]$compare_1) & list_building_blocks[["noTrend_Comp"]]$compare_2 == "_groupingVar", ]

  comp_state_noTrend <- add_suffix(comp_state_noTrend, merging_columns = merging_columns, suffix = "CrossDiffWithin")
  comp_state_groups <- add_suffix(comp_state_groups, merging_columns = merging_columns, suffix = "GroupDiff")

  noTrend_merge_cols <- c(
    "grouping_var",
    "state_var",
    "year",
    "competence_var",
    "depVar"
  )

  ## In for-Loop?:

  comp_within_whole_noTrend <- merge_2(
    comp_state_noTrend,
    comp_wholeGroup_noTrend,
    all.x = TRUE,
    by = noTrend_merge_cols
  )

  ## return besser überlegen: Jeweils den vollen?
  comp_within_whole_noTrend <- merge_2(
    comp_within_whole_noTrend,
    comp_state_groups,
    all = TRUE,
    by = noTrend_merge_cols
  )

  noTrend_data_merged <- merge_2(
    comp_within_whole_noTrend,
    list_building_blocks[["noTrend_noComp"]],
    by = noTrend_merge_cols,
    all = TRUE
  )

  return(list(
    "noTrend_merged" = noTrend_data_merged,
    "comp_within_whole_noTrend" = comp_within_whole_noTrend
  ))
}

prepare_comp_noTrend <- function(dat) {
  comp_trend <- data.frame()

  for (comp in c("crossDiff", "groupDiff")) { # unique(dat$comparison)) {

    if (!comp %in% c("crossDiff", "groupDiff")) {
      stop(paste0("The comparison '", comp, "' has not been implemented yet. Please contact the package author."))
    }

    ## Bei GroupDiff: Je nach Antwort von Sebastian ein BL oder eine wholeGroup vor Term hinterm Vs.

    dat_comp <- dat[!is.na(dat$comparison) & dat$comparison == comp, ]
    dat_comp$grouping_var <- gsub("\\.vs\\..*", "", dat_comp$grouping_var)

    dat_comp_state <- dat_comp[dat_comp$grouping_var != "noGroup" & dat_comp$state_var != "wholeGroup" & dat_comp$compare_2_Comp != "wholeGroup" & !is.na(dat_comp$compare_2_Comp), ]
    dat_comp_wide_state <- reshape_dat_comp_wide(dat_comp_state, comp)


    browser()
    dat_comp_whole <- dat_comp[grepl("wholeGroup", dat_comp$compare_2), ]
    dat_comp_wide_whole <- reshape_dat_comp_wide(dat_comp_whole, comp)

    if (nrow(dat_comp_wide_state) != 288) {
      warning(comp)
    }

    comp_trend <- merge_2(comp_trend,
      dat_comp_wide_state,
      by = c("depVar", "competence_var", "grouping_var", "state_var", "year"),
      all.x = TRUE
    )
  }

  return(comp_trend)
}

reshape_dat_comp_wide <- function(dat_comp, comp) {
  if (nrow(dat_comp) > 0) {
    dat_comp$compare_2_Comp <- paste0(comp, "_", dat_comp$compare_2_Comp)
    dat_comp <- remove_columns(dat_comp, c("comparison", "compare_1_Comp"))

    dat_comp_wide <- reshape(dat_comp,
      direction = "wide",
      idvar = c("depVar", "competence_var", "grouping_var", "state_var", "year"),
      timevar = c("compare_2_Comp"),
      sep = "_"
    )
    return(dat_comp_wide)
  } else {
    return(data.frame())
  }
}
