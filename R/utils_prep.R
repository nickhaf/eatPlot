
prepare_comp <- function(dat, year_columns) {
  comp_trend <- data.frame()

  for (comp in c("crossDiff", "groupDiff")) { # unique(dat$comparison)) {

    if (!comp %in% c("crossDiff", "groupDiff")) {
      stop(paste0("The comparison '", comp, "' has not been implemented yet. Please contact the package author."))
    }

    ## Bei GroupDiff: Je nach Antwort von Sebastian ein BL oder eine wholeGroup vor Term hinterm Vs.

    dat_comp <- dat[!is.na(dat$comparison) & dat$comparison == comp, ]

    ## Compare against state: (change _within to _sameGroup)
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
