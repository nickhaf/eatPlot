#' Prepare trend data for plotting
#'
#' Performs different data transformations, to bring the input data.frame into the correct formats for different kind of plots.
#'
#' @param dat Input data.frame stemming from `eatRep`.
#' @param competence Character string containing the competence that should be plotted.
#' @param grouping_var Character string containing the column name in `dat` that should be used to distinguish between subgroups.
#' @param states Character vector of the states that should be plotted.
#' @param state_var Character string containing the column name in `dat` that should be used to distinguish between groups that should be plotted seperatly. Normally, this should be the states ("Bundesländer"). Therfore, defaults to `"TR_BUNDESLAND"`.
#' @param group_var Character string containing the column name in `dat` that contains the different group memberships in one string. Defaults to `"group"`.
#' @param competence_var Character string containing the column name in `dat` that contains the different competences. Defaults to `"kb"`.
#' @param sig_niveau Numeric indicating the border, below which p-values will be considered significant. Defaults to `0.05`.
#' @param plot_mean Logical value indicating whether the mean of the subgroups should be plotted as well.
#' @param parameter Character string of the parameter value that should be reported. Defaults to `"mean"`.
#'
#' @returns `prep_plot()` returns a list containing four data.frames prepared for plotting with different [eatPlot] functions. This includes the data.frames:
#' * `plot_points` for plotting with [plot_points()]
#' * `plot_lines` for plotting with [plot_lines()]
#' * `plot_braces` for plotting with [plot_braces()]
#' * `plot_background_lines` for plotting with [plot_background_lines()].
#' @export
#'
#' @examples # tbd
prep_plot <- function(dat,
                       competence_var = "kb",
                       competence = NULL,
                       states = NULL,
                       state_var = "TR_BUNDESLAND",
                       group_var = "group",
                       grouping_var = NULL,
                       sig_niveau = 0.05,
                       plot_mean = FALSE,
                       parameter = "mean") {


# Checks ------------------------------------------------------------------
## Hier alle Spalten bilden, die später gebraucht werden.

  dat <- as.data.frame(dat)

## Check arguments
stopifnot(is.data.frame(dat))
stopifnot(is.character(competence) | is.null(competence))
stopifnot(is.character(grouping_var) | is.null(grouping_var))
stopifnot(is.character(states) | is.null(states))
stopifnot(is.character(state_var))
stopifnot(is.character(competence_var))
stopifnot(is.character(group_var))
stopifnot(is.numeric(sig_niveau))
stopifnot(is.logical(plot_mean))
stopifnot(is.character(parameter))

sapply(c(grouping_var, state_var, competence_var, group_var, "comparison"), check_column, dat = dat)

# Show a message, if a grouping_var was provided, but not as factor.
dat <- check_factor(dat, grouping_var, "grouping_var")

dat <- fill_column(dat, competence_var)
dat <- fill_column(dat, grouping_var, filling = )
dat <- fill_column(dat, state_var)
dat <- fill_column(dat, group_var)

## remove the old columns, but only after all columns have been build, in case one old column is needed 2x.
dat <- dat[, -which(colnames(dat) %in% c(competence_var, grouping_var, state_var, group_var))]

colnames(dat) <- gsub("\\.", "_", colnames(dat))
colnames(dat) <- gsub("sig_", "p_", colnames(dat))
colnames(dat) <- gsub("^sig$", "p", colnames(dat))



  all_states <- unique(dat$state_var)[!is.na(unique(dat$state_var))]
  if(!is.null(grouping_var)){
    sub_groups <- unique(dat$grouping_var)[!is.na(unique(dat$grouping_var))]
  }else{
    sub_groups <- NULL
  }

  merging_columns <- c("state_var",
                       "grouping_var",
                       "year_start",
                       "year_end",
                       "depVar",
                       "competence_var",
                       "years_Trend",
                       "year")

  dat <- clean_data(
    dat = dat,
    states = states,
    all_states = all_states,
    sub_groups = sub_groups,
    competence = competence,
    parameter = parameter
  )

  if (any(!is.na(dat$comparison))) {
    dat <- get_comparisons(dat,
      states = all_states[all_states != "wholeGroup"],
      sub_groups = sub_groups
    )
  }


  list_building_blocks <- prep_data_blocks(
    data_clean = dat,
    sig_niveau = sig_niveau,
    all_states,
    sub_groups,
    merging_columns = merging_columns
  )



# Build noTrend dataframe -------------------------------------------------
  comp_wholeGroup_noTrend <- list_building_blocks[["noTrend_Comp"]][list_building_blocks[["noTrend_Comp"]]$compare_2 == "wholeGroup", ]
  comp_wholeGroup_noTrend <- add_suffix(comp_wholeGroup_noTrend, merging_columns = merging_columns, suffix = "Whole")
  comp_state_noTrend <- list_building_blocks[["noTrend_Comp"]][list_building_blocks[["noTrend_Comp"]]$compare_2 == "BL" | list_building_blocks[["noTrend_Comp"]]$compare_1 == "_groupingVar", ]
  comp_state_noTrend <- add_suffix(comp_state_noTrend, merging_columns = merging_columns, suffix = "Within")


  if (nrow(comp_state_noTrend) != 0) {
    comp_within_whole_noTrend <- merge(
    comp_state_noTrend,
     comp_wholeGroup_noTrend,
      all.x = TRUE,
    by = c("grouping_var",
           "state_var",
           "year",
           "competence_var",
           "depVar")
    )
  } else {
    comp_within_whole_noTrend <- comp_wholeGroup_noTrend
  }


  if (nrow(comp_within_whole_noTrend) != 0) {
    noTrend_data_merged <- merge(
      comp_within_whole_noTrend,
      list_building_blocks[["noTrend_noComp"]],
      by = c("grouping_var",
             "state_var",
             "year",
             "competence_var",
             "depVar"),
      all = TRUE
    )
  } else {
    noTrend_data_merged <- list_building_blocks[["noTrend_noComp"]]
  }

  # Prepare the trend-data.frame --------------------------------------------
  # Data with comparison, either comparing with the whole group, or within the state

  comp_wholeGroup <- list_building_blocks[["Trend_Comp"]][list_building_blocks[["Trend_Comp"]]$compare_2 == "wholeGroup", ]
comp_wholeGroup <- add_suffix(comp_wholeGroup, merging_columns = merging_columns, suffix = "Whole")
   comp_state <- list_building_blocks[["Trend_Comp"]][list_building_blocks[["Trend_Comp"]]$compare_2 == "BL" | list_building_blocks[["Trend_Comp"]]$compare_1 == "_groupingVar", ]
   comp_state <- add_suffix(comp_state, merging_columns = merging_columns, suffix = "Within")


  if (nrow(comp_state) != 0) {
    comp_within_whole <- merge_trend_data(
      trend_data_1 = comp_state,
      trend_data_2 = comp_wholeGroup,
      suffixes = c("", ""),
      all.x = TRUE
    )
  } else {
    comp_within_whole <- comp_wholeGroup
  }

  ## Add data without comparison:

  if (nrow(comp_within_whole) != 0) {
    trend_data_merged <- merge_trend_data(
      trend_data_1 = comp_within_whole,
      trend_data_2 = list_building_blocks[["Trend_noComp"]],
      suffixes = c("", ""),
      all = TRUE
    )
  } else {
    trend_data_merged <- list_building_blocks[["Trend_noComp"]]
  }



# Merge to final data frame -----------------------------------------------
  trend_data_final <- merge_trend_point(
    trend_data = trend_data_merged,
    point_data = noTrend_data_merged
  )
  ## Drop unused levels
  if(any(!is.na(trend_data_final$grouping_var))){
    trend_data_final$grouping_var <- droplevels(trend_data_final$grouping_var)
}
  # Prepare the wholeGroup data.frame ---------------------------------------
     trend_data_wholeGroup <- merge_trend_point(
    list_building_blocks[["Trend_noComp_wholeGroup"]],
    list_building_blocks[["noTrend_noComp_wholeGroup"]]
  )

  # Fill up NAs -------------------------------------------------------------
  ## Fill up NA significances with FALSE (those which emerged through merging)
  for (i in grep("sig_", colnames(trend_data_final))) {
    trend_data_final[, i] <- ifelse(is.na(trend_data_final[, i]), FALSE, trend_data_final[, i])
  }

  # Build plotlist ----------------------------------------------------------
  plot_dat <- list()

  ################
  ## plot_lines ##
  ################
  plot_dat[["plot_lines"]] <- trend_data_final

  if (!is.null(grouping_var) & plot_mean == FALSE) { ## Should the mean group be plotted as well (not only the subgroups)?
    plot_dat[["plot_lines"]] <- plot_dat[["plot_lines"]][plot_dat[["plot_lines"]]$grouping_var != "noGroup", ]
  }
  #################
  ## plot_braces ##
  #################
  plot_dat[["plot_braces"]] <- trend_data_final

  if (!is.null(grouping_var) & plot_mean == FALSE) { ## Should the mean group be plotted as well (not only the subgroups)?
    plot_dat[["plot_braces"]] <- plot_dat[["plot_braces"]][plot_dat[["plot_braces"]]$grouping_var != "noGroup", ]
  }

  ###########################
  ## plot_background_lines ##
  ###########################
  plot_dat[["plot_background_lines"]] <- trend_data_wholeGroup

  ##############
  ## plot_bar ##
  ##############

  plot_dat[["plot_tablebar"]] <- plot_dat[["plot_lines"]]

  #################
  ## plot_points ##
  #################
  ## for the split lineplot, the middle points have to be plotted two times. Therefore, the plot_points function is build using the comparisons already calculated.

  dat_long <- stats::reshape(plot_dat[["plot_lines"]][,
                                                      c("depVar",
                                                        "grouping_var",
                                                        "year_start",
                                                        "year_end",
                                                        "years_Trend",
                                                        "state_var",
                                                        "competence_var")],
    direction = "long",
    varying = c("year_start", "year_end"),
    sep = "_"
  )

  dat_long <- remove_columns(dat_long, c("time", "id"))
  plot_dat[["plot_points"]] <- merge(dat_long,
                                     list_building_blocks[["noTrend_noComp"]],
                                     by = c("grouping_var",
                                            "state_var",
                                            "year",
                                            "competence_var",
                                            "depVar"),
                                     all.x = TRUE)
  plot_dat[["plot_points"]] <- plot_dat[["plot_points"]][plot_dat[["plot_points"]]$grouping_var != "noGroup", ]


  ## Order columns

  plot_dat <- lapply(plot_dat, function(x){
    x <- x[,grep("\\.x$|\\.y$", colnames(x), invert = TRUE)]
    x[,order(colnames(x))]
  })

  return(plot_dat)
}


