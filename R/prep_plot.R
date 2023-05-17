#' Prepare trend data for plotting
#'
#' Performs different data transformations, to bring the input data.frame into the correct formats for different kind of plots.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param dat Input data.frame stemming from `eatRep`.
#' @param competence Character string containing the competence that should be plotted.
#' @param grouping_var Character string containing the column name in `dat` that should be used to distinguish between subgroups.
#' @param states Character vector of the states that should be plotted.
#' @param state_var Character string containing the column name in `dat` that should be used to distinguish between groups that should be plotted seperatly. Normally, this should be the states ("Bundesländer"). Therfore, defaults to `"TR_BUNDESLAND"`.
#' @param group_var Character string containing the column name in `dat` that contains the different group memberships in one string. Defaults to `"group"`.
#' @param grouping_var_groups Character vector containing the groups from `grouping_var` you want to plot. Defaults to `NULL`, in which case all groups are prepared.
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
                      grouping_var_groups = NULL,
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
  stopifnot(is.character(grouping_var_groups) | is.null(grouping_var_groups))
  stopifnot(is.character(states) | is.null(states))
  stopifnot(is.character(state_var))
  stopifnot(is.character(competence_var))
  stopifnot(is.character(group_var))
  stopifnot(is.numeric(sig_niveau))
  stopifnot(is.logical(plot_mean))
  stopifnot(is.character(parameter))

  if(is.null(grouping_var)){
    message("Are you sure your data isn't grouped? If it is, but you didn't provide a grouping_var, this might lead to duplicated rows in the prepared data.frames.")
  }

  if(any(dat[!is.na(dat[, state_var]), state_var] == "")){
    warning(paste0("Your state_var column '", state_var, "' includes missing Values that are not coded as NA. Please recode to NA."), call. = FALSE)
  }

  sapply(c(grouping_var, state_var, competence_var, group_var, "comparison"), check_column, dat = dat)

  # Show a message, if a grouping_var was provided, but not as factor.
  dat <- check_factor(dat, grouping_var, "grouping_var")

  dat <- fill_column(dat, competence_var)
  dat <- fill_column(dat, grouping_var)
  dat <- fill_column(dat, state_var)
  dat <- fill_column(dat, group_var)

  ## remove the old columns, but only after all columns have been build, in case one old column is needed 2x.
  dat <- dat[, -which(colnames(dat) %in% c(competence_var, grouping_var, state_var, group_var))]


  if(!is.null(grouping_var_groups)){

    if(any(!grouping_var_groups %in% dat$grouping_var)){
      stop(paste0("One or more of your grouping_var_groups are not found in your grouping_var column '", grouping_var, "'."))
    }
    dat <- dat[is.na(dat$grouping_var) | dat$grouping_var %in% grouping_var_groups, ]
  }

  colnames(dat) <- gsub("\\.", "_", colnames(dat))
  colnames(dat) <- gsub("sig_", "p_", colnames(dat))
  colnames(dat) <- gsub("^sig$", "p", colnames(dat))

  if (!is.null(competence)) {
    if (!competence %in% dat$competence_var) {
      stop("Your competence is not in your competence_var column.")
    }
  }
  all_states <- unique(dat$state_var)[!is.na(unique(dat$state_var))]
  if (!is.null(grouping_var)) {
    sub_groups <- unique(dat$grouping_var)[!is.na(unique(dat$grouping_var))]
  } else {
    sub_groups <- NULL
  }

  merging_columns <- c(
    "state_var",
    "grouping_var",
    "year_start",
    "year_end",
    "depVar",
    "competence_var",
    "years_Trend",
    "year"
  )

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
  noTrend_merged <- prepare_noTrend(list_building_blocks, merging_columns)
  noTrend_data_merged <- noTrend_merged$noTrend_merged


  # Prepare the trend-data.frame --------------------------------------------
  if (any(grepl("trend", colnames(dat)))) {

    comp_wholeGroup <- list_building_blocks[["Trend_Comp"]][list_building_blocks[["Trend_Comp"]]$compare_2 == "wholeGroup", ]
    comp_wholeGroup <- add_suffix(comp_wholeGroup, merging_columns = merging_columns, suffix = "CrossDiffWhole")
    comp_state <- list_building_blocks[["Trend_Comp"]][list_building_blocks[["Trend_Comp"]]$compare_2 == "BL" | list_building_blocks[["Trend_Comp"]]$compare_1 == "_groupingVar", ]
    comp_state <- add_suffix(comp_state, merging_columns = merging_columns, suffix = "CrossDiffWithin")
    comp_groups <- list_building_blocks[["Trend_Comp"]][grepl("__groupingVar", list_building_blocks[["Trend_Comp"]]$compare_1) & list_building_blocks[["Trend_Comp"]]$compare_2 == "_groupingVar", ]

    comp_groups <- add_suffix(comp_groups, merging_columns = merging_columns, suffix = "GroupDiff")

      comp_within_whole <- merge_trend_data(
        trend_data_1 = comp_state,
        trend_data_2 = comp_wholeGroup,
        suffixes = c("", ""),
        all.x = TRUE
      )

      comp_within_whole <- merge_trend_data(
        trend_data_1 = comp_within_whole,
        trend_data_2 = comp_groups,
        suffixes = c("", ""),
        all = TRUE
      )


    ## Add data without comparison:
      Trend_data_merged <- merge_trend_data(
        trend_data_1 = comp_within_whole,
        trend_data_2 = list_building_blocks[["Trend_noComp"]],
        suffixes = c("", ""),
        all = TRUE
      )



    # Merge to final data frame -----------------------------------------------
    trend_data_final <- merge_trend_point(
      trend_data = Trend_data_merged,
      point_data = noTrend_data_merged
    )

    ## Drop unused levels
    if (any(!is.na(trend_data_final$grouping_var)) & nrow(list_building_blocks[["Trend_noComp_wholeGroup"]]) != 0 & nrow(list_building_blocks[["noTrend_noComp_wholeGroup"]]) ) {
      trend_data_final$grouping_var <- droplevels(trend_data_final$grouping_var)
    }
    # Prepare the wholeGroup data.frame ---------------------------------------
    trend_data_wholeGroup <- merge_trend_point(
      list_building_blocks[["Trend_noComp_wholeGroup"]],
      list_building_blocks[["noTrend_noComp_wholeGroup"]]
    )
  } else { ## If no trend columns in data.
    Trend_data_merged <- data.frame()
    trend_data_final <- noTrend_data_merged
    trend_data_wholeGroup <- list_building_blocks[["noTrend_noComp_wholeGroup"]]
  }

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
  plot_dat[["plot_lines"]] <- plot_dat[["plot_lines"]][plot_dat[["plot_lines"]]$grouping_var != "wholeGroup", ]
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
  if(nrow(noTrend_data_merged) != 0){
    # earlier merging might lead to NAs in the comparison columns. As they might be needed as ID for reshaping, NAs are substituted
    compare_cols <-  colnames(noTrend_data_merged)[grep("compare_", colnames(noTrend_data_merged))]
    for(i in compare_cols){
    noTrend_data_merged[is.na(noTrend_data_merged[, i]), i] <- "no_comp"
    }

  id_vars <- c("grouping_var", "state_var", "competence_var", "depVar", colnames(noTrend_data_merged)[grep("compare_", colnames(noTrend_data_merged))])


  noTrend_data_merged_wide <- stats::reshape(noTrend_data_merged,
                                      direction = "wide",
                                      timevar = "year",
                                      idvar = id_vars,
                                      sep = "_"
                                      )


  }else{
    noTrend_data_merged_wide <- data.frame()
  }

  if(nrow(Trend_data_merged) != 0){
    # earlier merging might lead to NAs in the comparison columns. As they might be needed as ID for reshaping, NAs are substituted
    compare_cols <-  colnames(Trend_data_merged)[grep("compare_", colnames(Trend_data_merged))]
    for(i in compare_cols){
      Trend_data_merged[is.na(Trend_data_merged[, i]), i] <- "no_comp"
    }

    id_vars <- c("grouping_var", "state_var", "competence_var", "depVar", colnames(Trend_data_merged)[grep("compare_", colnames(Trend_data_merged))])
    Trend_data_merged_wide <- stats::reshape(Trend_data_merged,
                                             direction = "wide",
                                             timevar = "years_Trend",
                                             idvar = id_vars,
                                             sep = "_"
    )


  }else{
    Trend_data_merged_wide <- data.frame()
  }

  plot_dat[["plot_tablebar"]] <- merge_2(noTrend_data_merged_wide,
                                       Trend_data_merged_wide,
                                       by = c("grouping_var", "state_var", "competence_var", "depVar"),
                                       all = TRUE
                                       )


  #################
  ## plot_points ##
  #################
  ## for the split lineplot, the middle points have to be plotted two times. Therefore, the plot_points function is build using the comparisons already calculated.

  if (any(grepl("trend", colnames(dat)))) {
    plot_dat[["plot_points"]] <- stats::reshape(
      plot_dat[["plot_lines"]][
        ,
        c(
          "depVar",
          "grouping_var",
          "year_start",
          "year_end",
          "years_Trend",
          "state_var",
          "competence_var"
        )
      ],
      direction = "long",
      varying = c("year_start", "year_end"),
      sep = "_"
    )


    plot_dat[["plot_points"]] <- remove_columns(plot_dat[["plot_points"]], c("time", "id"))
  } else {
    plot_dat[["plot_points"]] <- plot_dat[["plot_lines"]]
  }


    plot_dat[["plot_points"]] <- merge_2(
      plot_dat[["plot_points"]],
      list_building_blocks[["noTrend_noComp"]],
      by = c(
        "grouping_var",
        "state_var",
        "year",
        "competence_var",
        "depVar"
      ),
      all.x = TRUE
    )

    plot_dat[["plot_points"]] <- merge_2(
      plot_dat[["plot_points"]],
      noTrend_merged$comp_within_whole_noTrend,
      by = c(
        "grouping_var",
        "state_var",
        "year",
        "competence_var",
        "depVar"
      ),
      all.x = TRUE
    )

# if(any(grouping_var) != noGroup ){
#   plot_dat[["plot_points"]] <- plot_dat[["plot_points"]][plot_dat[["plot_points"]]$grouping_var != "noGroup", ]
# }


  ## Order columns
  plot_dat <- lapply(plot_dat, function(x) {
    x <- x[, grep("\\.x$|\\.y$", colnames(x), invert = TRUE)]
    x[, order(colnames(x))]
  })

  return(plot_dat)
}


prepare_noTrend <- function(list_building_blocks, merging_columns){



prepare_comp_noTrend <- function(dat){ # noTrend_Comp

  comp_trend <- data.frame()
  dat_comp_wide_list <- list()

  for(comp in unique(dat$comparison)){

    dat_comp <- dat[!is.na(dat$comparison) & dat$comparison == comp, ]
    dat_comp$grouping_var <- gsub("\\.vs\\..*", "", dat_comp$grouping_var)

    ## Get all state comparisons (without wholeGroup)
    dat_comp <- dat_comp[dat_comp$grouping_var != "noGroup" & dat_comp$state_var != "wholeGroup" & dat_comp$compare_2_Comp != "wholeGroup" & !is.na(dat_comp$compare_2_Comp), ]

    if(nrow(dat_comp) > 0){

    dat_comp$compare_2_Comp <- paste0(comp, "_", dat_comp$compare_2_Comp)

    dat_comp <- remove_columns(dat_comp, c("comparison", "compare_1_Comp"))

  dat_comp_wide_list[[comp]] <- reshape(dat_comp,
                           direction = "wide",
                           idvar = c("depVar", "competence_var", "grouping_var", "state_var", "year"),
                           timevar = c("compare_2_Comp"),
                           sep = "_"
                           )

  if(nrow(dat_comp_wide_list[[comp]]) != 288){warning(comp)}

  comp_trend <- merge_2(comp_trend,
                        dat_comp_wide_list[[comp]],
                        by = c("depVar", "competence_var", "grouping_var", "state_var", "year"),
                        all.x = TRUE
                        )

  }
  }
}
## Noch irgendwie checken, ob compare_1 wirklich jeweils gleich ist -> evtl danach in dat_comp_wide_list


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

  return(list("noTrend_merged" = noTrend_data_merged,
              "comp_within_whole_noTrend" = comp_within_whole_noTrend)
  )
}
