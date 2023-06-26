#' Prepare trend data for plotting
#'
#' Performs different data transformations, to bring the input data.frame into the correct formats for different kind of plots.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param dat Input data.frame stemming from `eatRep`.
#' @param comparisons Character vector for filtering specific comparisons. Defaults to `NULL`, in which case all comparisons will be prepared.
#' @param competence Character string containing the competence that should be plotted.
#' @param grouping_vars Character vector containing maximal two column names in `dat` that should be used to distinguish between subgroups. If two columns are provided, they will be merged internally into one.
#' @param states Character vector of the states that should be plotted.
#' @param state_var Character string containing the column name in `dat` that should be used to distinguish between groups that should be plotted seperatly. Normally, this should be the states ("Bundesländer"). Therfore, defaults to `"TR_BUNDESLAND"`.
#' @param group_var Character string containing the column name in `dat` that contains the different group memberships in one string. Defaults to `"group"`.
#' @param grouping_vars_groups Character vector containing the groups from `grouping_var` you want to plot. Defaults to `NULL`, in which case all groups are prepared.
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
                      comparisons = NULL,
                      competence_var = "kb",
                      competence = NULL,
                      states = NULL,
                      state_var = "TR_BUNDESLAND",
                      group_var = "group",
                      grouping_vars = NULL,
                      grouping_vars_groups = NULL,
                      sig_niveau = 0.05,
                      plot_mean = FALSE,
                      parameter = "mean") {
  # Checks ------------------------------------------------------------------
  ## Hier alle Spalten bilden, die später gebraucht werden.

  dat <- as.data.frame(dat)

  ## Check arguments
  stopifnot(is.data.frame(dat))
  stopifnot(is.character(competence) | is.null(competence))
  stopifnot(is.character(grouping_vars) | is.null(grouping_vars))
  stopifnot(is.character(grouping_vars_groups) | is.null(grouping_vars_groups))
  stopifnot(is.character(states) | is.null(states))
  stopifnot(is.character(state_var))
  stopifnot(is.character(competence_var))
  stopifnot(is.character(group_var))
  stopifnot(is.numeric(sig_niveau))
  stopifnot(is.logical(plot_mean))
  stopifnot(is.character(parameter))

  if (any(dat[!is.na(dat[, state_var]), state_var] == "")) {
    warning(paste0("Your state_var column '", state_var, "' includes missing Values that are not coded as NA. Please recode to NA."), call. = FALSE)
  }

  ## Check if columns are in data:
  sapply(c(grouping_vars, state_var, competence_var, group_var, "comparison"), check_column, dat = dat)

  if (!is.null(comparisons)) {
    dat <- dat[dat$comparison %in% comparisons | is.na(dat$comparison), ]
  }

  ## Build grouping_var
  dat <- fill_column(dat, group_var)
  dat <- construct_grouping_var(dat, grouping_vars, group_var = "group_var")


  dat <- fill_column(dat, competence_var)
  dat <- fill_column(dat, state_var)

  dat <- filter_subgroups(dat, grouping_vars, grouping_vars_groups)

  ## remove the old columns, but only after all columns have been build, in case one old column is needed 2x.
  dat <- dat[, -which(colnames(dat) %in% c(competence_var, grouping_vars, state_var, group_var))]

  colnames(dat) <- gsub("\\.", "_", colnames(dat))
  colnames(dat) <- gsub("sig_", "p_", colnames(dat))
  colnames(dat) <- gsub("^sig$", "p", colnames(dat))

  if (!is.null(competence)) {
    if (any(!competence %in% dat$competence_var)) {
      stop("Your competence is not in your competence_var column.", call. = FALSE)
    }
  }
  all_states <- unique(dat$state_var)[!is.na(unique(dat$state_var))]


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

  sub_groups <- unique(dat$grouping_var)[!is.na(unique(dat$grouping_var))]

  dat <- clean_data(
    dat = dat,
    states = states,
    all_states = all_states,
    sub_groups = sub_groups,
    competence = competence,
    parameter = parameter
  )

  dat$comparison <- gsub("_", "", dat$comparison)

  if (any(!is.na(dat$comparison))) {
    dat <- get_comparisons(dat,
      states = all_states[all_states != "wholeGroup"],
      sub_groups = sub_groups
    )

    ## Currently, only comparisons without nested comparisons are supported:

    dat <- dat[!grepl("\\.vs\\.", dat$compare_1), ]
  }

  list_building_blocks <- prep_data_blocks(
    data_clean = dat,
    sig_niveau = sig_niveau,
    all_states,
    sub_groups,
    merging_columns = merging_columns
  )

  # Build noTrend dataframe -------------------------------------------------
  comp_dat_noTrend <- prepare_comp(list_building_blocks$noTrend_Comp, year_columns = "year")

  noTrend_data_merged <- merge_2(
    dat_1 = comp_dat_noTrend,
    dat_2 = list_building_blocks$noTrend_noComp,
    by = c("depVar", "competence_var", "grouping_var", "state_var", "year"),
    all = TRUE
  )



  # Prepare the trend-data.frame --------------------------------------------
  if (any(grepl("trend", colnames(dat)))) {
    dat_comp_Trend <- list_building_blocks$Trend_Comp
    dat_comp_Trend <- rename_columns(dat_comp_Trend, "compare_2_Trend_Comp", "compare_2_Comp")

    comp_dat_Trend <- prepare_comp(dat_comp_Trend, year_columns = c("year_start", "year_end", "years_Trend"))

    Trend_data_merged <- merge_2(
      dat_1 = comp_dat_Trend,
      dat_2 = list_building_blocks$Trend_noComp,
      by = c("depVar", "competence_var", "grouping_var", "state_var", "year_start", "year_end", "years_Trend"),
      all = TRUE
    )


    # Merge to final data frame -----------------------------------------------
    trend_data_final <- merge_trend_point(
      trend_data = Trend_data_merged,
      point_data = noTrend_data_merged
    )

    ## Drop unused levels
    if (any(!is.na(trend_data_final$grouping_var)) & nrow(list_building_blocks[["Trend_noComp_wholeGroup"]]) != 0 & nrow(list_building_blocks[["noTrend_noComp_wholeGroup"]])) {
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
    trend_data_final[, i] <- ifelse(is.na(trend_data_final[, i]),
      FALSE,
      trend_data_final[, i]
    )
  }

  # Build plotlist ----------------------------------------------------------
  plot_dat <- list()


  # plot lines --------------------------------------------------------------

  plot_dat[["plot_lines"]] <- trend_data_final

  if (!is.null(grouping_vars) & plot_mean == FALSE) { ## Should the mean group be plotted as well (not only the subgroups)?
    plot_dat[["plot_lines"]] <- plot_dat[["plot_lines"]][plot_dat[["plot_lines"]]$grouping_var != "noGroup", ]
  }
  plot_dat[["plot_lines"]] <- plot_dat[["plot_lines"]][plot_dat[["plot_lines"]]$grouping_var != "wholeGroup", ]



  # plot braces -------------------------------------------------------------

  plot_dat[["plot_braces"]] <- trend_data_final

  if (!is.null(grouping_vars) & plot_mean == FALSE) { ## Should the mean group be plotted as well (not only the subgroups)?
    plot_dat[["plot_braces"]] <- plot_dat[["plot_braces"]][plot_dat[["plot_braces"]]$grouping_var != "noGroup", ]
  }



  # plot background lines ---------------------------------------------------

  plot_dat[["plot_background_lines"]] <- trend_data_wholeGroup


  # plot_bar ----------------------------------------------------------------
  if (nrow(noTrend_data_merged) != 0) {
    id_vars <- c("grouping_var", "state_var", "competence_var", "depVar") # , colnames(noTrend_data_merged)[grep("compare_", colnames(noTrend_data_merged))])

    if (all(noTrend_data_merged$year == "noTrend")) {
      noTrend_data_merged_wide <- noTrend_data_merged
    } else {
      noTrend_data_merged_wide <- stats::reshape(noTrend_data_merged,
        direction = "wide",
        timevar = "year",
        idvar = id_vars,
        sep = "_"
      )
    }
  } else {
    noTrend_data_merged_wide <- data.frame()
  }

  if (nrow(Trend_data_merged) != 0) {
    # earlier merging might lead to NAs in the comparison columns. As they might be needed as ID for reshaping, NAs are substituted
    # compare_cols <- colnames(Trend_data_merged)[grep("compare_", colnames(Trend_data_merged))]
    # for (i in compare_cols) {
    #   Trend_data_merged[is.na(Trend_data_merged[, i]), i] <- "no_comp"
    # }

    id_vars <- c("grouping_var", "state_var", "competence_var", "depVar") # , colnames(Trend_data_merged)[grep("compare_", colnames(Trend_data_merged))])
    Trend_data_merged_wide <- stats::reshape(
      Trend_data_merged,
      direction = "wide",
      timevar = "years_Trend",
      idvar = id_vars,
      sep = "_"
    )
  } else {
    Trend_data_merged_wide <- data.frame()
  }

  plot_dat[["plot_tablebar"]] <- merge_2(
    noTrend_data_merged_wide,
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
    noTrend_data_merged,
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


filter_subgroups <- function(dat, grouping_vars, grouping_vars_groups) {
  if (!is.null(grouping_vars_groups)) {
    if (any(!unique(grouping_vars_groups) %in% unlist(dat[, grouping_vars]))) {
      stop(paste0("One or more of your grouping_vars_groups are not found in your grouping_vars columns."))
    }

    if (any(grouping_vars_groups %in% dat[, grouping_vars[1]])) {
      grouping_var_1 <- is.na(dat$grouping_var) | dat[, grouping_vars[1]] %in% grouping_vars_groups | is.na(dat[, grouping_vars[1]])
    } else {
      grouping_var_1 <- rep(FALSE, nrow(dat))
    }

    if (!is.na(grouping_vars[2])) {
      if (any(grouping_vars_groups %in% dat[, grouping_vars[2]])) {
        grouping_var_2 <- is.na(dat$grouping_var) | dat[, grouping_vars[2]] %in% grouping_vars_groups
      }
    } else {
      grouping_var_2 <- rep(FALSE, nrow(dat))
    }

    dat <- dat[grouping_var_1 | grouping_var_2, ]
  }
  return(dat)
}
