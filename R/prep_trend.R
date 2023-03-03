#' Prepare trend data for plotting
#'
#' Performs different data transformations, to bring the input data.frame into the correct formats for different kind of plots.
#'
#' @param dat Input data.frame stemming from `eatRep`.
#' @param competence Character string containing the competence that should be plotted.
#' @param grouping_var Character string containing the column name in `dat` that should be used to distinguish between subgroups.
#' @param state_var Character string containing the column name in `dat` that should be used to distinguish between groups that should be plotted seperatly. Normally, this should be the states ("Bundesländer"). Therfore, defaults to `"TR_BUNDESLAND"`.
#' @param group_var Character string containing the column name in `dat` that contains the different group memberships in one string. Defaults to `"group"`.
#' @param competence_var Character string containing the column name in `dat` that contains the different competences. Defaults to `"kb"`.
#' @param x_years  List of numeric vectors containing the start and end year, between which a trend line should be plotted. Per default, lines are drawn from every year to the next consecutive year.
#' @param x_braces List of numeric vectors containing the start and end year, between which a brace should be plotted. Per default, braces are drawn from the last year to every other year included in the data.
#' @param sig_niveau Numeric indicating the border, below which p-values will be considered significant. Defaults to `0.05`.
#' @param plot_mean Logical value indicating whether the mean of both subgroups should be plotted as well.
#' @param parameter Character string of the parameter value that should be reported. Defaults to `"mean"`.
#'
#' @returns `prep_trend()` returns a list containing four data.frames prepared for plotting with different [eatPlot] functions. This includes the data.frames:
#' * `plot_points` for plotting with [plot_points()]
#' * `plot_lines` for plotting with [plot_lines()]
#' * `plot_braces` for plotting with [plot_braces()]
#' * `plot_background_lines` for plotting with [plot_background_lines()].
#' @export
#'
#' @examples # tbd
prep_trend <- function(dat,
                       competence,
                       grouping_var = "",
                       state_var = "TR_BUNDESLAND",
                       competence_var = "kb",
                       group_var = "group",
                       x_years = NULL,
                       x_braces = NULL,
                       sig_niveau = 0.05,
                       plot_mean = FALSE,
                       parameter = "mean") {
  dat <- as.data.frame(dat)
  states <- unique(dat[, state_var])[!is.na(unique(dat[, state_var]))]
  if (grouping_var != "") {
    sub_groups <- unique(dat[, grouping_var][!is.na(dat[, grouping_var])])
  } else {
    sub_groups <- NULL
  }

  dat <- clean_data(
    dat = dat,
    states = states,
    sub_groups = sub_groups,
    competence = competence,
    grouping_var = grouping_var,
    group_var = group_var,
    state_var = state_var,
    competence_var = competence_var,
    parameter = parameter
  )

  if (any(!is.na(dat$comparison))) {
    dat <- get_comparisons(dat,
      group_col = "group_var",
      states = states[states != "wholeGroup"],
      sub_groups = sub_groups
    )
  }

  list_building_blocks <- prep_data_blocks(
    data_clean = dat,
    sig_niveau = sig_niveau,
    states,
    sub_groups
  )


  # Prepare the trend-data.frame --------------------------------------------
  # Data with comparison:
  comp_wholeGroup <- list_building_blocks[["trend_comp_data"]][list_building_blocks[["trend_comp_data"]]$compare_2 == "wholeGroup", ]
  comp_state <- list_building_blocks[["trend_comp_data"]][list_building_blocks[["trend_comp_data"]]$compare_2 == "BL" | list_building_blocks[["trend_comp_data"]]$compare_1 == "_groupingVar", ]

  if (nrow(comp_state) != 0) {
    comp_within_whole <- merge_trend_data(
      trend_data_1 = comp_state,
      trend_data_2 = comp_wholeGroup,
      suffixes = c("_comp_within", "_comp_whole"),
      all.x = TRUE
    )
  } else {
    comp_within_whole <- comp_wholeGroup
  }

  ## Add data without comparison:
  if (nrow(comp_within_whole) != 0) {
    trend_data_merged <- merge_trend_data(
      trend_data_1 = comp_within_whole,
      trend_data_2 = list_building_blocks[["trend_no_comp_data"]],
      suffixes = c("_comp", "_no_comp"),
      all = TRUE
    )
  } else {
    trend_data_merged <- list_building_blocks[["trend_no_comp_data"]]
  }
  colnames(trend_data_merged) <- gsub("_trend$", "_trend_no_comp", colnames(trend_data_merged))


  trend_data_final <- merge_trend_point(
    trend_data = trend_data_merged,
    point_data = list_building_blocks[["point_no_comp_data"]]
  )
  trend_data_final$grouping_var <- droplevels(trend_data_final$grouping_var)

  # Prepare the wholeGroup data.frame ---------------------------------------
  trend_data_wholeGroup <- merge_trend_point(
    list_building_blocks[["wholeGroup_trend"]],
    list_building_blocks[["wholeGroup_point"]]
  )

  # Fill up NAs -------------------------------------------------------------
  ## Fill up NA significances with FALSE (those which emerged through merging)
  for (i in grep("sig_", colnames(trend_data_final))) {
    trend_data_final[, i] <- ifelse(is.na(trend_data_final[, i]), FALSE, trend_data_final[, i])
  }

  # Build plotlist ----------------------------------------------------------
  plot_dat <- list()

  # plot_lines

  if(is.null(x_years)){
  lineplot_years <- consecutive_numbers(c(trend_data_final$year_start, trend_data_final$year_end))
  }else{
    lineplot_years <- x_years
}
  plot_dat[["plot_lines"]] <- trend_data_final[filter_years(trend_data_final, lineplot_years), ]

  if (grouping_var != "" & plot_mean == FALSE) { ## Should the mean group be plotted as well (not only the subgroups)?
    plot_dat[["plot_lines"]] <- plot_dat[["plot_lines"]][plot_dat[["plot_lines"]]$grouping_var != "noGroup", ]
  }

  # plot_braces
  if (is.null(x_braces)) {
    ## Draw braces from last year to every other year
    plot_years <- unique(c(trend_data_final$year_start, trend_data_final$year_end))
    braceplot_years <- lapply(plot_years[-which(plot_years == max(plot_years))], function(x) {
      c(x, max(plot_years))
    })
  } else {
    braceplot_years <- x_braces
  }

  plot_dat[["plot_braces"]] <- trend_data_final[filter_years(trend_data_final, braceplot_years), ]
  if (grouping_var != "" & plot_mean == FALSE) { ## Should the mean group be plotted as well (not only the subgroups)?
    plot_dat[["plot_braces"]] <- plot_dat[["plot_braces"]][plot_dat[["plot_braces"]]$grouping_var != "noGroup", ]
  }

  # plot_background_lines
  plot_dat[["plot_background_lines"]] <- trend_data_wholeGroup[filter_years(trend_data_wholeGroup, lineplot_years), ]

  # plot_bar

  if (nrow(comp_state) != 0) {
    plot_dat[["plot_bar"]] <- merge(
      list_building_blocks[["point_no_comp_data"]],
      list_building_blocks[["point_comp_data"]],
      by = c("state_var", "year", "grouping_var", "depVar"),
      suffixes = c("_no_comp", "_comp"),
      all = TRUE
    )
  } else {
    plot_dat[["plot_bar"]] <- list_building_blocks[["point_no_comp_data"]]
  }

  ## Trends anhängen
  plot_dat <- get_trend(plot_dat)


  # plot_points
  ## for the split lineplot, the middle points have to be plotted two times. Therefore, the plot_points function is build using the comparisons already calculated.

  dat_long <- reshape(plot_dat[["plot_lines"]][, c("depVar", "grouping_var", "year_start", "year_end", "trend", "group_var", "state_var")],
          direction = "long",
          varying = c("year_start", "year_end"),
          sep = "_"
  )


  plot_dat[["plot_points"]] <- merge(dat_long, list_building_blocks[["point_no_comp_data"]], by = c("grouping_var", "group_var", "state_var", "year"), all.x = TRUE)
  plot_dat[["plot_points"]] <- plot_dat[["plot_points"]][plot_dat[["plot_points"]]$year %in% unlist(c(lineplot_years, braceplot_years)), ]
  plot_dat[["plot_points"]] <- plot_dat[["plot_points"]][plot_dat[["plot_points"]]$grouping_var != "noGroup", ]


  return(plot_dat)
}



# utils

# Return rows with respective start and end years.
filter_years <- function(dat, year_list) {
  # Filter the respective rows
  year_rows <- unlist(lapply(year_list, function(x) {
    which(dat$year_start == x[1] & dat$year_end == x[2])
  }))
  return(year_rows)
}

get_trend <- function(plot_list){
 res <- lapply(plot_list, function(x){
    if(all(c("year_start", "year_end") %in% colnames(x))){
      x$trend <- paste0(x$year_start, x$year_end)
      return(x)
    }else{return(x)}
  })
 return(res)
}

# dat <- trend_books
# grouping_var = "KBuecher_imp3"
# competence = "GL"
# state_var = "TR_BUNDESLAND"
# competence_var = "kb"
# group_var = "group"
# x_braces = NULL
# sig_niveau = 0.05
# plot_mean = FALSE
# parameter = "mean"
