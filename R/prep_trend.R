#' Prepare trend data for plotting
#'
#' Performs different data transformations, to bring the input data.frame into the correct formats for different kind of plots.
#'
#' @param dat Input data.frame stemming from `eatRep`.
#' @param grouping_var Character string containing the column in `dat` that should be used to distinguish between subgroups.
#' @param state_var Character string containing the column in `dat` that should be used to distinguish between groups that should be plotted seperatly. Normally, this should be the states ("Bundesländer"). Therfore, defaults to `"TR_BUNDESLAND"`.
#' @param competence Character string containing the competence that should be plotted. Currently has to be found in `dat$kb` (even though that should be made optional in the future).
#' @param sig_niveau Numeric indicating the border, below which p-values will be considered significant. Defaults to `0.05`.
#'
#' @returns `prep_trend()` returns a list containing four data.frames prepared for plotting with different [eatPlot] functions. This includes the data.frames:
#' * `plot_points` for plotting with [plot_points()]
#' * `plot_lines` for plotting with [plot_lines()]
#' * `plot_braces` for plotting with [plot_braces()]
#' * `plot_background_lines` for plotting with [plot_background_lines()].
#' @export
#'
#' @examples # tbd
prep_trend <- function(dat, competence, grouping_var = "", state_var = "TR_BUNDESLAND", competence_var = "kb", sig_niveau = 0.05) {
  states <- unique(dat[, state_var])[!is.na(unique(dat[, state_var]))]
  groups <- unique(dat[, grouping_var][!is.na(dat[, grouping_var])])

  dat <- clean_data(dat, grouping_var = grouping_var, competence = competence, BLs = states, groups = groups)

  if (any(!is.na(states))) {
    dat <- get_comparisons(dat, "group",
      states = states[states != "wholeGroup"],
      groups = groups
    )
  }


  plot_dat <- list()

  list_general <- prep_general(dat, sig_niveau = sig_niveau, states, groups)
  within_whole <- merge_within_whole(trend_comp_data = list_general[["trend_data"]], trend_no_comp_data = list_general[["trend_no_comp_data"]], BLs = states)[["trend_data_final"]]
  trend_point <- merge_trend_point(trend_data = within_whole, point_dat = list_general[[1]])
  wholeGroup_trend_point <- merge_trend_point(list_general[["wholeGroup_trend"]], list_general[["wholeGroup_point"]])


  plot_dat[["plot_points"]] <- list_general[["point_dat"]]

  plot_years_trend <- consecutive_numbers(c(trend_point$year_start, trend_point$year_end))
  plot_dat[["plot_lines"]] <- trend_point[filter_years(trend_point, plot_years_trend), ]

  ## Draw braces from last year to every other year
  plot_years <- unique(c(trend_point$year_start, trend_point$year_end))
  plot_years_braces <- lapply(plot_years[-which(plot_years == max(plot_years))], function(x) {
    c(x, max(plot_years))
  })

  plot_dat[["plot_braces"]] <- trend_point[filter_years(trend_point, plot_years_braces), ]
  plot_dat[["plot_braces"]] <- plot_dat[["plot_braces"]][plot_dat[["plot_braces"]]$grouping_var != "noGroup", ]
  plot_dat[["plot_background_lines"]] <- wholeGroup_trend_point[filter_years(wholeGroup_trend_point, plot_years_trend), ]

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
