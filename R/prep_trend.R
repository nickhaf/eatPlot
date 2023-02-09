#' Prepare trend data for plotting
#'
#' Performs different data transformations, to bring the input data.frame into the correct formats for different kind of plots.
#'
#' @param data Input data.frame stemming from `eatRep`.
#' @param grouping_var Character string containing the column in `data` that should be used to distinguish between subgroups.
#' @param competence Character string containing the competence that should be plotted. Currently has to be found in `data$kb` (even though that should be made optional in the future).
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
prep_trend <- function(data, grouping_var = "", competence, sig_niveau = 0.05) {
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]
  groups <- unique(data[ , grouping_var][!is.na(data[ ,grouping_var])])

  data <- clean_data(data, grouping_var = grouping_var, competence = competence, BLs = BLs, groups = groups)

  if(any(!is.na(BLs))){
    data <- get_comparisons(data, "group",
                                  BLs = BLs[BLs != "wholeGroup"],
                                  groups = groups
    )
  }


  plot_data <- list()

  list_general <- prep_general(data, sig_niveau = sig_niveau, BLs, groups)
  within_whole <- merge_within_whole(trend_comp_data = list_general[["trend_data"]], trend_no_comp_data = list_general[["trend_no_comp_data"]], BLs = BLs)[["trend_data_final"]]
  trend_point <- merge_trend_point(trend_data = within_whole, point_data = list_general[[1]])
  wholeGroup_trend_point <- merge_trend_point(list_general[["wholeGroup_trend"]], list_general[["wholeGroup_point"]])


  plot_data[["plot_points"]] <- list_general[["point_data"]]

  plot_years_trend <- consecutive_numbers(c(trend_point$year_start, trend_point$year_end))
  plot_data[["plot_lines"]] <- trend_point[filter_years(trend_point, plot_years_trend), ]

  ## Draw braces from last year to every other year
  plot_years <- unique(c(trend_point$year_start, trend_point$year_end))
  plot_years_braces <- lapply(plot_years[-which(plot_years == max(plot_years))], function(x) {
    c(x, max(plot_years))
  })

  plot_data[["plot_braces"]] <- trend_point[filter_years(trend_point, plot_years_braces), ]
  plot_data[["plot_braces"]] <- plot_data[["plot_braces"]][plot_data[["plot_braces"]]$grouping_var != "noGroup", ]
  plot_data[["plot_background_lines"]] <- wholeGroup_trend_point[filter_years(wholeGroup_trend_point, plot_years_trend), ]

  return(plot_data)
}



# utils

# Return rows with respective start and end years.
filter_years <- function(data, year_list) {
  # Filter the respective rows
  year_rows <- unlist(lapply(year_list, function(x) {
    which(data$year_start == x[1] & data$year_end == x[2])
  }))
  return(year_rows)
}
