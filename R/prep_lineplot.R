#' Title
#'
#' @param data Trend data from eatRep.
#' @param grouping_var Grouping variable.
#' @param competence Competence area.
#' @param sig_niveau Significance niveau.
#'
#' @return List of data.frames needed for different plot-functions.
#' @export
#'
#' @examples # tbd
prep_lineplot <- function(data, grouping_var = "", competence, sig_niveau = 0.05) {

  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]
  groups <- unique(data$grouping_var[!is.na(data$grouping_var)])


plot_data <- list()

  list_general <- prep_general(data, grouping_var = grouping_var, competence = competence, sig_niveau = sig_niveau)
  within_whole <- merge_within_whole(list_general[["trend_data"]], groups = groups, BLs = BLs)[["within_whole"]]
  trend_point <- merge_trend_point(trend_data = within_whole, point_data = list_general[[1]])
  wholeGroup_trend_point <- merge_trend_point(list_general[["wholeGroup_trend"]], list_general[["wholeGroup_point"]])


plot_data[["plot_points"]] <- list_general[["point_data"]]

  plot_years_trend <- consecutive_numbers(c(trend_point$year_start, trend_point$year_end))
  plot_data[["plot_lines"]] <- trend_point[filter_years(trend_point, plot_years_trend), ]


  plot_years <- unique(c(trend_point$year_start, trend_point$year_end))

  ## Draw braces from last year to every other year
 plot_years_braces <- lapply(plot_years[-which(plot_years == max(plot_years))], function(x) {
    c(x, max(plot_years))
  })

  plot_data[["plot_braces"]] <- trend_point[filter_years(trend_point, plot_years_braces), ]


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
