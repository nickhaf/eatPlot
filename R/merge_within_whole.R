#' Title
#'
#' @param trend_data Trend data.
#' @param groups Groups.
#' @param BLs States.
#' @param compare_against Should be compared against the wholeGroup, or grouping_vars wihtin the wholeGroup? Currently only "wholeGroup" necessary.
#'
#' @return list of data frames.
#' @export
#'
#' @examples #tbd
merge_within_whole <- function(trend_data, BLs, compare_against = "wholeGroup"){


trend_data <- trend_data[ , !(colnames(trend_data) %in% c("depVar", "modus", "comparison", "parameter", "kb"))]

plot_data_list <- list()

  plot_data_list[["bl_vs_wholeGroup"]] <- trend_data[trend_data$compare_2 == compare_against, ]
  plot_data_list[["bl_vs_bl"]] <- trend_data[trend_data$compare_2 == "BL" | trend_data$compare_1 == "_groupingVar", ]

  plot_data_list[["within_whole"]] <- merge(plot_data_list[["bl_vs_wholeGroup"]],
                                            plot_data_list[["bl_vs_bl"]],
                                            by = c("TR_BUNDESLAND", "grouping_var", "year_start", "year_end"),
                                            sort = FALSE,
                                            suffixes = c("_whole", "_within"),
                                            all.y = TRUE)

  return(plot_data_list)
}
