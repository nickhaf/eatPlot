#' Title
#'
#' @param trend_comp_data Trend data containing trend comparisons.
#' @param trend_no_comp_data Trend data containing estimates for the trends.
#' @param BLs States.
#' @param compare_against Should be compared against the wholeGroup, or grouping_vars wihtin the wholeGroup? Currently only "wholeGroup" necessary.
#'
#' @return list of data frames.
#' @export
#'
#' @examples #tbd
merge_within_whole <- function(trend_comp_data, trend_no_comp_data = NULL, BLs, compare_against = "wholeGroup"){


trend_comp_data <- trend_comp_data[ , !(colnames(trend_comp_data) %in% c("depVar", "modus", "comparison", "parameter", "kb"))]


plot_data_list <- list()

  plot_data_list[["bl_vs_wholeGroup"]] <- trend_comp_data[trend_comp_data$compare_2 == compare_against, ]
  plot_data_list[["bl_vs_bl"]] <- trend_comp_data[trend_comp_data$compare_2 == "BL" | trend_comp_data$compare_1 == "_groupingVar", ]

  plot_data_list[["within_whole"]] <- merge(plot_data_list[["bl_vs_wholeGroup"]],
                                            plot_data_list[["bl_vs_bl"]],
                                            by = c("state_var", "grouping_var", "year_start", "year_end"),
                                            sort = FALSE,
                                            suffixes = c("_whole", "_within"),
                                            all.y = TRUE)

  if(is.null(trend_no_comp_data)){
    plot_data_list["trend_data_final"] <- list(NULL)
  }else{
  trend_no_comp_data <- trend_no_comp_data[ , !(colnames(trend_no_comp_data) %in% c("depVar", "modus", "comparison", "parameter", "kb"))]
  plot_data_list[["trend_data_final"]] <- merge(plot_data_list[["within_whole"]],
                                               trend_no_comp_data,
                                               by= c("state_var", "grouping_var", "year_start", "year_end"),
                                               sort = FALSE,
                                               suffixes = c("_comp", "no_comp"),
                                               all = TRUE)
  colnames(plot_data_list[["trend_data_final"]]) <- gsub("_trend$", "_trend_no_comp",   colnames(plot_data_list[["trend_data_final"]]))
}

  return(plot_data_list)
}
