#' Title
#'
#' @param trend_data Trend data.
#' @param groups Groups.
#' @param BLs States.
#'
#' @return list of data frames.
#' @export
#'
#' @examples #tbd
merge_within_whole <- function(trend_data, groups, BLs){


trend_data <- trend_data[ , !(colnames(trend_data) %in% c("depVar", "modus", "comparison", "parameter", "kb"))]



  plot_data_list <- list()

  wholeGroup_vs_wholeGroup <- get_group(trend_data$group, groups, starts_with = "^") & get_wholeGroup(trend_data$group) & !get_group(trend_data$group, BLs)

  plot_data_list[["bl_vs_wholeGroup"]] <- trend_data[get_group(trend_data$group, BLs) & get_wholeGroup(trend_data$group) | wholeGroup_vs_wholeGroup, ]
  plot_data_list[["bl_vs_bl"]] <- trend_data[get_group(trend_data$group, BLs, ends_with = "$") | wholeGroup_vs_wholeGroup, ]


  data_whole <- plot_data_list[["bl_vs_wholeGroup"]]
  plot_data_list[["within_whole"]] <- merge(plot_data_list[["bl_vs_wholeGroup"]],
                                            plot_data_list[["bl_vs_bl"]],
                                            by = c("TR_BUNDESLAND", "grouping_var", "year_start", "year_end"),
                                            sort = FALSE,
                                            suffixes = c("_whole", "_within"),
                                            all = TRUE
  )

  return(plot_data_list)
}
