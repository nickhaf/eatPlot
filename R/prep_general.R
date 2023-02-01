#' Prepare eatRep output for further data wrangling.
#'
#' @param data eatRep output data.frame.
#' @param grouping_var Variable with subgroups.
#' @param competence Competence variable.
#'
#' @return List of data frames with different filters.
#' @export
#'
#' @examples #tbd
prep_general <- function(data, grouping_var = "", competence = ""){

  colnames(data)[colnames(data) == grouping_var] <- "grouping_var"
  colnames(data) <- gsub("\\.", "_", colnames(data))
  data <- data[data$kb == competence & data$parameter == "mean", ]
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]
  groups <- unique(data$grouping_var[!is.na(data$grouping_var)])


## filter rows, so different types of data.frames can be build:
  filtered_list <- list()



# Prepare point estimates -------------------------------------------------
filtered_list[["point_data"]] <- prep_long(data[is.na(data$comparison), ],
                                include_pattern = "est_|p_|se_|es_",
                                remove_pattern = "trend",
                                suffix = "point")

# Prepare trend data ------------------------------------------------------
  years_colnames <- unique(unlist(regmatches(colnames(data), gregexpr("[[:digit:]]+", colnames(data)))))
remove_columns <- unique(as.vector(
  sapply(c("es_", "est_", "se_", "p_"), function(val){
  sapply(years_colnames, function(year){
    grep(
      paste0("^",val, year, "$"),
      colnames(data),
      value = TRUE)
},
USE.NAMES = FALSE)
},
USE.NAMES = FALSE)
))


filtered_list[["trend_data"]] <- prep_long(data[!is.na(data$comparison) & data$comparison == "crossDiff", ],
                                 include_pattern = "est_trend|sig_trend|se_trend|es_trend",
                                 remove_pattern = paste0(paste0("^", remove_columns, "$"), collapse = "|"),
                                 suffix = "whole")

filtered_list[["trend_data"]] <- split_years(filtered_list[["trend_data"]])






#filtered_list[["bl_vs_wholeGroup"]] <- data[get_group(data$group, BLs) & get_wholeGroup(data$group), ]

#  filtered_list[["wholeGroup_vs_wholeGroup"]] <- data[get_group(data$group, groups, starts_with = "^") & get_wholeGroup(data$group) & !get_group(data$group, BLs), ]

#  filtered_list[["bl_vs_bl"]] <- data[get_group(data$group, BLs, ends_with = "$") & get_group(data$group, groups) & !is.na(data$comparison), ]



  ## Schleife durchgehen, die alle p_ Spalten filtert und calc_sig anwendet.

  return(filtered_list)

}


