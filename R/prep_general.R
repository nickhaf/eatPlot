prep_general <- function(data, grouping_var, competence){

  colnames(data)[colnames(data) == grouping_var] <- "grouping_var"
  data <- data[data$kb == competence & data$parameter == "mean", ]
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]
  groups <- unique(data$grouping_var[!is.na(data$grouping_var)])


## filter rows, so different types of data.frames can be build:

  filtered_list <- list()

  filtered_list[["bl_vs_wholeGroup"]] <- data[get_group(data$group, BLs) & get_wholeGroup(data$group), ]

  filtered_list[["wholeGroup_vs_wholeGroup"]] <- data[get_group(data$group, groups, starts_with = "^") & get_wholeGroup(data$group), ]

  filtered_list[["bl_vs_bl"]] <- data[get_group(data$group, BLs, ends_with = "$") & get_group(data$group, groups), ]

  filtered_list[["bl_point_estimates"]] <- data[is.na(data$comparison), ]

  return(filtered_list)

}

