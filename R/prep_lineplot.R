prep_lineplot <- function(data, grouping_var, competence, sig_niveau = 0.05){


  data <- data[data$kb == competence & data$parameter == "mean", ]
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]

# prepare_trend_within
  data_trend_within <- data[data$comparison != "crossDiff", ]


  # prepare_trend_crossDiff

  wholeGroup_rows <- unlist(sapply(unique(data[!is.na(data[, grouping_var]), grouping_var]), function(x){
    filter_strings(identifier = BLs, paste_vec = paste0("_", x, ".vs.wholeGroup"), val_vec = data$group)
  }, USE.NAMES =FALSE))


  data_trend_whole <- data[wholeGroup_rows, ]
  data_trend_whole_prep <- prep_trend(data_trend_whole, grouping_var = grouping_var)


prep_list <- list()



  prep_list[["lineplot"]] <- prep_line(data, grouping_var = grouping_var, sig_niveau = sig_niveau)
  prep_list[["pointplot"]] <- prep_points(data, grouping_var = grouping_var, competence = "GL", sig_niveau = sig_niveau)

  return(prep_list)
}
