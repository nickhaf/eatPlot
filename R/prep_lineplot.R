prep_lineplot <- function(data, grouping_var, competence, sig_niveau = 0.05){


  data <- data[data$kb == competence & data$parameter == "mean", ]
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]

  prep_list <- list()

  whole_group_rows <- unlist(sapply(unique(data[!is.na(data[, grouping_var]), grouping_var]), function(x){
    filter_strings(identifier = BLs, paste_vec = paste0("_", x, ".vs.wholeGroup"), val_vec = data$group)
  }, USE.NAMES =FALSE))


within_group_rows <-  unlist(sapply(unique(data[!is.na(data[, grouping_var]), grouping_var]), function(group){
  sapply(BLs, function(BL){ grep(paste0(BL, "_", group, ".vs.", BL ), data$group)}, USE.NAMES = FALSE)
}))

trend_within_group <- data[within_group_rows, ]

prep_list[["trend_whole_group"]] <- prep_trend(data = data[whole_group_rows, ], grouping_var = grouping_var, sig_niveau = sig_niveau)
prep_list[["trend_within_group"]] <- prep_trend(data = data[within_group_rows, ], grouping_var = grouping_var, sig_niveau = sig_niveau)
prep_list[["pointplot"]] <- prep_points(data, grouping_var = grouping_var, sig_niveau = sig_niveau)

  return(prep_list)
}

