prep_lineplot <- function(data, grouping_var, competence, sig_niveau = 0.05) {

  colnames(data)[colnames(data) == grouping_var] <- "grouping_var"

  data <- data[data$kb == competence & data$parameter == "mean", ]
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]
  groups <- unique(data$grouping_var[!is.na(data$grouping_var)])


  prep_list <- list()

  ## exctract relevant rows:
  whole_group_rows <- unlist(sapply(groups, function(x) {
    filter_strings(identifier = BLs, paste_vec = paste0("_", x, ".vs.wholeGroup"), val_vec = data$group)
  }, USE.NAMES = FALSE))

  trend_whole_group <- data[whole_group_rows, ]
  trend_whole_group$grouping_var <- get_group(trend_whole_group$group, groups = groups)

  within_group_rows <- unlist(sapply(groups, function(group) {
    sapply(BLs, function(BL) {
      grep(paste0(BL, "_", group, ".vs.", BL), data$group)
    }, USE.NAMES = FALSE)
  }))

  trend_within_group <- data[within_group_rows, ]
  trend_within_group$grouping_var <- get_group(trend_within_group$group, groups = groups)

  prep_list[["trend_whole_group"]] <- prep_trend(data = trend_whole_group, sig_niveau = sig_niveau)
  prep_list[["trend_within_group"]] <- prep_trend(data = trend_within_group, sig_niveau = sig_niveau)
  prep_list[["point_estimates"]] <- prep_points(data, sig_niveau = sig_niveau)

  return(prep_list)
}



# utils

get_group <- function(val_vec, groups){

  val_vec <- gsub("_", "\\.", val_vec)
  group_vec <- strsplit(val_vec, split = "\\.")

  res_vec <- unlist(
    lapply(group_vec, function(x){
    log_vec <- x %in% groups
    if(all(log_vec == FALSE)){res <- NA} else{ res <- x[log_vec] }
    if(length(res) > 1){stop("Multiple groups in your grouping column.")} else{ return(res)}
  })
  )
  return(res_vec)

}

