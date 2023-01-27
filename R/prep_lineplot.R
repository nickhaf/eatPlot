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

  trend_whole <- data[whole_group_rows, ]
  trend_whole$grouping_var <- get_group(trend_whole$group, groups = groups)
  trend_whole_long <- prep_trend(data = trend_whole, sig_niveau = sig_niveau)
  trend_whole_long <- rename_column(trend_whole_long, old = "esttrend", new = "est_trend_whole")
  trend_whole_long <- rename_column(trend_whole_long, old = "sig", new = "sig_trend_whole")


  within_group_rows <- unlist(sapply(groups, function(group) {
    sapply(BLs, function(BL) {
      grep(paste0(BL, "_", group, ".vs.", BL), data$group)
    }, USE.NAMES = FALSE)
  }))

  trend_within <- data[within_group_rows, ]
  trend_within$grouping_var <- get_group(trend_within$group, groups = groups)
  trend_within_long <- prep_trend(data = trend_within, sig_niveau = sig_niveau)
  trend_within_long <- rename_column(trend_within_long, old = "esttrend", new = "est_trend_within")
  trend_within_long <- rename_column(trend_within_long, old = "sig", new = "sig_trend_within")

  point_estiamtes <- prep_points(data, sig_niveau = sig_niveau)
  point_estimates <- rename_column(point_estimates, old = "est", new = "est_point")
  point_estimates <- rename_column(point_estimates, old = "sig", new = "sig_point")


  prep_list[["trend_whole"]] <- trend_whole_long
  prep_list[["trend_within_group"]] <- trend_within_long
  prep_list[["point_estimates"]] <- point_estimates

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

