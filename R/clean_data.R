
clean_data <- function(data, grouping_var, competence){

  if (grouping_var == "") {
    data$grouping_var <- rep(NA, nrow(data))
  } else {
    colnames(data)[colnames(data) == grouping_var] <- "grouping_var"
  }
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]
  data$TR_BUNDESLAND <- write_group(data$group, groups = BLs)
  data[is.na(data$TR_BUNDESLAND) & get_wholeGroup(data$group), "TR_BUNDESLAND"] <- "wholeGroup"
  data[is.na(data$grouping_var), "grouping_var"] <- "noGroup"

  colnames(data) <- gsub("\\.", "_", colnames(data))
  colnames(data) <- gsub("sig_", "p_", colnames(data))
  data <- data[data$kb == competence & data$parameter == "mean", ]
  data <- data[ , !colnames(data) %in% c("modus","depVar", "modus", "parameter", "kb")]

}
