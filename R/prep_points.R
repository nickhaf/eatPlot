bt_data <- trend_books
competence = "GL"
grouping_var = "KBuecher_imp3"


prep_points <- function(data, competence, grouping_var){

  data <- subset(data, kb == competence & parameter == "mean")

  p_cols <- grep("p_", colnames(data), value = TRUE)
  est_cols <- grep("est", colnames(data), value = TRUE)
  est_cols <- grep("trend", est_cols, invert = TRUE, value = TRUE)
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]

## Filter all rows with the pattern: "BL + _0|_1 + .vs.wholeGroup". This
## are the rows containing the p-Values for the point estimates of each
## Bundesland vs. the whole Group (Germany).
  p_rows <- c(filter_strings(identifier = BLs, paste_vec = paste0("_0", ".vs.wholeGroup"), val_vec = data$group),
              filter_strings(identifier = BLs, paste_vec = paste0("_1", ".vs.wholeGroup"), val_vec = data$group))
  data_p <- data[p_rows, c("TR_BUNDESLAND", "group", p_cols)]

  data_p$group <- gsub(".vs.wholeGroup", "", data_p$group)
  data_p$TR_BUNDESLAND <- data_p$group
  data_p$TR_BUNDESLAND <- gsub("_0", "", data_p$TR_BUNDESLAND)
  data_p$TR_BUNDESLAND <- gsub("_1", "", data_p$TR_BUNDESLAND)

## Filter all rows with the pattern: "BL + _0|_1 . This
## are the rows containing the point estimates of each
## Bundesland vs. the whole Group (Germany).
  est_rows <- c(## Germany:
              grep("^0$", data$group), grep("^1$", data$group),
              filter_strings(identifier = BLs, paste_vec = "_0$", val_vec = data$group),
              filter_strings(identifier = BLs, paste_vec = "_1$", val_vec = data$group))

  data_est <- data[est_rows, c("TR_BUNDESLAND", "group", grouping_var, est_cols)]

  ## Put Deutschland in the respective fields for the respective subgroups.
  data_est[is.na(data_est$TR_BUNDESLAND), "TR_BUNDESLAND"] <- rep("Deutschland", length(which(is.na(data_est$TR_BUNDESLAND))))


  data_wide <- merge(data_est, data_p, by = c("group", "TR_BUNDESLAND"), all.x = TRUE)




## Into Long Format
  pointEsts_long <- pointEsts %>%
    gather(parameter, estimate, c(est_2011:est_2021,
                                  p_2011:p_2021)
    ) %>%
    separate(parameter, c("parameter", "year")) %>%
    spread(parameter, estimate)

  pointEsts_long <- pointEsts_long[,c("kb", "TR_BUNDESLAND", grouping_var, "year", "est", "p")] %>%
    rename(p_vsGermany = p) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(sig_vsGermany = ifelse(p_vsGermany < 0.05, "Sig", "noSig"))

  return(pointEsts_long)

}
