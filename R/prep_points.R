bt_data <- trend_books
competence = "GL"
grouping_var = "KBuecher_imp3"


prepare_pointEstimates <- function(data, competence, grouping_var){

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

  ## Filter all rows with the pattern: "BL + _0|_1 . This
  ## are the rows containing the point estimates of each
  ## Bundesland vs. the whole Group (Germany).

  est_rows <- c(filter_strings(identifier = BLs, paste_vec = "_0$", val_vec = data$group),
              filter_strings(identifier = BLs, paste_vec = "_1$", val_vec = data$group))
  data_est <- data[est_rows, c("TR_BUNDESLAND", "group", est_cols)]





  for(i in unique(bt_data$TR_BUNDESLAND)[-is.na(unique(bt_data$TR_BUNDESLAND))]){

    for(j in c("2011", "2016", "2021")){

      bt_data[grepl(i, bt_data$group),"TR_BUNDESLAND"] <- i

      # Eintragen der p-Werte in die entsprechende Zeile
      bt_data[grepl(paste0(i, "_0"), bt_data$group), paste0("p_", j)] <- bt_data[grepl(paste0(i, "_0", ".vs.wholeGroup"), bt_data$group), paste0("p_",j )]
      bt_data[grepl(paste0(i, "_1"), bt_data$group), paste0("p_", j)] <- bt_data[grepl(paste0(i, "_1", ".vs.wholeGroup"), bt_data$group), paste0("p_",j )]

    }
  }

  pointEsts <- bt_data[!is.na(bt_data$TR_BUNDESLAND) | bt_data$group %in% c("0", "1"), ]
  pointEsts <- pointEsts[!is.na(pointEsts[,grouping_var]), ]
  pointEsts <- pointEsts[is.na(pointEsts$comparison), ]

  pointEsts[pointEsts$group %in% c("0","1"),"TR_BUNDESLAND"] <- rep("Deutschland", 2)

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
