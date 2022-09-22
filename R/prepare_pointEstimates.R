prepare_pointEstimates <- function(bt_data, competence, grouping_var){

  for(i in unique(bt_data$TR_BUNDESLAND)[-is.na(unique(bt_data$TR_BUNDESLAND))]){

    for(j in c("2011", "2016", "2021")){

      bt_data[grepl(i, bt_data$group),"TR_BUNDESLAND"] <- i

      # Eintragen der p-Werte in die entsprchende Zeile
      bt_data[grepl(paste0(i, "_0"), bt_data$group), paste0("p_", j)] <- bt_data[grepl(paste0(i, "_0", ".vs.wholeGroup"), bt_data$group), paste0("p_",j )]
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
