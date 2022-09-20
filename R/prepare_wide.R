bt21 <- read_csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/Abbildungen/01_KAS/Nicklas/Daten/Abb65Buecher_mitTrend_KOPIE.csv")


prepare_wide <- function(bt_data, competence){

  colnames(bt_data) <- gsub("sig", "p", colnames(bt_data))
  bt_data <- bt_data[bt_data$parameter == "mean", ]
  bt_data <- bt_data[bt_data$kb == competence, ]

  for(i in unique(bt_data$TR_BUNDESLAND)[!is.na(unique(bt_data$TR_BUNDESLAND))]){
    for(j in c("2011", "2016", "2021")){
      bt_data[grepl(i, bt_data$group),"TR_BUNDESLAND"] <- i
      # Eintragen der p-Werte in die entsprchende Zeile
      bt_data[grepl(paste0(i, "_0"), bt_data$group), paste0("p_", j)] <- bt_data[grepl(paste0(i, "_0", ".vs.wholeGroup"), bt_data$group), paste0("p_",j )]
    }
  }

  bt_data <- bt_data %>%
    filter(!is.na(TR_BUNDESLAND) | group %in% c("0", "1"))

  return(bt_data)

}
