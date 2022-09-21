bt21 <- read.csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/Abbildungen/01_KAS/Nicklas/Daten/Abb65Buecher_mitTrend_KOPIE.csv", na.strings = "")

prepare_long <- function(wide_bt, grouping_var){

  data_long <- wide_bt %>%
  # filter(is.na(comparison)) %>%
  # filter(!is.na(.[[grouping_var]])) %>%
  # filter(!(group %in% c("0","1"))) %>%
    gather(parameter, estimate, c(est_2011:est_2021,
                                  p_2011:p_2021,
                                  se_2011:se_2021)
    ) %>%
    separate(parameter, c("parameter", "year")) %>%
    spread(parameter, estimate) %>%
    mutate(parameter = case_when(is.na(comparison) ~ "mean_group",
                                 ))


  data_long <- data_long[,c("kb", "TR_BUNDESLAND", grouping_var, "year", "est", "p")] %>%
    rename(p_est = p) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(sig = ifelse(p_est < 0.05, "Sig", "noSig"))

  return(data_long)
}
