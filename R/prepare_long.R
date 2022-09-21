bt21 <- read.csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/Abbildungen/01_KAS/Nicklas/Daten/Abb65Buecher_mitTrend_KOPIE.csv")

wide_bt <- prepare_wide(bt21, competence = "GL")

prepare_long <- function(wide_bt){

  long_bt <- wide_bt %>%
    filter(is.na(comparison)) %>%
    filter(!is.na(KBuecher_imp3)) %>%
    filter(!(group %in% c("0","1"))) %>%
    gather(parameter, estimate, c(est_2011:est_2021,
                                  p_2011:p_2021,
                                  se_2011:se_2021)
    ) %>%
    separate(parameter, c("parameter", "year")) %>%
    spread(parameter, estimate) %>%
    #select(kb, adjust, TR_BUNDESLAND, year, est, p, se )
    mutate(year = as.numeric(year)) %>%
    mutate(sig = ifelse(p < 0.05, "Sig", "noSig"))

  for(i in unique(bt21a$TR_BUNDESLAND)){
    bt21a_long[grepl(i, bt21a_long$group),"TR_BUNDESLAND"] <- i
  }
}
