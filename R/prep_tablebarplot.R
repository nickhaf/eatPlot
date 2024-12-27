prep_tablebarplot <- function(eatRep_dat, par = "mean", facet_var = "TR_BUNDESLAND", total_group = "total") {
check_eatRep_dat(eatRep_dat)

check_columns(eatRep_dat$plain, c("unit_1", "unit_2", "id", "comparison", "parameter", "year", "est", "se", "p", "mhg", facet_var))
eatRep_dat <- rename_comparisons_total(eatRep_dat, total_group, facet_var)

## Dirty Fix: "- total" aus TR_BUNDESLAND entfernen.
eatRep_dat$plain$TR_BUNDESLAND <- gsub(" - total", "", eatRep_dat$plain$TR_BUNDESLAND)
## Gibts hier ein besseres Vorgehen? Ich könnte mir die Daten auch selber zusammenmergen. Aber for now ...


dat <- eatRep_dat$plain
dat <- dat[dat$parameter %in% par, ]
dat$sig <- ifelse(dat$p < 0.05, TRUE, FALSE)

dat <- dat[, !(colnames(dat) %in% c("unit_1", "unit_2", "id"))]

## Hier gibt es jetzt duplicated rows (nur wenn ich - total in Bundesland entferne):
dat_wide <- dat[, c("comparison", "TR_BUNDESLAND", "parameter", "year", "est", "se", "es", "sig", "mhg")] |>
  pivot_wider(names_from = c(year, comparison, parameter), values_from = c(est, se, es, sig))
dat_wide$y_axis <- 1:nrow(dat)

dat_wide <- as.data.frame(dat_wide)

  return(dat_wide)
}
