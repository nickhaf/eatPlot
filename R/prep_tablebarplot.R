prep_tablebarplot <- function(eatRep_dat, par = "mean", facet_var = "TR_BUNDESLAND", total_group = "total") {
check_eatRep_dat(eatRep_dat)

check_columns(eatRep_dat$plain, c("unit_1", "unit_2", "id", "comparison", "parameter", "year", "est", "se", "p", "mhg"))
eatRep_dat <- rename_comparisons_total(eatRep_dat, total_group, facet_var)


dat <- eatRep_dat$plain
dat <- dat[dat$parameter %in% par, ]
dat$sig <- ifelse(dat$p < 0.05, TRUE, FALSE)

dat <- dat[, !(colnames(dat) %in% c("unit_1", "unit_2", "id"))]
dat <- dat[, c("comparison", "TR_BUNDESLAND", "parameter", "year", "est", "se", "es", "sig", "mhg")] |>
  pivot_wider(names_from = c(year, comparison, parameter), values_from = c(est, se, es, sig))
dat$y_axis <- 1:nrow(dat)

dat <- as.data.frame(dat)

  return(dat)
}
