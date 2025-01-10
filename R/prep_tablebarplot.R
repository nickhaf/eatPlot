#' Prepare lineplot data.
#'
#' @inheritParams prep_lineplot
#'
#' @return Data prepared for plotting the BT-lineplots.
#' @export
#'
#' @examples # tbd
prep_tablebarplot <- function(eatRep_dat, subgroup_var, names_from =  c("year", "comparison", "parameter"), par = "mean", facet_var = "TR_BUNDESLAND", total_group = "total", comparisons = NULL) {
check_eatRep_dat(eatRep_dat)

check_columns(eatRep_dat$plain, c("unit_1", "unit_2", "id", "comparison", "parameter", "year", "est", "se", "p", facet_var, subgroup_var))

eatRep_dat <- rename_comparisons_total(eatRep_dat, total_group, facet_var)

## Dirty Fix: "- total" aus TR_BUNDESLAND entfernen.
eatRep_dat$plain$TR_BUNDESLAND <- gsub(" - total", "", eatRep_dat$plain$TR_BUNDESLAND)
## Gibts hier ein besseres Vorgehen? Ich könnte mir die Daten auch selber zusammenmergen. Aber for now ...

dat <- eatRep_dat$plain
dat <- dat[dat$parameter %in% par, ]
if(!is.null(comparisons)){
dat <- dat[dat$comparison %in% c(comparisons, paste0(comparisons, "Total")), ]
}
dat$sig <- ifelse(dat$p < 0.05, TRUE, FALSE)

dat <- dat[, !(colnames(dat) %in% c("unit_1", "unit_2", "id"))]

dat_wide <- dat[, c("comparison", "TR_BUNDESLAND", "parameter", "year", "est", "se", "es", "sig", subgroup_var)] |>
  unique() |>
  tidyr::pivot_wider(names_from = names_from, values_from = c(est, se, es, sig))


dat_wide$y_axis <- 1:nrow(dat_wide)

dat_wide <- as.data.frame(dat_wide)

  return(dat_wide)
}
