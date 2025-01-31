#' Prepare lineplot data.
#'
#' @inheritParams prep_lineplot
#' @inheritParams plot_lineplot
#'
#' @param par Character vector of the parameters that should be used for the data preperation. Defaults to `mean`.
#' @param names_from Character vector of the variables that should be used to create the wide format. Defaults to `c("year", "comparison", "parameter")`.
#' @param facet_var Character string of the variable containing information on groups some of the comparisons are made against. This is needed to decosntruct comparisons like `crossDiff` into `crossDiff` and `crossDiffTotal` (so a crossDiff comparison against the total group). Name might be a bit confusing, but is the same as in `prep_lineplot`. Defaults to `TR_BUNDESLAND`.
#'
#' @return Data prepared for plotting the BT-lineplots.
#' @export
#'
#' @examples # tbd
prep_tablebarplot <- function(eatRep_dat,
                              subgroup_var = NULL,
                              names_from = c("year", "comparison", "parameter"),
                              parameter = "mean",
                              facet_var = "TR_BUNDESLAND",
                              total_facet = "total",
                              comparisons = NULL,
                              sig_niveau = 0.05,
                              total_subgroup = "total") {
  # Check input -------------------------------------------------------------
  check_eatRep_dat(eatRep_dat)
  check_columns(eatRep_dat$estimate, cols = c("p"))

  if (is.null(subgroup_var)) {
    message("Are you sure you don't have a subgroup_var? If you do,  please set it.")
  }

  eatRep_dat$group <- build_column(eatRep_dat$group, old = subgroup_var, new = "subgroup_var", fill_value = "total") ## set to total so the background_line can be plotted
  check_no_columns(eatRep_dat$estimate, cols = "sig")
  eatRep_dat$estimate$sig <- ifelse(eatRep_dat$estimate$p < sig_niveau, TRUE, FALSE)

  # Filtering ---------------------------------------------------------------
  if (!is.null(comparisons)) {
    eatRep_dat$comparisons <- eatRep_dat$comparisons[eatRep_dat$comparisons$comparison %in% comparisons, ]
  }

  dat_unnested <- unnest_eatRep(eatRep_dat)
  dat_merged <- merge_eatRep(dat_unnested)
  dat_prepped <- prep_comparisons(dat_merged, facet_var, total_facet, total_subgroup)
  dat_wide <- pivot_eatRep(dat_prepped)


  if (nrow(eatRep_dat$comparison) > 0) {
    check_columns(eatRep_dat$plain, c("unit_1", "unit_2", "id", "comparison", "parameter", "year", "est", "se", "p", facet_var, subgroup_var))


    if (!is.null(par)) {
      dat <- dat[dat$parameter %in% par, ]
    }
    if (!is.null(comparisons)) {
      dat <- dat[dat$comparison %in% c(comparisons, paste0(comparisons, "Total")), ]
    }
    dat$sig <- ifelse(dat$p < sig_niveau, TRUE, FALSE)


    ## make argument so users can decide whether year etc. should be in the column names
    dat_wide <- dat[, colnames(dat) %in% c("comparison", "depVar", facet_var, "domain", "parameter", "year", "est", "p", "se", "es", "sig", subgroup_var)] |>
      unique() |>
      tidyr::pivot_wider(names_from = tidyr::all_of(names_from), values_from = c("est", "se", "es", "sig", "p"))

    dat_wide$y_axis <- 1:nrow(dat_wide)
}
return(dat_wide)
}
