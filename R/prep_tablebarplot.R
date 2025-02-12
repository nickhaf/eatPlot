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
                              parameter = "mean",
                              facet_var = "TR_BUNDESLAND",
                              total_facet = "total",
                              sig_niveau = 0.05,
                              total_subgroup = "total") {
  # Check input -------------------------------------------------------------
  check_eatRep_dat(eatRep_dat)
  check_columns(eatRep_dat$estimate, cols = c("p"))

  if (is.null(subgroup_var)) {
    message("Are you sure you don't have a subgroup_var? If you do,  please set it.")
  }

  eatRep_dat$group <- build_column(eatRep_dat$group,
    old = subgroup_var,
    new = "subgroup_var",
    fill_value = "total"
  )
  eatRep_dat$group <- build_column(eatRep_dat$group,
    old = facet_var,
    new = "state_var",
    fill_value = "total"
  )
  eatRep_dat$estimate <- build_column(eatRep_dat$estimate,
    old = "es",
    new = "es",
    fill_value = NA
  )
  eatRep_dat$group <- build_column(eatRep_dat$group,
    old = "year",
    new = "year",
    fill_value = NA
  )
  eatRep_dat$group <- build_column(eatRep_dat$group,
    old = "kb",
    new = "kb",
    fill_value = NA
  )

  check_no_columns(eatRep_dat$estimate, cols = "sig")
  eatRep_dat$estimate$sig <- ifelse(eatRep_dat$estimate$p < sig_niveau, TRUE, FALSE)

  # Filtering ---------------------------------------------------------------
  eatRep_dat$estimate <- eatRep_dat$estimate[eatRep_dat$estimate$parameter %in% parameter, ]

  dat_unnested <- unnest_eatRep(eatRep_dat)
  dat_merged <- merge_eatRep(dat_unnested, eatRep_dat)
  dat_prepped <- prep_comparisons(dat_merged, facet_var, total_facet, total_subgroup)
  dat_wide <- pivot_eatRep(dat_prepped, names_from)
  dat_wide <- dat_wide[order(dat_wide$state_var), ]
  dat_wide <- dat_wide[, colSums(!is.na(dat_wide)) > 0]
  dat_wide$y_axis <- 1:nrow(dat_wide)
  return(as.data.frame(dat_wide))
}
