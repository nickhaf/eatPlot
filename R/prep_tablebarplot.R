#' Prepare lineplot data.
#'
#' @inheritParams prep_lineplot
#' @inheritParams plot_lineplot
#'

#' @param facet_var Character string of the variable containing information on groups some of the comparisons are made against. This is needed to decosntruct comparisons like `crossDiff` into `crossDiff` and `crossDiffTotal` (so a crossDiff comparison against the total group). Name might be a bit confusing, but is the same as in `prep_lineplot`. Defaults to `TR_BUNDESLAND`.
#' @param ... Further arguments to be passed through, needed for pivoting the data internally. Only temporary, will be removed.
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
                              total_subgroup = "total",
                              ...) {
  dat_wide <- prep_plot(
    eatRep_dat = eatRep_dat,
    subgroup_var = subgroup_var,
    parameter = parameter,
    facet_var = facet_var,
    total_facet = total_facet,
    sig_niveau = sig_niveau,
    total_subgroup = total_subgroup,
    plot_type = "table",
    ...
  )
  dat_wide$y_axis <- 1:nrow(dat_wide)
  return(as.data.frame(dat_wide))
}
