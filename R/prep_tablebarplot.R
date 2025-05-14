#' Prepare tableplot data.
#'
#' @inheritParams prep_lineplot
#' @inheritParams plot_lineplot
#'

#' @param facet_var Character string of the variable containing information on groups some of the comparisons are made against. This is needed to decosntruct comparisons like `crossDiff` into `crossDiff` and `crossDiffTotal` (so a crossDiff comparison against the total group). Name might be a bit confusing, but is the same as in `prep_lineplot`. Defaults to `TR_BUNDESLAND`.
#' @param parameter_to_colname Logical that indicates whether the parameter should be also pivoted into wide format, or the parameter column should be kept in the long format. Mainly set to `FALSE` for stacked barplots. Defaults to `TRUE`.
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
                              parameter_to_colname = TRUE) {


  if(!parameter_to_colname){
    dat_wide <- prep_plot(
      eatRep_dat = eatRep_dat,
      subgroup_var = subgroup_var,
      parameter = parameter,
      facet_var = facet_var,
      total_facet = total_facet,
      sig_niveau = sig_niveau,
      total_subgroup = total_subgroup,
      plot_type = "table",
      ## Don't put parameter into the column names! Keep it as a own column
      names_from_none = c("year"),
      names_from_comp = c("comparison_split", "trend")
    )
  }else{
    dat_wide <- prep_plot(
      eatRep_dat = eatRep_dat,
      subgroup_var = subgroup_var,
      parameter = parameter,
      facet_var = facet_var,
      total_facet = total_facet,
      sig_niveau = sig_niveau,
      total_subgroup = total_subgroup,
      plot_type = "table"
    )
}

  dat_wide$y_axis <- 1:nrow(dat_wide)
  return(as.data.frame(dat_wide))
}
