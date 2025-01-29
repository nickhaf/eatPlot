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
  eatRep_dat$plain <- build_column(eatRep_dat$plain, old = subgroup_var, new = "subgroup_var", fill_value = "total") ## set to total so the background_line can be plotted



  # Filtering ---------------------------------------------------------------
  if (!is.null(comparisons)) {
    eatRep_dat$comparisons <- eatRep_dat$comparisons[eatRep_dat$comparisons$comparison %in% c(comparisons, paste0(comparisons, "Total")), ]
  }

  dat <- unnest_eatRep2(eatRep_dat)

  dat_group <- merge(dat,
                     eatRep_dat$group,
                     all = TRUE,
                     by.x = "value",
                     by.y = "id")

  ## Okay, what happens if we don't have any comps? Check that!

  ## Gruppieren nach id
  ## Umbennen der comparisons as needed:
  ## - remove all comparisons with a total in 1.1.1 or 1.1 or unit_1
  ## - Build 2 new columns: comparison_specifications
  ## - all groups that stil have a total somewhere then get a _total at the end of the compar


  remove_ids <- dat_group[dat_group$unit %in% c("unit_1", "1.1", "1.1.1") &
                            dat_group$TR_BUNDESLAND == "total", "id"]


 dat_group_r <- dat_group[!(dat_group$id %in% remove_ids),]

  ## remove duplicates, for example because of total group, which would be overrepresented otherwise.
## They should be duplicated, but check that later. Alternatively, the total stuff has to be prepared seperatly and rbinded?
 dat_hardest <- dat_group[dat_group$comparison == "trend_crossDiff_of_groupDiff", ]

id_list <- split( dat_hardest[ dat_hardest$comparison == "trend_crossDiff_of_groupDiff", ],  dat_hardest$id)
## this contains the comparisons
browser()
## deal with that! How to proceed?

lapply(id_list, function(x){
  if(unique(x$TR_BUNDESLAND) > 1)
})



## merge with estimates: once for the subgroups, and once for the id (so the comparison estimates)
  ## pivot wider everything like before



  eatRep_dat <- rename_comparisons_total(eatRep_dat,
                                         facet_var = facet_var,
                                         total_facet = total_facet,
                                         total_subgroup = total_subgroup
  )
  eatRep_dat$plain <- NULL
  eatRep_dat$estimate <- eatRep_dat$estimate[eatRep_dat$estimate$parameter == parameter, ]

  eatRep_dat$group$year <- as.numeric(eatRep_dat$group$year)

  # Merge Data --------------------------------------------------------------
  check_no_columns(eatRep_dat$estimate, cols = "sig")
  eatRep_dat$estimate$sig <- ifelse(eatRep_dat$estimate$p < sig_niveau, TRUE, FALSE)

  built_plot_dat <- build_plot_dat(eatRep_dat, facet_var, total_facet, total_subgroup)

  noTrend_wide <- tidyr::pivot_wider(
    unique(noTrend[, !colnames(noTrend) %in% c("group", "id")]),
    names_from = "comparison",
    values_from = c("est_comparison", "se_comparison", "sig_comparison", "p_comparison", "es_comparison")
  ) |>
    as.data.frame()

  trend_wide <- tidyr::pivot_wider(
    unique(trend[, !colnames(trend) %in% c("group", "id")]),
    names_from = "comparison",
    values_from = c("est_comparison", "se_comparison", "sig_comparison", "p_comparison", "es_comparison")
  ) |>
    as.data.frame()

  plot_dat <- merge(
    noTrend_wide[, !colnames(noTrend_wide) %in% c("group", "id", "trend")],
    trend_wide,
    all = TRUE
  )


























# old, using plain data frame ---------------------------------------------

  check_eatRep_dat(eatRep_dat)

  if (is.null(subgroup_var)) {
    message("Are you sure you don't have a subgroup_var? If you do,  please set it.")
  }

  eatRep_dat$group <- build_column(eatRep_dat$group, old = subgroup_var, new = "subgroup_var", fill_value = "total") ## set to total so the background_line can be plotted
  eatRep_dat$plain <- build_column(eatRep_dat$plain, old = subgroup_var, new = "subgroup_var", fill_value = "total") ## set to total so the background_line can be plotted


  if (nrow(eatRep_dat$comparison) > 0) {
    check_columns(eatRep_dat$plain, c("unit_1", "unit_2", "id", "comparison", "parameter", "year", "est", "se", "p", facet_var, subgroup_var))

    eatRep_dat <- rename_comparisons_total(eatRep_dat, facet_var, total_facet) ## don't call it facet var.
    # its the variable where the comparison is made against

    ## Dirty Fix: "- total" aus TR_BUNDESLAND entfernen.
    eatRep_dat$plain$TR_BUNDESLAND <- gsub(" - total", "", eatRep_dat$plain$TR_BUNDESLAND)
    ## Gibts hier ein besseres Vorgehen? Ich könnte mir die Daten auch selber zusammenmergen. Aber for now ...

    dat <- eatRep_dat$plain

    if (!is.null(par)) {
      dat <- dat[dat$parameter %in% par, ]
    }
    if (!is.null(comparisons)) {
      dat <- dat[dat$comparison %in% c(comparisons, paste0(comparisons, "Total")), ]
    }
    dat$sig <- ifelse(dat$p < sig_niveau, TRUE, FALSE)

    dat <- dat[, !(colnames(dat) %in% c("unit_1", "unit_2", "id"))]
    dat_wide <- dat[, colnames(dat) %in% c("comparison", "depVar", facet_var, "domain", "parameter", "year", "est", "p", "se", "es", "sig", subgroup_var)] |>
      unique() |>
      tidyr::pivot_wider(names_from = tidyr::all_of(names_from), values_from = c("est", "se", "es", "sig", "p"))


    dat_wide$y_axis <- 1:nrow(dat_wide)

    dat_wide <- as.data.frame(dat_wide)
  } else {
    dat <- eatRep_dat$plain
    if (!is.null(par)) {
      dat <- dat[dat$parameter %in% par, ]
    }
    dat$sig <- ifelse(dat$p < sig_niveau, TRUE, FALSE)
    dat <- dat[, colnames(dat) %in% c("depVar", "comparison", "domain", facet_var, "parameter", "year", "est", "p", "se", "es", "sig", subgroup_var)] |>
      unique()
    dat$y_axis <- 1:nrow(dat)
    return(dat)
  }
  return(dat_wide)
}



