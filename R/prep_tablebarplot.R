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

  dat_unnested <- unnest_eatRep(eatRep_dat)

  dat_group <- merge(dat,
    eatRep_dat$group,
    all = TRUE,
    by.x = "value",
    by.y = "id"
  )

  dat_group_est <- merge(dat_group,
                        eatRep_dat$estimate[eatRep_dat$estimate$parameter == "mean", c("id", "est", "se", "p", "es")] ,
                        by.x = "value",
                        by.y = "id")

  dat_group_long <- merge(dat_group_est,
                              eatRep_dat$estimate[eatRep_dat$estimate$parameter == "mean", c("id", "est", "se", "p", "es")],
                              by = "id",
                              suffixes = c("_comparison_none", "_comparison"))

  dat_group_long_t <- do.call(rbind, lapply(split(dat_group_long, dat_group_long$id), create_trend))

  unique(trend_3$plain$comparison)
  dat_hardest <- dat_group_long_t[dat_group_long_t$comparison != "trend_crossDiff_of_groupDiff", ]


  id_list <- split(dat_hardest, dat_hardest$id)

df_list <-   lapply(id_list, function(x){

## Split the facet comparisons
  if(length(unique(x$TR_BUNDESLAND)) == 1){
      x$comparison_split <- paste0(x$comparison, "_sameFacet")
    }else if(any(grepl(total_facet, x$TR_BUNDESLAND))){
      x$comparison_split <- paste0(x$comparison, "_totalFacet")
    }

## Split the subgroup comparisons
  if(length(unique(x$subgroup_var)) == 1){
    x$comparison_split <- paste0(x$comparison_split, "_sameSubgroup")
  }else if(any(grepl(total_subgroup, x$subgroup_var))){
    x$comparison_split <- paste0(x$comparison_split, "_totalSubgroup")
  }else{
    possible_subgroups <- unique(x$subgroup_var)

res <- c()
    for(i in 1:nrow(x)){
      res[i] <- possible_subgroups[possible_subgroups != x[i, "subgroup_var"]]
    }


    x$comparison_split <- paste0(x$comparison_split, "_", res, "Subgroup")
  }

# Remove all comparisons that start with total! They are duplicates
if(any(grepl(total_facet, x$TR_BUNDESLAND)) & length(unique(x$TR_BUNDESLAND)) != 1){
  x <- x[x$TR_BUNDESLAND != total_facet, ]
}
if(any(grepl(total_subgroup, x$subgroup_var)) & length(unique(x$subgroup_var)) != 1){
    x <- x[x$subgroup_var != total_subgroup, ]
  }
    return(x)
  })

## maybe thats enough for now, and the merging can just be done with ID first?
## So: I've now established which comparisons are of which type. Now I can just Loose everything but ID and comparison_split and
## then merge again? But I will also need the subgroups. But this might make the merging easier?


dat_comp <- do.call(rbind, df_list)


dat_comp_test <- dat_comp[, c("TR_BUNDESLAND", "subgroup_var", "year", "trend", "comparison_split", "est_comparison_none", "est_comparison", "se_comparison_none", "se_comparison")]

dat_comp_test_trend <- dat_comp_test[grep("_", dat_comp_test$trend), ]
dat_comp_test_noTrend <- dat_comp_test[grep("_", dat_comp_test$trend, invert = TRUE), ]

noTrend_wide <- tidyr::pivot_wider(
  unique(dat_comp_test_noTrend %>% select(- c("trend"))),
  names_from = c("comparison_split"),
  values_from = c("est_comparison", "se_comparison") #, "se_comparison", "p_comparison", "es_comparison") #  "sig_comparison",
)


noTrend_wide_year <-  tidyr::pivot_wider(
  noTrend_wide,
  names_from = c("year"),
    values_from = colnames(noTrend_wide)[grep(c("est_comparison|se_comparison"), colnames(noTrend_wide))] #, "se_comparison", "p_comparison", "es_comparison") #  "sig_comparison",
  )

  trend_wide_year <- tidyr::pivot_wider(
    unique(dat_comp_test_trend %>% select(- c("est_comparison_none", "se_comparison_none", "year"))),
    names_from = c("comparison_split", "trend"),
    values_from = c("est_comparison", "se_comparison") #, "se_comparison", "p_comparison", "es_comparison") #  "sig_comparison",
  )


trend_wide <- merge(noTrend_wide_year, trend_wide_year)

  eatRep_dat$group$year <- as.numeric(eatRep_dat$group$year)

  # Merge Data --------------------------------------------------------------
  check_no_columns(eatRep_dat$estimate, cols = "sig")
  eatRep_dat$estimate$sig <- ifelse(eatRep_dat$estimate$p < sig_niveau, TRUE, FALSE)




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

return(dat_wide)
}
