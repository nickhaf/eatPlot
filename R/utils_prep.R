
















merge_eatRep <- function(eatRep_unnested){

  dat_group <- merge(dat_unnested,
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

  return(dat_group_long_t)
}


prep_comparisons <- function(eatRep_merged, facet_var, total_facet, total_subgroup = NULL) {
  dat_hardest <- eatRep_merged[eatRep_merged$comparison != "trend_crossDiff_of_groupDiff", ]
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

  dat_comp <- do.call(rbind, df_list)

  return(dat_comp)
}



pivot_eatRep <- function(eatRep_prepped){
  eatRep_prepped <- eatRep_prepped[, c("TR_BUNDESLAND", "subgroup_var", "year", "trend", "comparison_split", "est_comparison_none", "est_comparison", "se_comparison_none", "se_comparison")]

  eatRep_prepped_trend <- eatRep_prepped[grep("_", eatRep_prepped$trend), ]
  eatRep_prepped_noTrend <- eatRep_prepped[grep("_", eatRep_prepped$trend, invert = TRUE), ]

  noTrend_wide <- tidyr::pivot_wider(
    unique(subset(eatRep_prepped_noTrend, select = -trend)),
    names_from = c("comparison_split"),
    values_from = c("est_comparison", "se_comparison") #, "se_comparison", "p_comparison", "es_comparison") #  "sig_comparison",
  )

  noTrend_wide_year <-  tidyr::pivot_wider(
    noTrend_wide,
    names_from = c("year"),
    values_from = colnames(noTrend_wide)[grep(c("est_comparison|se_comparison"), colnames(noTrend_wide))] #, "se_comparison", "p_comparison", "es_comparison") #  "sig_comparison",
  )

  trend_wide_year <- tidyr::pivot_wider(
    unique(subset(eatRep_prepped_trend, select =  -c(est_comparison_none, se_comparison_none, year))),
    names_from = c("comparison_split", "trend"),
    values_from = c("est_comparison", "se_comparison") #, "se_comparison", "p_comparison", "es_comparison") #  "sig_comparison",
  )

  trend_wide <- merge(noTrend_wide_year, trend_wide_year)

  return(trend_wide)
}


