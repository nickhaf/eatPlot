unnest_eatRep <- function(eatRep_dat) {
  if(nrow(eatRep_dat$comparisons) == 0){
    eatRep_out <- data.frame("id" = eatRep_dat$group[,"id"],
                             "comparison" = "",
                             "unit" = NA,
                             "value" = eatRep_dat$group[,"id"])

    return(eatRep_out)
  }

  comp_long <- tidyr::pivot_longer(eatRep_dat$comparisons,
                                   cols = c("unit_1", "unit_2"),
                                   names_to = "unit")

  comp_long_noComps <- comp_long[grep("comp_", comp_long$value, invert = TRUE), ] ## these are done, rbind!
  comp_long_comps <- comp_long[grep("comp_", comp_long$value),  ]


  while(length(grep("comp_", comp_long_comps$value)) > 0){
    comp_long_m <- merge(comp_long_comps,
                         eatRep_dat$comparisons[, c("id", "unit_1", "unit_2")],
                         by.x = "value",
                         by.y = "id",
                         all.x = TRUE)
    comp_long_m$unit <- gsub("unit_", "", comp_long_m$unit)

    comp_long_comps_l <- tidyr::pivot_longer(comp_long_m[, c("id", "comparison", "unit", "unit_1", "unit_2" )],
                                             cols = c("unit_1", "unit_2"))

    comp_long_comps_l$unit <- paste(comp_long_comps_l$unit, gsub("unit_", "", comp_long_comps_l$name), sep = ".")

    comp_long_comps <- comp_long_comps_l[grep("comp_", comp_long_comps_l$value),  ]
    comp_long_noComps <- rbind(comp_long_noComps,
                               comp_long_comps_l[grep("comp_", comp_long_comps_l$value, invert = TRUE), c("id", "comparison", "unit", "value")])
  }


  return(comp_long_noComps)

}

merge_eatRep <- function(eatRep_unnested, eatRep_dat){
  dat_group <- merge(eatRep_unnested,
                     eatRep_dat$group,
                     all = TRUE,
                     by.x = "value",
                     by.y = "id"
  )

  dat_group_est <- merge(dat_group,
                         eatRep_dat$estimate[eatRep_dat$estimate$parameter == "mean", c("id", "est", "se", "p", "es", "sig")] ,
                         by.x = "value",
                         by.y = "id")

  dat_group_long <- merge(dat_group_est,
                          eatRep_dat$estimate[eatRep_dat$estimate$parameter == "mean", c("id", "est", "se", "p", "es", "sig")],
                          by = "id",
                          suffixes = c("_comparison_none", "_comparison"))
  if(nrow(eatRep_dat$comparisons) == 0){
  dat_group_long[, grep("_comparison$", colnames(dat_group_long))] <- NA
  }

  dat_group_long_t <- do.call(rbind, lapply(split(dat_group_long, dat_group_long$id), create_trend))

  return(dat_group_long_t)
}

prep_comparisons <- function(eatRep_merged, facet_var, total_facet, total_subgroup = NULL) {

  dat_hardest <- eatRep_merged[eatRep_merged$comparison != "trend_crossDiff_of_groupDiff", ]
  id_list <- split(dat_hardest, dat_hardest$id)

  df_list <-   lapply(id_list, function(x){

    ## Split the facet comparisons
    if(length(unique(x$state_var)) == 1){
      x$comparison_split <- paste0(x$comparison, "_sameFacet")
    }else if(any(grepl(total_facet, x$state_var))){
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
    if(any(grepl(total_facet, x$state_var)) & length(unique(x$state_var)) != 1){
      x <- x[x$state_var != total_facet, ]
    }
    if(any(grepl(total_subgroup, x$subgroup_var)) & length(unique(x$subgroup_var)) != 1){
      x <- x[x$subgroup_var != total_subgroup, ]
    }
    return(x)
  })

  dat_comp <- do.call(rbind, df_list)

  return(dat_comp)
}

pivot_eatRep <- function(eatRep_prepped, names_from){

  eatRep_prepped <- eatRep_prepped[, colnames(eatRep_prepped) %in% c("state_var", "subgroup_var", "kb", "year", "trend") | grepl("comparison", colnames(eatRep_prepped))]

  eatRep_prepped_trend <- eatRep_prepped[grep("_", eatRep_prepped$trend), ]
  eatRep_prepped_noTrend <- eatRep_prepped[grep("_", eatRep_prepped$trend, invert = TRUE), ]

  noTrend_wide <- tidyr::pivot_wider(
    unique(subset(eatRep_prepped_noTrend, select = -c(trend, comparison))),
    names_from = c("comparison_split"),
    values_from = c("est_comparison", "se_comparison", "p_comparison", "es_comparison", "sig_comparison")
  )

  noTrend_wide_year <-  tidyr::pivot_wider(
    noTrend_wide,
    names_from = c("year"),
    values_from = colnames(noTrend_wide)[grep(c("est_comparison|se_comparison|p_comparison|es_comparison|sig_comparison"), colnames(noTrend_wide))]
  )

  if(nrow(eatRep_prepped_trend) > 0){
trend_wide_year <- tidyr::pivot_wider(
    unique(eatRep_prepped_trend[, c("state_var", "subgroup_var", "kb", "trend", "comparison_split", "est_comparison", "se_comparison", "sig_comparison", "p_comparison", "es_comparison")]),
    names_from = names_from,
    values_from =  c("est_comparison", "se_comparison", "p_comparison", "es_comparison", "sig_comparison")
  )

  trend_wide <- merge(noTrend_wide_year, trend_wide_year)
}else{
  trend_wide <- noTrend_wide_year[, c("state_var", "subgroup_var", "kb", colnames(noTrend_wide_year)[grepl("_comparison", colnames(noTrend_wide_year)) ])]
}

  return(trend_wide)
}


