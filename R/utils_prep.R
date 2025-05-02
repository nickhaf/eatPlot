prep_plot <- function(
    eatRep_dat,
    subgroup_var = NULL,
    parameter = "mean",
    facet_var = "TR_BUNDESLAND",
    total_facet = "total",
    sig_niveau = 0.05,
    total_subgroup = "total",
    names_from_none = c("year", "parameter_comp_none"),
    names_from_comp = c("comparison_split", "trend", "parameter_comp"),
    plot_type = c("table", "line")) {

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

  eatRep_dat$estimate$sig <- ifelse(eatRep_dat$estimate$p < sig_niveau & !is.na(eatRep_dat$estimate$p), TRUE, FALSE)

  # Filtering ---------------------------------------------------------------
  merge_by <- c("id", "parameter")
  if(!is.null(parameter)){
    eatRep_dat$estimate <- eatRep_dat$estimate[eatRep_dat$estimate$parameter %in% parameter, ]
    merge_by <- c("id")
  }

  dat_unnested <- unnest_eatRep(eatRep_dat)

  ## Don't loose any comparisons!
  no_comp_anywhere <- data.frame(id = 1:nrow(eatRep_dat$group),
                                 comparison = "none",
                                 unit = "unit_1",
                                 value = eatRep_dat$group[!eatRep_dat$group$id %in% dat_unnested$value, "id" ])


  dat_unnested <- rbind(dat_unnested, no_comp_anywhere)

  dat_merged <- merge_eatRep(dat_unnested, eatRep_dat, by = merge_by)
  dat_prepped <- prep_comparisons(dat_merged, facet_var, total_facet, total_subgroup)

  dat_wide <- pivot_eatRep(dat_prepped,
                           names_from_none = names_from_none,
                           names_from_comp = names_from_comp,
                           plot_type = plot_type)
  dat_wide <- dat_wide[order(dat_wide$state_var), ]
  dat_wide <- dat_wide[, colSums(!is.na(dat_wide)) > 0]
}


unnest_eatRep <- function(eatRep_dat) {

  if (nrow(eatRep_dat$comparisons) == 0) {
    eatRep_out <- data.frame(
      "id" = eatRep_dat$group[, "id"],
      "comparison" = "",
      "unit" = NA,
      "value" = eatRep_dat$group[, "id"]
    )

    return(eatRep_out)
  }

  comp_long <- tidyr::pivot_longer(eatRep_dat$comparisons,
    cols = c("unit_1", "unit_2"),
    names_to = "unit"
  )

  comp_long_noComps <- comp_long[grep("comp_", comp_long$value, invert = TRUE), ] ## these are done, rbind!
  comp_long_comps <- comp_long[grep("comp_", comp_long$value), ]

  while (length(grep("comp_", comp_long_comps$value)) > 0) {
    comp_long_m <- merge(comp_long_comps,
      eatRep_dat$comparisons[, c("id", "unit_1", "unit_2")],
      by.x = "value",
      by.y = "id",
      all.x = TRUE
    )
    comp_long_m$unit <- gsub("unit_", "", comp_long_m$unit)

    comp_long_comps_l <- tidyr::pivot_longer(comp_long_m[, c("id", "comparison", "unit", "unit_1", "unit_2")],
      cols = c("unit_1", "unit_2")
    )

    comp_long_comps_l$unit <- paste(comp_long_comps_l$unit, gsub("unit_", "", comp_long_comps_l$name), sep = ".")

    comp_long_comps <- comp_long_comps_l[grep("comp_", comp_long_comps_l$value), ]
    comp_long_noComps <- rbind(
      comp_long_noComps,
      comp_long_comps_l[grep("comp_", comp_long_comps_l$value, invert = TRUE), c("id", "comparison", "unit", "value")]
    )
  }


  return(comp_long_noComps)
}

merge_eatRep <- function(eatRep_unnested, eatRep_dat, by, stacked = TRUE) {

  dat_group <- merge(eatRep_unnested,
    eatRep_dat$group,
    all = TRUE,
    by.x = "value",
    by.y = "id"
  )


  dat_group_est <- merge(dat_group,
    eatRep_dat$estimate[, c("id", "est", "se", "p", "es", "sig", "parameter")],
    by.x = "value",
    by.y = "id"
  )

  ## Up to here totao_subgroup is still in it, IF total_subgroup is set to "" outside


  ## Ncases gets removed here, bad?
  if(stacked){
    dat_group_long <- merge(dat_group_est,
                            eatRep_dat$estimate[, c("id", "est", "se", "p", "es", "sig", "parameter")],
                            by = by,
                            suffixes = c("_comp_none", "_comp"),
                            all = TRUE
    )

    dat_group_long <- dat_group_long[!is.na(dat_group_long$subgroup_var), ]

  }else{
    dat_group_long <- merge(dat_group_est,
                            eatRep_dat$estimate[, c("id", "est", "se", "p", "es", "sig", "parameter")],
                            by = by,
                            suffixes = c("_comp_none", "_comp")
    )

  }

  if (nrow(eatRep_dat$comparisons) == 0) {
    dat_group_long[, grep("_comp$", colnames(dat_group_long))] <- NA
  }
  dat_group_long_t <- do.call(rbind, lapply(split(dat_group_long, dat_group_long$id), create_trend))

  return(dat_group_long_t)
}

prep_comparisons <- function(eatRep_merged, facet_var, total_facet, total_subgroup = NULL) {

  #dat_hardest <- eatRep_merged[eatRep_merged$comparison != "trend_crossDiff_of_groupDiff", ]
  dat_hardest <- eatRep_merged
  id_list <- split(dat_hardest, dat_hardest$id)

  df_list <- lapply(id_list, function(x) {
    ## Split the facet comparisons
    if (length(unique(x$state_var)) == 1) {
      x$comparison_split <- paste0(x$comparison, "_sameFacet")
    } else if (any(grepl(total_facet, x$state_var))) {
      x$comparison_split <- paste0(x$comparison, "_totalFacet")
    }

    ## Split the subgroup comparisons
    if (length(unique(x$subgroup_var)) == 1) {
      x$comparison_split <- paste0(x$comparison_split, "_sameSubgroup")
    } else if (any(grepl(total_subgroup, x$subgroup_var))) {
      x$comparison_split <- paste0(x$comparison_split, "_totalSubgroup")
    } else {
      possible_subgroups <- unique(x$subgroup_var)

      res <- c()
      for (i in 1:nrow(x)) {
        res[i] <- possible_subgroups[possible_subgroups != x[i, "subgroup_var"]]
      }
      x$comparison_split <- paste0(x$comparison_split, "_", res, "Subgroup")
    }

    # Remove all comparisons that start with total! They are duplicates
    if (any(grepl(total_facet, x$state_var)) & length(unique(x$state_var)) != 1) {
      x <- x[x$state_var != total_facet, ]
    }
    if (any(grepl(total_subgroup, x$subgroup_var)) & length(unique(x$subgroup_var)) != 1) {
      x <- x[x$subgroup_var != total_subgroup, ]
    }

    return(x)
  })

  dat_comp <- do.call(rbind, df_list)
  return(dat_comp)
}

pivot_eatRep <- function(eatRep_prepped,
                         names_from_none = c("year", "parameter_comp_none"),
                         names_from_comp = c("comparison_split", "trend", "parameter_comp"),
                         plot_type = c("line", "table")) {

  value_cols <- c("est_comp", "se_comp", "p_comp", "es_comp", "sig_comp")

  eatRep_prepped <- eatRep_prepped[, colnames(eatRep_prepped) %in% c("state_var", "subgroup_var", "kb", "year", "trend") | grepl("comp|parameter", colnames(eatRep_prepped))]

  ## If parameter is not put into column names, it has to be filtered so we have unique rows for pivoting
  if(!grepl("parameter", names_from_none)){
    eatRep_prepped_none <- eatRep_prepped[, colnames(eatRep_prepped) %in% c("state_var", "subgroup_var", "kb", "year", "trend", "parameter") | grepl("comp_none", colnames(eatRep_prepped))]
    eatRep_prepped_comp <- eatRep_prepped[, colnames(eatRep_prepped) %in% c("state_var", "subgroup_var", "kb", "year", "trend", "comparison_split", "parameter") | grepl("comp$", colnames(eatRep_prepped))]

   }else{
     eatRep_prepped_none <- eatRep_prepped[, colnames(eatRep_prepped) %in% c("state_var", "subgroup_var", "kb", "year", "trend") | grepl("comp_none", colnames(eatRep_prepped))]
     eatRep_prepped_comp <- eatRep_prepped[, colnames(eatRep_prepped) %in% c("state_var", "subgroup_var", "kb", "year", "trend", "comparison_split") | grepl("comp$", colnames(eatRep_prepped))]
        }


  eatRep_prepped_none$trend <- NULL

  ## Hier muss parameter in diesem Fall auch rein, damit Identifikation stimmt!!
  eatRep_prepped_none_wide <- tidyr::pivot_wider(
    unique(eatRep_prepped_none),
    names_from = tidyr::all_of(names_from_none),
    values_from = paste0(value_cols, "_none")
  )

  if(plot_type == "line"){
  eatRep_comp_trend <- subset(eatRep_prepped_comp, grepl("_", eatRep_prepped_comp$trend))
  eatRep_comp_noTrend <- subset(eatRep_prepped_comp, !grepl("_", eatRep_prepped_comp$trend))
  eatRep_comp_noTrend$trend <- NULL

  }else{
    eatRep_comp_trend <- subset(eatRep_prepped_comp, grepl("_", eatRep_prepped_comp$trend))
    eatRep_comp_trend$year <- NULL
    eatRep_comp_noTrend <- subset(eatRep_prepped_comp, !grepl("_", eatRep_prepped_comp$trend))
    eatRep_comp_noTrend$year <- NULL
  }

  eatRep_comp_trend_wide <- tidyr::pivot_wider(
    unique(eatRep_comp_trend),
    names_from = tidyr::all_of(names_from_comp),
    values_from = tidyr::all_of(value_cols)
  )

  eatRep_comp_noTrend_wide <- tidyr::pivot_wider(
    unique(eatRep_comp_noTrend),
    names_from = tidyr::all_of(names_from_comp),
    values_from = tidyr::all_of(value_cols)
  )

  eatPlot_dat_noTrend <- merge(eatRep_prepped_none_wide, eatRep_comp_noTrend_wide, all.x = TRUE)
eatPlot_dat <- merge(eatPlot_dat_noTrend, eatRep_comp_trend_wide, all.x = TRUE)

  colnames(eatPlot_dat)[grep("_comp_", colnames(eatPlot_dat))] <- vapply(colnames(eatPlot_dat)[grep("_comp_", colnames(eatPlot_dat))], function(col) {
    parameter <- unlist(regmatches(col, gregexpr("[^_]+$", col)))
    col <- gsub(paste0("_", parameter, "$"), "", col)
    col <- sub("_comp_", paste0("_", parameter, "_comp_"), col)
    return(col)
  },
  FUN.VALUE = character(1)
  )


  return(eatPlot_dat)
}
