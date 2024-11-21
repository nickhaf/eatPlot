prep_tablebarplot <- function(dat, par = "mean", comparisons = NULL) {
  dat$estimate <- subset(
    dat$estimate,
    parameter == par
  )

  dat$group_estimates <- merge(dat$group,
                               dat$estimate,
                               by = "id",
                               all.x = TRUE
  )

  if(!is.null(comparisons)) {
browser()
    dat$comparisons <- dat$comparisons[dat$comparisons$comparison %in% comparisons,]

    dat$comp_estimates <- merge(dat$comparisons,
                                       dat$estimate,
                                       by = "id",
                                       all.x = TRUE)

    dat_long <- tidyr::pivot_longer(
      dat$comp_estimates,
      cols = starts_with("unit"),
      names_to = "unit",
      values_to = "group"
    )

    dat_merged <- merge(dat_long,
                               dat$group_estimates,
                               by.x = "group",
                               by.y = "id",
                               all.x = TRUE,
                               suffixes = c("_comp", "_point"))

  }

  dat$group_estimates$sig <- ifelse(dat$group_estimates$p < 0.05, TRUE, FALSE)

  return(dat$group_estimates)
}
