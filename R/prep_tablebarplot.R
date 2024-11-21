prep_tablebarplot <- function(dat, par = "mean") {
  dat$estimate <- subset(
    dat$estimate,
    parameter == par
  )

  dat$group_estimates <- merge(dat$group,
                               dat$estimate,
                               by = "id",
                               all.x = TRUE
  )

  dat$group_estimates$sig <- ifelse(dat$group_estimates$p < 0.05, TRUE, FALSE)

  return(dat$group_estimates)
}
