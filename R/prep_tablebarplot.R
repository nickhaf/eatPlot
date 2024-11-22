prep_tablebarplot <- function(dat, par = "mean") {

dat <- dat$plain
dat <- dat[dat$parameter == par, ]
dat$sig <- ifelse(dat$p < 0.05, TRUE, FALSE)

dat[, -c("parameter", "unit_1", "unit_2", "id")]

  return(dat)
}
