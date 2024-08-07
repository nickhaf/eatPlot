check_eatRep_dat <- function(plot_dat){
  checkmate::assert_list(plot_dat, len = 4)
  checkmate::assert_names(names(plot_dat), permutation.of = c("plain", "comparisons", "group", "estimate"))
}


