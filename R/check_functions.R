check_facets <- function(dat, facets) {

  if(!checkmate::test_factor(dat[, facets], ordered = TRUE)){
    message("Facets will be ordered alphabetically. To enforce a custom order, convert your `facet` variable into an ordered factor.")

  ordered_dat <- dat[order(dat[, facets]), ]
  levels(ordered_dat[, facets]) <- factor(unique(ordered_dat[, facets]), levels = unique(ordered_dat[, facets]), ordered = TRUE)

  }
  return(ordered_dat)

}

check_eatRep_dat <- function(plot_dat){
  checkmate::assert_list(plot_dat, len = 4)
  checkmate::assert_names(names(plot_dat), permutation.of = c("plain", "comparisons", "group", "estimate"))
}


