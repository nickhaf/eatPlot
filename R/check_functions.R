check_facets <- function(vec) {

  if(!checkmate::test_factor(vec, ordered = TRUE)){

    message("Facets will be ordered alphabetically. To enforce a custom order, convert your `facet` variable into an ordered factor, see https://campus.datacamp.com/courses/introduction-to-r-for-finance/factors-4?ex=8")

  ordered_vec <- vec[order(vec)]
  vec <- factor(ordered_vec, levels = unique(ordered_vec), ordered = TRUE)

  }
  return(vec)

}

check_eatRep_dat <- function(plot_dat){
  checkmate::assert_list(plot_dat, len = 4)
  checkmate::assert_names(names(plot_dat), permutation.of = c("plain", "comparisons", "group", "estimate"))
}


check_years <- function(vec, l = years_lines, b = years_braces){
  if(!checkmate::test_subset(paste_trend_years(l), choices = vec)){
    stop("Some of the trends you provided in 'years_lines' are not in the data.")
  }

  if(!checkmate::test_subset(paste_trend_years(b), choices = vec)){
    stop("Some of the trends you provided in 'years_braces' are not in the data.")
  }
}
