
#' Coalesce together multiple columns into one.
#'
#' Columns that contain unique values per row can be combined into one. Main usage if multiple different subgroup variables should be plotted in the same column in a plot.
#'
#'
#' @return A vector consisting of the merged columns.
#' @export
#'
#' @examples
#' #tbd

merge_cols <- function(dat, type, comparison, facet = "sameFacet"){

  ## Test whether there actually are NAs in every other oclumn, if we haf a value somewhere

  cols_to_merge <- grep(paste0("^", type, "_", ".*", facet, ".*", comparison), names(dat), value = TRUE)
  res <- apply(dat[ , cols_to_merge], 1, function(row) {
    row[!is.na(row)][1]
  })

  return(res)
}
