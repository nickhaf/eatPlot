
#' Coalesce together multiple columns into one.
#'
#' Columns that contain unique values per row can be combined into one. Main usage if multiple different subgroup variables should be plotted in the same column in a plot.
#'
#' @param dat A data frame prepared by [prep_tablebarplot] containing the columns to be merged.
#' @param type A string indicating which column type should be merged (e.g., "est", "se", "sig" ...)
#' @param comparison A string indicating which comparison should be merged (e.g., "groupDiff").
#' @param facet A string indicating which facet should be merged ("sameFacet", "totalFacet").
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
