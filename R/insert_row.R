#' Insert a row above a given index, preserving column types
#'
#' @param dat A data.frame.
#' @param above Integer scalar: insert *before* this row (1..nrow(dat)+1).
#' @param cols Character vector of column names to fill in the new row.
#' @param values List/atomic vector of values matching `cols` (same length).
#'
#' @return The input data.frame with one additional row.
#' @export
insert_row <- function(dat, above, cols = character(), values = list()) {
  stopifnot(is.data.frame(dat))
  if (!is.numeric(above) || length(above) != 1 || is.na(above)) {
    stop("`above` must be a single numeric index.")
  }
  if (above < 1 || above > nrow(dat) + 1) {
    stop("`above` must be between 1 and nrow(dat)+1.")
  }
  if (length(cols) != length(values)) {
    stop("`cols` and `values` must have the same length.")
  }
  if (length(cols) && !all(cols %in% names(dat))) {
    miss <- setdiff(cols, names(dat))
    stop("Unknown column(s) in `cols`: ", paste(miss, collapse = ", "))
  }

  # Build a 1-row template that preserves column types
  new_row <- as.data.frame(
    stats::setNames(lapply(dat, function(col) {
      if (is.factor(col))    return(factor(NA, levels = levels(col)))
      if (is.character(col)) return(NA_character_)
      if (is.logical(col))   return(NA)
      if (is.integer(col))   return(NA_integer_)
      if (inherits(col, "Date"))    return(as.Date(NA))
      if (inherits(col, "POSIXct")) return(as.POSIXct(NA, tz = attr(col, "tzone")))
      # default: numeric/double or other
      return(NA_real_)
    }), names(dat)),
    stringsAsFactors = FALSE
  )

  # Fill requested columns (no recycling)
  if (length(cols)) {
    for (i in seq_along(cols)) {
      new_row[[ cols[i] ]] <- values[[i]]
    }
  }

  # Bind: head | new_row | tail
  head_part <- if (above > 1) dat[seq_len(above - 1), , drop = FALSE] else dat[0, , drop = FALSE]
  tail_part <- if (above <= nrow(dat)) dat[above:nrow(dat), , drop = FALSE] else dat[0, , drop = FALSE]

  res <- rbind(head_part, new_row, tail_part)

  return(res)
}
