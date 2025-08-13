#' Round a numeric vector so it sums exactly to 100
#'
#' @description
#' Floors values and then distributes the remaining units to the entries with
#' the largest fractional parts (the “largest remainders” method) so that the
#' final vector sums to exactly 100.
#'
#'
#' @param x A numeric vector whose elements approximately sum to 100
#'   (within c(99, 101)). Values should be finite and typically non-negative.
#'
#' @return
#' A numeric vector of the same length as `x`, with integer values that
#' sum to exactly 100.
#'
#' @examples
#' round_preserve_100(c(33.4, 33.3, 33.3))
#' round_preserve_100(c(12.9, 12.1, 25.5, 49.5))
#'
#' # Edge case: already sums to 100
#' round_preserve_100(c(10, 20, 30, 40))
#'
#' # Will error if far from 100:
#' \dontrun{
#' round_preserve_100(c(10, 10, 10))  # sum = 30
#' }
#'
#' @export

round_preserve_100 <- function(x) {
  sx <- sum(x)
  if (sx > 101 || sx < 99) {
    stop("Your values do not approximate 100. Check `sum(x)`.")
  }

  names_x <- names(x)

  # Round down baseline
  xf <- floor(x)
  rem <- x - xf
  diff <- as.integer(100 - sum(xf))

  if (diff > 0) {
    idx <- order(rem, decreasing = TRUE)
    xf[idx[seq_len(diff)]] <- xf[idx[seq_len(diff)]] + 1
  } else if (diff < 0) {
    # Need to remove units: take those with the smallest remainders first
    idx <- order(rem, decreasing = FALSE)
    xf[idx[seq_len(-diff)]] <- xf[idx[seq_len(-diff)]] - 1
  }
  as.numeric(xf)
}

