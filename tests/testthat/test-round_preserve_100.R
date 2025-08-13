test_that("sums to exactly 100 and preserves length", {
  x <- c(33.4, 33.3, 33.3)          # sums to ~100
  y <- round_preserve_100(x)

  expect_equal(sum(y), 100)
  expect_equal(length(y), length(x))
})

test_that("returns integers only", {
  x <- c(12.9, 12.1, 25.5, 49.5)
  y <- round_preserve_100(x)

  expect_true(all(is.finite(y)))
  expect_true(all(y == as.integer(y)))
})

test_that("works when already sums to 100 (not necessarily unchanged)", {
  x <- c(10, 20, 30, 40)            # already exactly 100
  y <- round_preserve_100(x)

  expect_equal(sum(y), 100)
  expect_true(all(y == as.integer(y)))
  expect_true(all(x == y))
})

test_that("largest remainders get incremented", {
  x <- c(33.6, 33.2, 33.2)          # sum = 100.0
  # floor = 33, 33, 33 -> diff = 1 -> add to index with largest remainder (1st)
  y <- round_preserve_100(x)

  expect_equal(y, c(34, 33, 33))
})

test_that("ties are broken by original order", {
  x <- c(10.25, 20.25, 30.25, 39.25) # all remainders equal (0.25), sum = 100
  # floor = 10,20,30,39 -> diff = 1 -> first index gets +1
  y <- round_preserve_100(x)

  expect_equal(y, c(11, 20, 30, 39))
})

test_that("boundary sums 98 and 102 are allowed", {
  x1 <- c(99, 0, 0, 0)
  x2 <- c(51, 50, 0)

  y1 <- round_preserve_100(x1)
  y2 <- round_preserve_100(x2)

  expect_equal(sum(y1), 100)
  expect_equal(sum(y2), 100)
})

test_that("errors when sum is outside [98, 102]", {
  x_low  <- c(30, 30, 30, 6.9)
  x_high <- c(40, 40, 20, 1.1)

  expect_error(round_preserve_100(x_low),  "approximate 100", ignore.case = TRUE)
  expect_error(round_preserve_100(x_high), "approximate 100", ignore.case = TRUE)
})

