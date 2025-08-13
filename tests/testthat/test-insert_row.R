test_that("inserts a row in the middle and preserves order/cols", {
  dat <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  res <- insert_row(dat, above = 2, cols = c("a", "b"), values = list(99L, "z"))

  expect_equal(nrow(res), nrow(dat) + 1)
  expect_identical(names(res), names(dat))
  expect_equal(res$a, c(1L, 99L, 2L, 3L))
  expect_equal(res$b, c("a", "z", "b", "c"))
})

test_that("inserts at top (above = 1)", {
  dat <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  res <- insert_row(dat, above = 1, cols = "a", values = 42L)

  expect_equal(nrow(res), 4)
  expect_equal(res$a[1], 42L)
  expect_true(is.na(res$b[1]))
  expect_equal(res$a[-1], 1:3)
})

test_that("inserts at bottom (above = nrow(dat) + 1)", {
  dat <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  res <- insert_row(dat, above = nrow(dat) + 1, cols = c("a", "b"), values = list(7L, "x"))

  expect_equal(nrow(res), 4)
  expect_equal(res$a, c(1L, 2L, 3L, 7L))
  expect_equal(res$b, c("a", "b", "c", "x"))
})

test_that("works with empty cols/values (pure NA row) and preserves surrounding rows", {
  dat <- data.frame(a = 1:2, b = c(TRUE, FALSE))
  res <- insert_row(dat, above = 2)

  expect_equal(nrow(res), 3)
  expect_true(all(is.na(res[2, ])))
  expect_equal(res$a[c(1, 3)], c(1, 2))
  expect_equal(res$b[c(1, 3)], c(TRUE, FALSE))
})

test_that("preserves column types including factor/Date/POSIXct/integer", {
  dt <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  dat <- data.frame(
    num = c(1.5, 2.5),
    int = c(1L, 2L),
    chr = c("a", "b"),
    lgl = c(TRUE, FALSE),
    fct = factor(c("x", "y")),
    dat = as.Date(c("2024-01-01", "2024-01-02")),
    stringsAsFactors = FALSE
  )
  dat$ts <- c(dt, dt + 3600)

  res <- insert_row(dat, above = 2,
                    cols = c("num", "int", "chr", "fct", "dat", "ts"),
                    values = list(9.9, 100L, "z", "x", as.Date("2025-08-13"), dt))

  expect_true(is.double(res$num))
  expect_true(is.integer(res$int))
  expect_true(is.character(res$chr))
  expect_true(is.logical(res$lgl))
  expect_true(is.factor(res$fct))
  expect_s3_class(res$dat, "Date")
  expect_s3_class(res$ts, "POSIXct")

  # inserted values appear correctly
  expect_equal(res$num[2], 9.9)
  expect_equal(res$int[2], 100L)
  expect_equal(res$chr[2], "z")
  expect_equal(as.character(res$fct[2]), "x")
  expect_equal(res$dat[2], as.Date("2025-08-13"))
  expect_equal(res$ts[2], dt)
})

test_that("mismatched `cols` and `values` errors (no recycling)", {
  dat <- data.frame(a = 1:2, b = 3:4)
  expect_error(insert_row(dat, above = 2, cols = c("a", "b"), values = 9),
               "same length", ignore.case = TRUE)
})

test_that("unknown columns error clearly", {
  dat <- data.frame(a = 1:2, b = 3:4)
  expect_error(insert_row(dat, above = 2, cols = c("a", "c"), values = list(1, 2)),
               "Unknown column", ignore.case = TRUE)
})

test_that("out-of-bounds 'above' (<1 or > nrow+1) errors", {
  dat <- data.frame(a = 1:3, b = 3:5)
  expect_error(insert_row(dat, above = 0), "between 1 and nrow\\(dat\\)\\+1")
  expect_error(insert_row(dat, above = nrow(dat) + 2), "between 1 and nrow\\(dat\\)\\+1")
})


