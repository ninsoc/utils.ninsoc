test_that("compress_data converts columns to their minimal type", {
  old <- options(utils.ninsoc.parallel = FALSE)
  on.exit(options(old), add = TRUE)

  df <- data.frame(
    char_num = c("1", "2", "3"),
    char_txt = c("a", "b", "c"),
    dbl_int = c(1, 2, 3),
    dbl_frac = c(1.5, 2.5, 3.5),
    int = c(1L, 2L, 3L),
    lgl = c(TRUE, FALSE, NA),
    stringsAsFactors = FALSE
  )

  res <- compress_data(df)

  expect_s3_class(res, "data.frame")
  expect_named(res, names(df))
  expect_type(res$char_num, "integer")
  expect_type(res$char_txt, "character")
  expect_type(res$dbl_int, "integer")
  expect_type(res$dbl_frac, "double")
  expect_type(res$int, "integer")
  expect_type(res$lgl, "logical")
})

test_that("compress_data leaves date columns untouched and preserves order", {
  old <- options(utils.ninsoc.parallel = FALSE)
  on.exit(options(old), add = TRUE)

  df <- data.frame(
    v = c("10", "20", "30"),
    d = as.Date("2020-01-01") + 0:2,
    w = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )

  res <- compress_data(df)

  expect_named(res, c("v", "d", "w"))
  expect_s3_class(res$d, "Date")
  expect_equal(res$d, df$d)
  expect_type(res$v, "integer")
  expect_type(res$w, "character")
})

test_that("parallel path yields identical result to sequential path", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")

  set.seed(1)
  n <- 2000L
  df <- data.frame(
    a = as.character(sample.int(100L, n, replace = TRUE)),
    b = as.double(sample.int(100L, n, replace = TRUE)),
    c = sample.int(100L, n, replace = TRUE),
    d = sample(c(TRUE, FALSE), n, replace = TRUE),
    e = sample(letters, n, replace = TRUE),
    f = runif(n),
    stringsAsFactors = FALSE
  )

  seq_res <- local({
    old <- options(utils.ninsoc.parallel = FALSE)
    on.exit(options(old))
    compress_data(df)
  })
  par_res <- local({
    old <- options(
      utils.ninsoc.parallel = TRUE,
      utils.ninsoc.parallel_threshold = 1,
      utils.ninsoc.workers = 2
    )
    on.exit(options(old))
    compress_data(df)
  })

  expect_identical(par_res, seq_res)
})

test_that("compress_data works on the bundled example data", {
  old <- options(utils.ninsoc.parallel = FALSE)
  on.exit(options(old), add = TRUE)

  srvdata <- readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
  res <- compress_data(srvdata)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), nrow(srvdata))
  expect_named(res, names(srvdata))
})

test_that("compress_data preserves excluded columns", {
  old <- options(utils.ninsoc.parallel = FALSE)
  on.exit(options(old), add = TRUE)

  df <- data.frame(
    compress = c("1", "2", "3"),
    keep_character = c("1", "2", "3"),
    keep_double = c(1, 2, 3),
    date = as.Date("2020-01-01") + 0:2,
    stringsAsFactors = FALSE
  )

  res <- compress_data(df, exclude = c("keep_character", "keep_double"))

  expect_named(res, names(df))
  expect_type(res$compress, "integer")
  expect_identical(res$keep_character, df$keep_character)
  expect_identical(res$keep_double, df$keep_double)
  expect_identical(res$date, df$date)
})

test_that("compress_data accepts empty exclusion lists", {
  df <- data.frame(value = c("1", "2", "3"))

  expect_identical(compress_data(df, exclude = NULL), compress_data(df))
  expect_identical(compress_data(df, exclude = character()), compress_data(df))
})

test_that("compress_data can exclude every column", {
  df <- data.frame(
    character = c("1", "2", "3"),
    double = c(1, 2, 3),
    date = as.Date("2020-01-01") + 0:2,
    stringsAsFactors = FALSE
  )

  res <- compress_data(df, exclude = names(df))

  expect_named(res, names(df))
  expect_identical(res, df)
})

test_that("compress_data validates exclude", {
  df <- data.frame(value = c("1", "2", "3"))

  expect_snapshot(error = TRUE, compress_data(df, exclude = 1))
  expect_snapshot(error = TRUE, compress_data(df, exclude = "missing"))
})
