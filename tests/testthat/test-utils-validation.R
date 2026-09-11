test_that("data frame metadata validates its input", {
  expect_snapshot(error = TRUE, df_variables(list(value = 1)))
})

test_that("file metadata validates paths and extensions", {
  fst_file <- system.file("extdata", "srvdata.fst", package = "utils.ninsoc", mustWork = TRUE)
  parquet_file <- system.file(
    "extdata",
    "srvdata.parquet",
    package = "utils.ninsoc",
    mustWork = TRUE
  )
  txt_file <- file.path(tempdir(), "utils-ninsoc-validation.txt")
  writeLines("value", txt_file)
  on.exit(unlink(txt_file), add = TRUE)
  missing_fst <- file.path(tempdir(), "utils-ninsoc-validation.fst")
  missing_parquet <- file.path(tempdir(), "utils-ninsoc-validation.parquet")
  normalize_tempdir <- function(x) gsub(tempdir(), "<tempdir>", x, fixed = TRUE)

  expect_snapshot(error = TRUE, fst_variables(1))
  expect_snapshot(error = TRUE, fst_variables(rep(fst_file, 2)))
  expect_snapshot(error = TRUE, fst_variables(txt_file), transform = normalize_tempdir)
  expect_snapshot(error = TRUE, fst_variables(missing_fst), transform = normalize_tempdir)

  expect_snapshot(error = TRUE, pq_variables(1))
  expect_snapshot(error = TRUE, pq_variables(rep(parquet_file, 2)))
  expect_snapshot(error = TRUE, pq_variables(txt_file), transform = normalize_tempdir)
  expect_snapshot(error = TRUE, pq_variables(missing_parquet), transform = normalize_tempdir)
})

test_that("compression helpers validate their inputs", {
  expect_snapshot(error = TRUE, compress_needs_convert(environment()))

  expect_snapshot(error = TRUE, compress_should_parallelize("3", 2))
  expect_snapshot(error = TRUE, compress_should_parallelize(3, "2"))

  expect_snapshot(error = TRUE, compress_convert(1:3, 3))
  expect_snapshot(error = TRUE, compress_convert(list(1:3), "3"))
})

test_that("data frame compression validates its inputs", {
  expect_snapshot(error = TRUE, compress_data(list(value = 1)))
})

test_that("Arrow compression validates its inputs", {
  df <- data.frame(value = c(1, 2))

  expect_snapshot(error = TRUE, compress_arrow(list(value = 1)))
  expect_snapshot(error = TRUE, compress_arrow(df, int64 = 1))
  expect_snapshot(error = TRUE, compress_arrow(df, exclude = 1))
  expect_snapshot(error = TRUE, compress_arrow(df, exclude = "missing"))
})

test_that("chunk sizing validates its inputs", {
  df <- data.frame(value = c(1, 2))

  expect_snapshot(error = TRUE, optimal_chunk_size(list(value = 1)))
  expect_snapshot(error = TRUE, optimal_chunk_size(df, chunk_size_bytes = "1"))
})

test_that("Arrow casting validates its inputs", {
  arrow_table <- arrow::Table$create(data.frame(value = 1:2))

  expect_snapshot(error = TRUE, cast_arrow_dtype(data.frame(value = 1:2), value, arrow::int8()))
  expect_snapshot(error = TRUE, cast_arrow_dtype(arrow_table, value, "int8"))
  expect_snapshot(error = TRUE, cast_arrow_dtype(arrow_table, missing, arrow::int8()))
})
