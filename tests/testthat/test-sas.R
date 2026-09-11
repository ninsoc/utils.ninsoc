test_that("parse_sas_input_code validates its arguments", {
  sas_file <- tempfile(fileext = ".sas")
  writeLines("input value 1;", sas_file)

  expect_snapshot(error = TRUE, parse_sas_input_code())
  expect_snapshot(error = TRUE, parse_sas_input_code(1))
  expect_snapshot(error = TRUE, parse_sas_input_code("does-not-exist.sas"))
  expect_snapshot(error = TRUE, parse_sas_input_code(sas_file, beginline = "1"))
  expect_snapshot(error = TRUE, parse_sas_input_code(sas_file, lrecl = "1"))
  expect_snapshot(error = TRUE, parse_sas_input_code(sas_file, encoding = 1))
})

test_that("uncomment_sas_code validates its arguments", {
  expect_snapshot(error = TRUE, utils.ninsoc:::uncomment_sas_code(1, "/*", "*/"))
  expect_snapshot(error = TRUE, utils.ninsoc:::uncomment_sas_code("input value 1;", 1, "*/"))
  expect_snapshot(error = TRUE, utils.ninsoc:::uncomment_sas_code("input value 1;", "/*", 1))
})

test_that("sas_input_dict validates its arguments", {
  sas_file <- tempfile(fileext = ".sas")
  writeLines("input value 1;", sas_file)

  expect_snapshot(error = TRUE, sas_input_dict())
  expect_snapshot(error = TRUE, sas_input_dict(1))
  expect_snapshot(error = TRUE, sas_input_dict("does-not-exist.sas"))
  expect_snapshot(error = TRUE, sas_input_dict(sas_file, file_ext = 1))
  expect_snapshot(error = TRUE, sas_input_dict(sas_file, encoding = 1))
})
