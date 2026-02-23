
srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
srvdata_fst_file = system.file("extdata", "srvdata.fst", package = "utils.ninsoc", mustWork = TRUE)

test_that("return dimensions", {
  expect_equal(ncol(srvdata), nrow(df_variables(srvdata)))
  expect_equal(ncol(srvdata), nrow(fst_variables(srvdata_fst_file)))
})
