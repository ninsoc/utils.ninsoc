test_that("pnadc_original_vars validates x", {
  expect_snapshot(error = TRUE, pnadc_original_vars(list(x = 1)))
})

test_that("pnadc_design_lowcase validates data_pnadc", {
  expect_snapshot(error = TRUE, pnadc_design_lowcase(data.frame(upa = 1, id_domicilio = 1)))
})
