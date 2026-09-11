test_that("label_values validates x", {
  expect_snapshot(error = TRUE, label_values())
  expect_snapshot(error = TRUE, label_values(new.env(), c("1" = "one")))
  expect_snapshot(error = TRUE, label_values(matrix(1:4, nrow = 2), c("1" = "one")))
})

test_that("label_values validates format", {
  dimensional_format <- matrix(c("one", "two"), nrow = 1)
  names(dimensional_format) <- c("1", "2")

  expect_snapshot(error = TRUE, label_values(1:2))
  expect_snapshot(error = TRUE, label_values(1:2, new.env()))
  expect_snapshot(error = TRUE, label_values(1:2, dimensional_format))
  expect_snapshot(error = TRUE, label_values(1:2, c("one", "two")))
  expect_snapshot(error = TRUE, label_values(1:2, c("1" = "one", "1" = "another one")))
})

test_that("label_values validates na", {
  expect_snapshot(error = TRUE, label_values(1:2, c("1" = "one", "2" = "two"), na = 1))
  expect_snapshot(error = TRUE, label_values(1:2, c("1" = "one", "2" = "two"), na = c("NA", "")))
})

test_that("label_values accepts broad non-dimensional vectors", {
  format <- c("2020-01-01" = "first", "2020-01-02" = "second")

  expect_identical(
    label_values(as.Date(c("2020-01-01", "2020-01-02")), format),
    factor(
      as.Date(c("2020-01-01", "2020-01-02")),
      levels = names(format),
      labels = unname(format),
      exclude = NULL
    )
  )
})
