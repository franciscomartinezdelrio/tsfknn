context("n_training_examples function")

test_that("number of examples with MIMO strategy", {
  expect_equal(n_training_examples(ts(1:10), h = 2, lags = 1:3, msas = "MIMO"),
               6)
})

test_that("number of examples with recursive strategy", {
  expect_equal(
    n_training_examples(ts(1:10), h = 2, lags = 1:3, msas = "recursive"),
    7
  )
})
