context("Rolling origin function")

pred <- knn_forecasting(ts(1:30), h = 4, msas = "recursive")
ro <- rolling_origin(pred, h = 4)

m <- matrix(c(27, 28, 29, 30, 28, 29, 30, NA, 29, 30, NA, NA, 30, NA, NA, NA),
            nrow = 4, byrow = TRUE)
colnames(m) <- paste("h=", 1:4, sep = "")

test_that("Test set is built correctly", {
  expect_equal(m, ro$test_sets)
})

p <- matrix(c(24, 24, 24, 24, 25, 25, 25, NA, 26, 26, NA, NA, 27, NA, NA, NA),
            nrow = 4, byrow = TRUE)
colnames(p) <- paste("h=", 1:4, sep = "")

test_that("Predictions are OK", {
  expect_equal(p, ro$prediction)
})


e <- matrix(c(3, 4, 5, 6, 3, 4, 5, NA, 3, 4, NA, NA, 3, NA, NA, NA),
            nrow = 4, byrow = TRUE)
colnames(e) <- paste("h=", 1:4, sep = "")

test_that("Errors are OK", {
  expect_equal(e, ro$errors)
})

ro <- rolling_origin(pred, h = 4, rolling = FALSE)

m <- matrix(c(27, 28, 29, 30), nrow = 1, byrow = TRUE)
colnames(m) <- paste("h=", 1:4, sep = "")

test_that("Test set is built correctly", {
  expect_equal(m, ro$test_sets)
})

p <- matrix(c(24, 24, 24, 24), nrow = 1, byrow = TRUE)
colnames(p) <- paste("h=", 1:4, sep = "")

test_that("Predictions are OK", {
  expect_equal(p, ro$prediction)
})
