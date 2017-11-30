context("KNN regression")

model <- knn_model(ts(c(2, 3, 1, 5, 4, 0, 7, 1, 2)), lags = 1:2, k = 2)

test_that("knn regression with one target", {
  expect_equal(regression(model, c(2, 1)), 3)
})

model <- knn_model(ts(c(2, 3, 1, 5, 4, 0, 7, 1, 2)), lags = 1:2, k = 2, h = 2)

test_that("knn regression with multiple targets", {
  expect_equal(regression(model, c(2, 1)), c(3, 4.5))
})
