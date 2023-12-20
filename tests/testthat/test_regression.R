test_that("knn regression with one target", {
  model <- knn_model(ts(c(2, 3, 1, 5, 4, 0, 7, 1, 2)), lags = 1:2, k = 2,
                     transform = "none")
  r <- list(
    prediction = 3,
    neighbors = c(3, 4)
  )
  expect_equal(regression(model, c(1, 2), k = 2), r)
})

test_that("knn regression with multiple targets", {
  model <- knn_model(ts(c(2, 3, 1, 5, 4, 0, 7, 1, 2)), lags = 1:2, k = 2, nt = 2,
                     transform = "none")
  r <- list(
    prediction = c(3, 4.5),
    neighbors = c(3, 4)
  )
  expect_equal(regression(model, c(1, 2), k = 2), r)
})

test_that("knn regression with weighted combination and equal neighbor", {
  model <- knn_model(ts(c(1, 2, 1, 5, 4, 0, 7, 1, 2)), lags = 1:2, k = 2,
                     nt = 1, cf = "weighted", transform = "none")
  r <- list(
    prediction = 1,
    neighbors = c(3, 4)
  )
  expect_equal(regression(model, c(1, 2), k = 2), r)
})
