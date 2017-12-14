context("KNN forecasting function")

expect_error(knn_forecasting(ts(1:5), h = 1, 3:1, 2))

expect_error(knn_forecasting(ts(1:5), h = 1, 0:2, 2))

expect_error(knn_forecasting(ts(1:5), h = 1, lags = 3:5, k = 1))

pred <- knn_forecasting(ts(c(2, 3, 1, 5, 4, 0, 7, 1, 2)), h = 2, lags = 1:2, k = 2,
                        msas = "MIMO")
test_that("MIMO strategy", {
  expect_equal(as.vector(pred$prediction), c(3, 4.5))
})
