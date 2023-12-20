expect_error(predict(knn_forecasting(ts(1:30), msas = "MIMO", h = 3), h = 4))

test_that("MIMO strategy predicts fine", {
  expect_equal(knn_forecasting(ts(1:30), h = 3)$prediction,
               predict(knn_forecasting(ts(1:30), h = 3), h = 3)$prediction)
})

test_that("recursive strategy predicts fine", {
  expect_equal(knn_forecasting(ts(1:30), h = 6, msas = "recursive")$prediction,
               predict(knn_forecasting(ts(1:30), h = 3, msas = "recursive"), h
                       = 6)$prediction)
})
