#' Assessing forecasting accuracy with rolling origin
#'
#' It uses the model and the time series associated to the \code{knnForecast}
#' object to asses the forecasting accuracy of the model using the last
#' \code{h} values of the time series to build test sets applying a rolling
#' origin evaluation.
#'
#' This function assess the forecast accuracy of the model used by the
#' \code{knnForecast} object. It uses \code{h} different test and training
#' sets. The first test set consists of the last \code{h} values of the time
#' series (the training set is formed by the previous values). The next test
#' set consists of the last \eqn{h - 1} values of the time series and so on
#' (the last test set is formed by the last value of the time series).
#'
#' @param knnf A \code{knnForecast} object.
#' @param h A positive integer. The forecast horizon. If \code{NULL} the
#'    prediction horizon of the \code{knnForecast} object is used.
#' @param rolling A logical. If \code{TRUE} (the default), forecasting
#'    horizons from 1 to \code{h} are used. Otherwise, only horizon
#'    \code{h} is used.
#' @return A list containing at least the following fields:
#'
#'  \item{\code{test_sets}}{a matrix containing the test sets used in the
#'   evaluation. Every row contains a different test set.}
#'  \item{\code{predictions}}{The predictions for the test sets.}
#'  \item{\code{errors}}{The errors for the test sets.}
#'  \item{\code{global_accu}}{Different measures of accuracy applied to all the
#'  errors.}
#'  \item{\code{h_accu}}{Different measures of accuracy applied to all the
#'  errors for every forecasting horizon.}
#'
#' @examples
#' pred <- knn_forecasting(UKgas, h = 4, lags = 1:4, k = 2)
#' ro <- rolling_origin(pred)
#' print(ro$global_accu)
#' @export
rolling_origin <- function(knnf, h = NULL, rolling = TRUE) {
  # Check knnf parameter
  stopifnot(class(knnf) == "knnForecast")

  # Check h parameter
  if (is.null(h)) h <- length(knnf$prediction)
  stopifnot(is.numeric(h), length(h) == 1, h >= 1)

  # Check rolling parameter
  stopifnot(is.logical(rolling), length(rolling) == 1)

  if (rolling) {
    horizons <- seq(h, 1)
  } else {
    horizons <- h
  }
  timeS <- knnf$model$ts
  test_sets <- matrix(NA, nrow = length(horizons), ncol = h)
  predictions <- test_sets
  ind <- 1
  for (hor in horizons) {
    tt <- train_test(timeS, hor)
    pred <- knn_forecasting(tt$training,
                            h = hor,
                            lags = rev(knnf$model$lags),
                            k = knnf$model$k,
                            msas = knnf$msas,
                            cf = knnf$model$cf)
    test_sets[ind, 1:hor] <- tt$test
    predictions[ind, 1:hor] <- pred$prediction
    ind <- ind + 1
  }
  colnames(test_sets)   <-  paste("h=", 1:h, sep = "")
  colnames(predictions) <-  paste("h=", 1:h, sep = "")
  errors <- test_sets - predictions
  g_rmse <- sqrt(mean(errors ^ 2, na.rm = TRUE))
  g_mae  <- mean(abs(errors), na.rm = TRUE)
  g_mape <- mean(abs(100*errors/test_sets), na.rm = TRUE)
  global_accu <- c(g_rmse, g_mae, g_mape)
  names(global_accu) <- c("RMSE", "MAE", "MAPE")

  accu <- function(c) {
    rmse <- sqrt(mean(errors[, c] ^ 2, na.rm = TRUE))
    mae  <- mean(abs(errors[, c]), na.rm = TRUE)
    mape <- mean(abs(100*errors[, c]/test_sets[, c]), na.rm = TRUE)
    c(rmse, mae, mape)
  }
  h_accu <- sapply(1:h, accu)
  colnames(h_accu) <-  paste("h=", 1:h, sep = "")
  rownames(h_accu) <- c("RMSE", "MAE", "MAPE")

  structure(
    list(
      knnf = knnf,
      test_sets = test_sets,
      predictions = predictions,
      errors = test_sets - predictions,
      global_accu = global_accu,
      h_accu = h_accu
    ),
    class = "knnForecastRO"
  )
}

# @export
# rolling_origin2 <- function(knnf, h = NULL) {
#   # Check knnf parameter
#   stopifnot(class(knnf) == "knnForecast")
#
#   # Check h parameter
#   if (is.null(h)) h <- length(knnf$prediction)
#   stopifnot(is.numeric(h), length(h) == 1, h >= 1)
#
#   timeS <- knnf$model$ts
#   test_sets <- matrix(NA, nrow = h, ncol = h)
#   predictions <- matrix(NA, nrow = h, ncol = h)
#   ind <- h
#   for (hor in seq(h)) {
#     tt <- train_test(timeS, hor)
#     test_sets[ind, 1:hor] <- tt$test
#     knnf$model$examples <- knnf$model$examples[1:(nrow(knnf$model$examples) - 1)]
#     knnf$model$targets <- knnf$model$targets[1:(nrow(knnf$model$targets) - 1)]
#     knnf$model$targetsI <- knnf$model$targetsI[1:(nrow(knnf$model$targetsI) - 1)]
#     predictions[ind, 1:hor] <- pred$prediction
#     ind <- ind - 1
#   }
#   colnames(test_sets)   <-  paste("h=", 1:h, sep = "")
#   colnames(predictions) <-  paste("h=", 1:h, sep = "")
#   errors <- test_sets - predictions
#   g_rmse <- sqrt(mean(errors ^ 2, na.rm = TRUE))
#   g_mae  <- mean(abs(errors), na.rm = TRUE)
#   g_mape <- mean(abs(100*errors/test_sets), na.rm = TRUE)
#   global_accu <- c(g_rmse, g_mae, g_mape)
#   names(global_accu) <- c("RMSE", "MAE", "MAPE")
#
#   accu <- function(c) {
#     rmse <- sqrt(mean(errors[, c] ^ 2, na.rm = TRUE))
#     mae  <- mean(abs(errors[, c]), na.rm = TRUE)
#     mape <- mean(abs(100*errors[, c]/test_sets[, c]), na.rm = TRUE)
#     c(rmse, mae, mape)
#   }
#   h_accu <- sapply(1:h, accu)
#   colnames(h_accu) <-  paste("h=", 1:h, sep = "")
#   rownames(h_accu) <- c("RMSE", "MAE", "MAPE")
#
#   structure(
#     list(
#       knnf = knnf,
#       test_sets = test_sets,
#       predictions = predictions,
#       errors = test_sets - predictions,
#       global_accu = global_accu,
#       h_accu = h_accu
#     ),
#     class = "knnForecastRO"
#   )
# }

#' Plot a prediction of a test set
#'
#' It uses a test set generated with the function \code{\link{rolling_origin}}
#' and plots its forecast.
#'
#' @param x the object obtained from a call to x \code{\link{rolling_origin}}.
#'
#' @param h an integer. The forecasting horizon. If \code{NULL}, the maximum
#'    forecasting horizon of all the test sets is used.
#' @param ... Other plotting parameters to affect the plot.
#'
#' @export
plot.knnForecastRO <- function(x, h = NULL, ...) {
  if (is.null(h))
    h <- ncol(x$test_sets)
  stopifnot(is.numeric(h), length(h) == 1, h >= 1, h <= ncol(x$test_sets))

  timeS <- x$knnf$model$ts
  graphics::plot(timeS, type = "o", pch = 20, ylab = "")
  prediction <- timeS
  prediction[1:(length(timeS) - 1)] <- rep(NA, length(timeS) - 1)
  prediction[(length(timeS) - h + 1):length(timeS)] <-
    x$predictions[nrow(x$test_sets) - h + 1, 1:h]
  graphics::lines(prediction, col = my_colours("red"))
  graphics::points(prediction, col = my_colours("red"), pch = 20)
}

