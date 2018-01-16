#' Forecasts using KNN regression.
#'
#' It uses the KNN algorithm to forecast the h next periods ot the time series.
#' The lags used as autoregressive variables are set with the \code{lags}
#' parameter.
#'
#' @param timeS A numeric vector or time series of class \code{ts}.
#' @param h Number of periods for forecasting.
#' @param lags An integer vector in increasing order expressing the lags to be
#'             used as autoregressive variables.
#' @param k The number of neighbours for KNN regression.
#' @return A list with two fields: 1) a matrix with the features of the
#'         examples and 2) a matrix with the targets of the examples
#'
#' @examples
#' pred <- knn_forecasting(USAccDeaths, h = 12, lags = 1:12, k = 2)
#' @export
knn_forecasting <- function(timeS, h, lags, k, msas = "recursive") {
  # Check timeS parameter
  stopifnot(is.ts(timeS) || is.vector(timeS, mode = "numeric"))
  if (! is.ts(timeS))
    timeS <- as.ts(timeS)

    # Check h parameter
  stopifnot(is.numeric(h), length(h) == 1, h >= 1)

  # Check lags parameter
  stopifnot(is.vector(lags, mode = "numeric"))
  if (is.unsorted(lags)) stop("lags should be a vector in increasing order")
  stopifnot(lags[1] >= 1)
  if (tail(lags, 1) + h > length(timeS))
    stop("Impossible to create one example")
  lags <- rev(lags)

  # Check k parameter
  stopifnot(is.numeric(k), length(k) == 1, k >= 1)

  # Check msas parameter
  stopifnot(msas %in% c("recursive", "MIMO"))

  if (msas == "recursive") {
    fit <- knn_model(timeS, lags = lags, k = k , nt = 1)
    prediction <- recPrediction(fit, h = h)
  } else { # MIMO
    fit <- knn_model(timeS, lags = lags, k = k , nt = h)
    example <- as.vector(timeS[(length(timeS) + 1) - lags])
    reg <- regression(fit, example)
    prediction <- reg$prediction
    neighbours <- reg$neighbours
  }
  temp <- ts(1:2, start = end(timeS), frequency = frequency(timeS))
  prediction <- ts(prediction, start = end(temp),
                   frequency = frequency(timeS))
  structure(
    list(
      timeS = timeS,
      model = fit,
      prediction = prediction,
      neighbours = neighbours
    ),
    class = "knnForecast"
  )
}

recPrediction <- function(model, h) {
  pred <- numeric(h)
  values <- as.vector(model$ts)
  for (hor in 1:h) {
    example <- values[(length(values) + 1) - model$lags]
    pred[hor] <- regression(model, example)$prediction
    values <- c(values, pred[hor])
  }
  return(pred)
}
