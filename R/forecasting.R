#' Time series forecasting using KNN regression
#'
#' It applies KNN regression to forecast the future values of a time series.
#' The lags used as autoregressive variables are set with the \code{lags}
#' parameter.
#'
#' @param timeS A numeric vector or time series of class \code{ts}.
#' @param h A positive integer. Number of values to forecast.
#' @param lags An integer vector in increasing order expressing the lags used
#'     as autoregressive variables.
#' @param k A positive integer. The k parameter in KNN regression.
#' @param msas A string indicating the Multiple Step Ahead Strategy used when
#'     more than one value is predicted. It can be "recursive" or "MIMO".
#' @param cf A string. It indicates the combination function used to aggregate
#'     the targets associated with the nearest neighbors. It can be "mean" or
#'     "median".
#' @return An object of class "knnForecast".
#'
#' @examples
#' pred <- knn_forecasting(USAccDeaths, h = 12, lags = 1:12, k = 2)
#' pred$prediction # To see a time series with the forecasts
#' plot(pred) # To see a plot with the forecast
#' @export
knn_forecasting <- function(timeS, h, lags, k, msas = "MIMO",
                            cf = "mean") {
  # Check timeS parameter
  stopifnot(stats::is.ts(timeS) || is.vector(timeS, mode = "numeric"))
  if (! stats::is.ts(timeS))
    timeS <- stats::as.ts(timeS)

  # Check h parameter
  stopifnot(is.numeric(h), length(h) == 1, h >= 1)

  # Check lags parameter
  stopifnot(is.vector(lags, mode = "numeric"))
  if (is.unsorted(lags)) stop("lags should be a vector in increasing order")
  stopifnot(lags[1] >= 1)
  if (utils::tail(lags, 1) + h > length(timeS))
    stop("Impossible to create one example")

  # Check k parameter
  stopifnot(is.numeric(k), length(k) == 1, k >= 1)

  # Check msas parameter
  stopifnot(msas %in% c("recursive", "MIMO"))

  # Check cb parameter
  stopifnot(cf %in% c("mean", "median"))

  if (msas == "recursive") {
    fit <- knn_model(timeS, lags = lags, k = k , nt = 1, cf = cf)
    pred <- recPrediction(fit, h = h)
    prediction <- pred$prediction
    neighbors <- pred$neighbors
  } else { # MIMO
    fit <- knn_model(timeS, lags = lags, k = k , nt = h, cf = cf)
    example <- as.vector(timeS[(length(timeS) + 1) - fit$lags])
    reg <- regression(fit, example)
    prediction <- reg$prediction
    neighbors <- reg$neighbors
  }
  temp <- stats::ts(1:2,
                    start = stats::end(timeS),
                    frequency = stats::frequency(timeS)
  )
  prediction <- stats::ts(prediction,
                          start = stats::end(temp),
                          frequency = stats::frequency(timeS)
  )
  structure(
    list(
      model = fit,
      prediction = prediction,
      neighbors = neighbors
    ),
    class = "knnForecast"
  )
}

recPrediction <- function(model, h) {
  prediction <- numeric(h)
  neighbors <- matrix(nrow = h, ncol = model$k)
  values <- as.vector(model$ts)
  for (hor in 1:h) {
    example <- values[(length(values) + 1) - model$lags]
    reg <- regression(model, example)
    prediction[hor] <- reg$prediction
    neighbors[hor, ] <- reg$neighbors
    values <- c(values, prediction[hor])
  }
  return(list(
    prediction = prediction,
    neighbors = neighbors
  ))
}

#' Nearest neighbors associated with predictions
#'
#' It allows to check the new instances and their nearest neighbors used in a
#' prediction associated with a "knnForecast" object.
#'
#' @param forecast A \code{knnForecast} object.
#' @return A list including the new instances used in KNN regression and their
#'    nearest neighbors.
#'
#' @examples
#' pred <- knn_forecasting(UKgas, h = 4, lags = 1:4, k = 2, msas = "MIMO")
#' nearest_neighbors(pred)
#' @export
nearest_neighbors <- function(forecast) {
  if (ncol(forecast$model$examples$targets) == 1) {
    return(nearest_neighbors_recursive(forecast))
  } else {
    return(nearest_neighbors_mimo(forecast))
  }
}

nearest_neighbors_recursive <- function(forecast) {
  result <- list()
  timeS <- forecast$model$ts
  temp <- c(timeS, forecast$prediction)
  for (h in 1:nrow(forecast$neighbors)){
    # extract the example
    example <- temp[length(timeS) + h - forecast$model$lags]
    names(example) <- paste("Lag", forecast$model$lags)

    r <- data.frame(matrix(
      ncol = ncol(forecast$model$examples$patterns) + 1,
      nrow = forecast$model$k
    ))
    colnames(r) <- c(paste("Lag", forecast$model$lags), "H1")
    for (k in seq(forecast$neighbors[h, ])) {
      d <- forecast$neighbors[h, k]
      r[k, 1:length(forecast$model$lags)] <- timeS[d - forecast$model$lags]
      r[k, (length(forecast$model$lags) + 1):ncol(r)] <-
        timeS[d + seq(ncol(forecast$model$examples$targets)) - 1]
    }
    result[[h]] <- list(
      instance = example,
      nneighbors = r
    )
  }
  return(result)
}

nearest_neighbors_mimo <- function(forecast) {
  timeS <- forecast$model$ts
  example <- timeS[length(timeS) + 1 - forecast$model$lags]
  names(example) <- paste("Lag", forecast$model$lags)
  r <- data.frame(matrix(
    ncol = ncol(forecast$model$examples$patterns) +
      ncol(forecast$model$examples$targets),
    nrow = forecast$model$k
  ))
  colnames(r) <- c(paste("Lag", forecast$model$lags),
                   paste0("H", 1:ncol(forecast$model$examples$targets)))
  for (k in seq(forecast$neighbors)) {
    d <- forecast$neighbors[k]
    r[k, 1:length(forecast$model$lags)] <- timeS[d - forecast$model$lags]
    r[k, (length(forecast$model$lags) + 1):ncol(r)] <-
      timeS[d + seq(ncol(forecast$model$examples$targets)) - 1]
  }
  return(list(
    instance = example,
    nneighbors = r
  ))
}
