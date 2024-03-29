#' Time series forecasting using KNN regression
#'
#' It applies KNN regression to forecast the future values of a time series.
#' The lags used as autoregressive variables are set with the \code{lags}
#' parameter. If the user does not set the number of nearest neighbors or
#' the lags, these values are selected automatically.
#'
#' @param timeS A numeric vector or time series of class \code{ts}.
#' @param h A positive integer. Number of values to forecast.
#' @param lags An integer vector in increasing order expressing the lags used
#'     as autoregressive variables.
#' @param k A positive integer. The k parameter in KNN regression. A vector of
#'     k values can also be used. In that case, the forecast is the average
#'     of the forecasts produced by the different models with the different k
#'     parameters.
#' @param msas A string indicating the Multiple-Step Ahead Strategy used when
#'     more than one value is predicted. It can be "recursive" or "MIMO" (the
#'     default).
#' @param cf A string. It indicates the combination function used to aggregate
#'     the targets associated with the nearest neighbors. It can be "median",
#'     "weighted" or "mean" (the default).
#' @param transform A character value indicating whether the training samples
#'   are transformed. If the time series has a trend it is recommended. By
#'   default is \code{"multiplicative"} (multiplicative transformation). It is also
#'   possible a multiplicative transformation or no transformation.
#' @return An object of class \code{"knnForecast"}. The
#'     function \code{\link[base]{summary}} can be used to obtain or print a
#'     summary of the results.
#'
#'     An object of class \code{"knnForecast"} is a list containing at least
#'     the following components:
#'
#'  \item{\code{call}}{the matched call.}
#'  \item{\code{msas}}{the Multi-Step Ahead Strategy.}
#'  \item{\code{prediction}}{a time series with the forecast.}
#'  \item{\code{model}}{an object of class \code{"knnModel"} with the KNN
#'                      model}
#'
#' @examples
#' pred <- knn_forecasting(USAccDeaths, h = 12, lags = 1:12, k = 2)
#' pred$prediction # To see a time series with the forecasts
#' plot(pred) # To see a plot with the forecast
#' @export
knn_forecasting <- function(timeS, h, lags = NULL, k = c(3, 5, 7),
                            msas = c("recursive", "MIMO"),
                            cf = c("mean", "median", "weighted"),
                            transform = c("additive", "multiplicative", "none")) {
  # Check timeS parameter
  stopifnot(stats::is.ts(timeS) || is.vector(timeS, mode = "numeric"))
  if (! stats::is.ts(timeS))
    timeS <- stats::as.ts(timeS)

  # Check h parameter
  stopifnot(is.numeric(h), length(h) == 1, h >= 1)

  # msas parameter
  msas <- match.arg(msas)

  # Check transform parameter
  transform <- match.arg(transform)

  # Check lags parameter
  stopifnot(is.null(lags) || is.vector(lags, mode = "numeric"))
  if (is.null(lags)) {
    if (stats::frequency(timeS) > 1) {
      lags <- 1:stats::frequency(timeS)
    } else {
      partial <- stats::pacf(timeS, plot = FALSE)
      lags <- which(partial$acf > 2/ sqrt(length(timeS)))
      if (length(lags) == 0 ||
          (length(lags) == 1 && transform %in% c("additive", "multiplicative"))) {
          lags = 1:5
      }
    }
  }

  if (is.unsorted(lags)) stop("lags should be a vector in increasing order")
  stopifnot(lags[1] >= 1)

  if ((length(lags) == 1 && transform %in% c("additive", "multiplicative"))) {
    stop("It does not make sense to use only 1 autoregressive lag with the additive or multiplicative transformation")
  }

  # Check k parameter
  stopifnot(is.numeric(k))
  k <- sort(k)
  if (k[1] < 1) stop("k values should be positive")
  if (k[1] > n_training_examples(timeS, h, lags, msas)) {
    stop(paste("Impossible to create", k[1], "examples"))
  } else {
    tmp <- k
    k <- NULL
    for (x in tmp)
      if (x <= n_training_examples(timeS, h, lags, msas)) {
        k <- c(k, x)
      } else {
        warning(paste("k =", x, "rejected: impossible to create",
                      x, "examples"))
      }
  }

  # cf parameter
  cf <- match.arg(cf)

  if (msas == "recursive") {
    fit <- knn_model(timeS, lags = lags, k = k, nt = 1, cf = cf, transform)
  } else { # MIMO
    fit <- knn_model(timeS, lags = lags, k = k, nt = h, cf = cf, transform)
  }
  fit$k <- k
  r <- structure(
    list(
      call = match.call(),
      model = fit,
      msas = msas,
      transformation = transform
    ),
    class = "knnForecast"
  )
  predict(r, h)
}

#' Number of training examples
#'
#' It computes the number of training examples that would have a KNN model
#' with the specified parameters.
#'
#' @inheritParams knn_forecasting
#' @return An integer.
#'
#' @examples
#' n_training_examples(ts(1:10), h = 2, lags = 1:3, msas = "MIMO")
#' n_training_examples(ts(1:10), h = 2, lags = 1:3, msas = "recursive")
#' @export
n_training_examples <- function(timeS, h, lags,
                                msas = c("MIMO", "recursive")) {
  # Check timeS parameter
  stopifnot(stats::is.ts(timeS) || is.vector(timeS, mode = "numeric"))
  if (! stats::is.ts(timeS))
    timeS <- stats::as.ts(timeS)

  # Check h parameter
  stopifnot(is.numeric(h), length(h) == 1, h >= 1)

  # Check lags parameter
  stopifnot(is.vector(lags, mode = "numeric"))

  # Check msas parameter
  msas <- match.arg(msas)

  if (is.unsorted(lags)) stop("lags should be a vector in increasing order")
  stopifnot(lags[1] >= 1)
  if (utils::tail(lags, 1) + ifelse(msas == "MIMO", h, 1) > length(timeS))
    stop("Impossible to create one example")

  length(timeS) - utils::tail(lags, 1) - ifelse(msas == "MIMO", h, 1) + 1
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
  stopifnot(class(forecast) == "knnForecast")

  if (forecast$msas == "recursive") {
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

#' Examples of the model associated with a prediction
#'
#' It allows to see the examples of the model associated to a
#' \code{knnForecast} object.
#'
#' @param forecast A \code{knnForecast} object.
#' @return A matrix including the features and targets of the examples
#'    associated with the model of a \code{knnForecast} object.
#'
#' @examples
#' pred <- knn_forecasting(ts(1:8), h = 1, lags = 1:2, k = 2)
#' knn_examples(pred)
#' @export
knn_examples <- function(forecast) {
  stopifnot(class(forecast) == "knnForecast")
  cbind(forecast$model$examples$patterns, forecast$model$examples$targets)
}

