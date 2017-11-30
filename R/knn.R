#' Build the examples.
#'
#' Build the examples for a KNN model to forecast a time series using
#' lags values of the series as autoregressive features.
#'
#' @param timeS The time series.
#' @param lags An integer vector with the lags used as feature vector in
#'             increasing order.
#' @param h The number of targets (amount of horizons to be forecast).
#'
#' @return A list with two fields: 1) a matrix with the features of the
#'         examples and 2) a matrix with the targets of the examples
#' @examples
#' build_examples(ts(1:5), lags = 1:2)
#' build_examples(ts(1:5), lags = 1:2, h = 2)
build_examples <- function(timeS, lags, h = 1) {
  MAXLAG <- tail(lags, 1)
  NCOL = length(lags)
  NROW = length(timeS) - MAXLAG - h + 1
  patterns <- matrix(data = 0, nrow = NROW, ncol = NCOL)
  targets  <- matrix(data = 0, nrow = NROW, ncol = h)
  row <- 1
  for (ind in seq(MAXLAG + h, length(timeS))) {
    patterns[row, ] <- timeS[ind - h + 1 - lags]
    targets[row, ] <- timeS[(ind - h + 1):ind]
    row <- row + 1
  }
  colnames(patterns) <- paste0("Lag", lags)
  colnames(targets)  <- paste0("H", 1:h)
  return(list(patterns = patterns, targets = targets))
}

#' Create a KNN model.
#'
#' Build a KNN model to forecast a time series using autoregressive features.
#'
#' @param timeS The time series.
#' @param lags An integer vector with the lags used as feature vector in
#'             increasing order.
#' @param k The k parameter.
#' @param h The number of targets (amount of horizons to be forecast).
#' @return An object of type knnModel.
#'
#' @export
knn_model <- function(timeS, lags, k, h = 1) {
  stopifnot(lags[1] >= 1)
  MAXLAG <- tail(lags, 1)
  if (MAXLAG + h > length(timeS)) stop("Impossible to create one example")
  examples <- build_examples(timeS, lags, h)
  if (k > nrow(examples$patterns)) stop("k > number of examples")
  result <- list(ts = timeS, lags = lags, examples = examples, k = k)
  structure(result, class = "knnModel")
}

#' Change the k parameter of a KNN model.
#'
#' @param knnMod The KNN model.
#' @param value The new value of k.
#'
#' @export
#' @examples
#' model <- knn_model(ts(1:10), lags = 2:3, k = 3)
#' changeK(model) <-  4
`changeK<-` <- function(knnModel, value) {
  if (value > nrow(knnModel$examples$patterns)) stop("k > number of examples")
  knnModel$k <- value
  knnModel
}

#' Predicts one example doing KNN regression.
#'
#' @param model The KNN model (its class should be knnModel).
#' @param example The features of the example whose target is to be predicted.
#'
#' @export
#' @example
#' model <- knn_model(ts(c(2, 3, 1, 5, 4, 0, 7, 1, 2)), lags = 1:2, k = 2)
#' regression(model, c(2, 1))
regression <- function(model, example) {
  distances <- apply(model$examples$patterns, 1,
                     function(p) sqrt(sum((p - example) ^ 2)))
  o <- order(distances)
  values <- model$examples$targets[o, , drop = F][1:model$k, , drop = F]
  unname(colMeans(values))
}
