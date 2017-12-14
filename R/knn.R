#' Build the examples.
#'
#' Build the examples for a KNN model to forecast a time series using
#' lags values of the series as autoregressive features.
#'
#' @param timeS The time series.
#' @param lags An integer vector with the lags used as feature vector in
#'             decreasing order.
#' @param nt The number of targets.
#'
#' @return A list with two fields: 1) a matrix with the features of the
#'         examples and 2) a matrix with the targets of the examples
#' @examples
#' build_examples(ts(1:5), lags = 2:1)
#' build_examples(ts(1:5), lags = 2:1, nt = 2)
build_examples <- function(timeS, lags, nt = 1) {
  MAXLAG <- lags[1]
  NCOL = length(lags)
  NROW = length(timeS) - MAXLAG - nt + 1
  patterns <- matrix(data = 0, nrow = NROW, ncol = NCOL)
  targets  <- matrix(data = 0, nrow = NROW, ncol = nt)
  row <- 1
  for (ind in seq(MAXLAG + nt, length(timeS))) {
    patterns[row, ] <- timeS[ind - nt + 1 - lags]
    targets[row, ] <- timeS[(ind - nt + 1):ind]
    row <- row + 1
  }
  colnames(patterns) <- paste0("Lag", lags)
  colnames(targets)  <- paste0("H", 1:nt)
  return(list(patterns = patterns, targets = targets))
}

#' Create a KNN model.
#'
#' Build a KNN model to forecast a time series using autoregressive features.
#'
#' @param timeS The time series.
#' @param lags An integer vector with the lags used as feature vector in
#'             decreasing order.
#' @param k The k parameter.
#' @param nt The number of targets (amount of horizons to be forecast).
#' @return An object of type knnModel.
#'
#' @export
knn_model <- function(timeS, lags, k, nt = 1) {
  stopifnot(tail(lags, 1) >= 1)
  MAXLAG <- lags[1]
  if (MAXLAG + nt > length(timeS)) stop("Impossible to create one example")
  examples <- build_examples(timeS, lags, nt)
  if (k > nrow(examples$patterns)) stop("k > number of examples")
  structure(
    list(
      ts = timeS,
      lags = lags,
      examples = examples,
      k = k
    ),
    class = "knnModel"
  )
}

#' Change the k parameter of a KNN model.
#'
#' @param knnMod The KNN model.
#' @param value The new value of k.
#'
#' @export
#' @examples
#' model <- knn_model(ts(1:10), lags = 3:2, k = 3)
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
#' @examples
#' model <- knn_model(ts(c(2, 3, 1, 5, 4, 0, 7, 1, 2)), lags = 2:1, k = 2)
#' regression(model, c(2, 1))
regression <- function(model, example) {
  distances <- apply(model$examples$patterns, 1,
                     function(p) sqrt(sum((p - example) ^ 2)))
  o <- order(distances)
  values <- model$examples$targets[o, , drop = F][1:model$k, , drop = F]
  unname(colMeans(values))
}
