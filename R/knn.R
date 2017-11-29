#------------------------------------------------------------------------------------------------
# Select the examples in a times series for autoregression.
# Inputs:
#   - timeS: the time series
#   - lags: a vector with the lags in increasing order
#   - h: amount of horizons to be forecast
# Return value: A list with two fields
#   - a matrix with the features of the examples
#   - a matrix with the targets of the examples
#------------------------------------------------------------------------------------------------
#' @export
build_examples <- function(timeS, lags, h = 1) {
  # Todas estas comprobaciones quiza en la funcion que invoca
  MAXLAG <- tail(lags, 1)
  if (MAXLAG + h > length(timeS)) stop("Impossible to create one example")
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

