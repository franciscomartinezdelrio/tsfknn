#' Combine time series
#'
#' @param ts1 a time series
#' @param ts2 a time series or numeric vector
#'
#' @return The combination of \code{ts1} and \code{ts2}
combine <- function(ts1, ts2) {
  stats::ts(c(ts1, ts2),
            start = stats::start(ts1),
            frequency = stats::frequency(ts1)
  )
}
