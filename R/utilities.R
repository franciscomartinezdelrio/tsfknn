# Combine time series
#
# @param ts1 a time series
# @param ts2 a time series or numeric vector
#
# @return The combination of \code{ts1} and \code{ts2}
combine <- function(ts1, ts2) {
  stats::ts(c(ts1, ts2),
            start = stats::start(ts1),
            frequency = stats::frequency(ts1)
  )
}

train_test <- function(timeS, h) {
  training <- stats::ts(utils::head(timeS, -h),
                        start = stats::start(timeS),
                        frequency = stats::frequency(timeS)
  )
  tmp <- stats::ts(1:2,
                    start = stats::end(training),
                    frequency = stats::frequency(training)
  )
  test <- stats::ts(utils::tail(timeS, h),
                    start = stats::end(tmp),
                    frequency = stats::frequency(tmp)
  )
  list(
    training = training,
    test = test
  )
}
