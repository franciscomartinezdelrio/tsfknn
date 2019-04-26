#' @export
print.knnForecast <- function (x, ...) {
  cat("\nCall:  ",
      paste(deparse(x$call),
            sep = "\n",
            collapse = "\n"
      ),
      "\n\n",
      sep = ""
  )
  cat("Multiple-Step Ahead Strategy:", x$msas, "\n")
  if (length(x$model$k) == 1) {
    cat("K (number of nearest neighors):", x$model$k, "\n")
  } else {
    cat("K (number of nearest neighors):",
        length(x$model$k),
        "models with ")
    for (ind in seq_along(x$model$k)) {
      if (ind == 1) {
        cat(x$model$k[ind])
      } else if (ind == length(x$model$k)) {
        cat (" and", x$model$k[ind])
      } else {
        cat (",", x$model$k[ind])
      }
    }
    cat(" neighbors repectively\n")
  }
  cat("Autoregressive lags:", rev(x$model$lags), "\n")
  cat("Number of examples:", nrow(x$model$examples$patterns), "\n")
  cat("Targets are combined using ")
  if (x$model$cf %in% c("mean", "median")) {
    cat("the", x$model$cf, "function.\n")
  } else {
    cat("a weighted average.\n")
  }
  invisible(x)
}

#' @export
summary.knnForecast <- function (object, ...) {
  structure(
    list(
      call = object$call,
      k = object$model$k,
      msas = object$msas,
      nneighbors =  nrow(object$model$examples$patterns),
      lags = rev(object$model$lags),
      prediction = object$prediction,
      cf = object$model$cf
    ),
    class = "summary.knnForecast"
  )
}

#' @export
print.summary.knnForecast <- function (x, ...) {
  stopifnot(inherits(x, "summary.knnForecast"))
  cat("\nCall:  ",
      paste(deparse(x$call),
            sep = "\n",
            collapse = "\n"
      ),
      "\n\n",
      sep = ""
  )
  cat("Multiple-Step Ahead Strategy:", x$msas, "\n")
  if (length(x$k) == 1) {
    cat("K (number of nearest neighors):", x$k, "\n")
  } else {
    cat("K (number of nearest neighors):",
        length(x$k),
        "models with ")
    for (ind in seq_along(x$k)) {
      if (ind == 1) {
        cat(x$k[ind])
      } else if (ind == length(x$k)) {
        cat (" and", x$k[ind])
      } else {
        cat (",", x$k[ind])
      }
    }
    cat(" neighbors repectively\n")
  }
  cat("Autoregressive lags:", x$lags, "\n")
  cat("Number of examples:", x$nneighbors, "\n")
  cat("Targets are combined using ")
  if (x$cf %in% c("mean", "median")) {
    cat("the", x$cf, "function.\n")
  } else {
    cat("a weighted average.\n")
  }
  cat("Forecasting horizon:", length(x$prediction), "\n")
  cat("Forecast:\n")
  print(x$prediction)
  invisible(x)
}

