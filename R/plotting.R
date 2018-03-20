my_colours <- function(name) {
  col_l <- list("blue" = "#000099",
                "red" = "#CC0000",
                "green" = "#339900",
                "orange" = "#CC79A7"
  )
  return(col_l[[name]])
}

#' @importFrom graphics plot
#' @export
plot.knnForecast <- function(x, y, ...) {
  timeS <- c(x$model$ts, x$prediction)
  timeS <- stats::ts(timeS,
                     start = stats::start(x$model$ts),
                     frequency = stats::frequency(x$model$ts)
  )
  graphics::plot(timeS, type = "n", ylab = "")
  graphics::lines(x$model$ts, type = "o", pch = 20)
  graphics::lines(x$prediction, type = "o",
                  col = my_colours("red"),
                  pch = 20)
}

#' Create a ggplot object from a knnForecast object
#'
#' It uses a knnForecast object to create a ggplot object that plots a time
#' series and its forecast using KNN regression.
#'
#' @param forecast The knnForecast object.
#' @param highlight A string value indicating what elements should be
#'     highlighted. Possible values are "none", "points" and
#'     "neighbors".
#' @param faceting Logical. This applies only if the \code{highlight}
#'     parameter is set to "neighbors". It indicates whether the different
#'     nearest neighbors should be seen in different plots (True) or in one
#'     plot.
#'
#' @return The ggplot object representing a graph with the forecast.
#'
#' @examples
#' pred <- knn_forecasting(USAccDeaths, h = 12, lags = 1:12, k = 2)
#' library(ggplot2)
#' autoplot(pred)
#' autoplot(pred, highlight = "neighbors")
#' @export
#' @importFrom ggplot2 autoplot
autoplot.knnForecast <- function(forecast, highlight = "none", faceting = TRUE) {
  # extract the time series
  timeS <- data.frame(
    x = as.vector(stats::time(forecast$model$ts)),
    y = as.vector(forecast$model$ts)
  )

  # extract the forecast
  pred <- data.frame(
    x = as.vector(stats::time(forecast$prediction)),
    y = as.vector(forecast$prediction)
  )

  if (highlight %in% c("neighbours", "neighbors")) {
    if (length(forecast$model$k) > 1) {
      warning("When several k are used it is not possible to see the
              neighbors")
    } else if (forecast$msas == "recursive") {
      return(plot_recursive(timeS, pred, forecast, faceting))
     } else {
      return(plot_mimo(timeS, pred, forecast, faceting))
     }
  }

  p <- ggplot2::ggplot(timeS, ggplot2::aes_string('x', 'y'))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour = "Original"))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour = "Forecast"), data = pred)
  if (highlight == "points") {
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Original"))
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Forecast"), data = pred)
  }
  breaks <- c("Original", "Forecast")
  colours <- c("Original" = "black", "Forecast" = my_colours("red"))
  p <- p + ggplot2::scale_colour_manual(values = colours, breaks = breaks)
  p <- p + ggplot2::labs(x = "Time", y = NULL, colour = "Time series")
  p
}

plot_recursive <- function(timeS, predS, forecast, faceting) {
  op <- graphics::par(ask = TRUE)
  on.exit(graphics::par(op), add = TRUE)
  for (h in 1:nrow(predS)){
    # extract the example
    temp <- rbind(timeS, predS)
    example <- temp[nrow(timeS) + h - forecast$model$lags, ]

    # extract the K nearest neighbours
    features <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(features) <- c("x", "y", "k")
    targets <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(targets) <- c("x", "y", "k")
    for (k in seq(forecast$model$k)) {
      d <- forecast$neighbors[h, k]
      feature <- timeS[d - forecast$model$lags, ]
      feature$k <- rep(k, nrow(feature))
      features <- rbind(features, feature)
      target  <- timeS[d + seq(ncol(forecast$model$examples$targets)) - 1, ]
      target$k <- rep(k, nrow(target))
      targets <- rbind(targets, target)
    }
    p <- plot_neighbours(timeS, predS, predS[h, ], example, features, targets, faceting)
    print(p)
  }
}

plot_mimo <- function(timeS, predS, forecast, faceting) {
  # extract the example
  example <- timeS[nrow(timeS) + 1 - forecast$model$lags, ]

  # extract the K nearest neighbours
  features <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(features) <- c("x", "y", "k")
  targets <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(targets) <- c("x", "y", "k")
  for (k in seq(forecast$neighbors)) {
    d <- forecast$neighbors[k]
    feature <- timeS[d - forecast$model$lags, ]
    feature$k <- rep(k, nrow(feature))
    features <- rbind(features, feature)
    target  <- timeS[d + seq(ncol(forecast$model$examples$targets)) - 1, ]
    target$k <- rep(k, nrow(target))
    targets <- rbind(targets, target)
  }
  plot_neighbours(timeS, predS, predS, example, features, targets, faceting)
}

plot_neighbours <- function(timeS, pred, pred2, example, features, targets,
                            faceting) {
  # plot the time series
  p <- ggplot2::ggplot(timeS, ggplot2::aes_string('x', 'y'))
  p <- p + ggplot2::geom_line()

  # plot the forecast
  p <- p + ggplot2::geom_line(data = pred, colour = my_colours("red"))
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Forecast",
                                            shape = "Forecast"), data = pred2)

  # plot the example
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Instance",
                                            shape = "Instance"),
                               data = example,
                               size = 2
  )

  # plot the K nearest neighbours
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "NN Features",
                                            shape = "NN Features"),
                               size = 2,
                               data = features
  )
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "NN Targets",
                                            shape = "NN Targets"),
                               size = 2,
                               data = targets
  )
  if (faceting) {
    p <- p + ggplot2::facet_grid(k ~ .)
  }

  shapes <- c("NN Features" = 1, "NN Targets" = 0, "Instance" = 18,
              "Forecast" = 16)
  breaks <- c("NN Features", "NN Targets", "Instance", "Forecast")
  p <- p + ggplot2::scale_shape_manual(values = shapes, breaks = breaks)
  colours <- c("NN Features" = my_colours("blue"),
               "NN Targets" = my_colours("green"),
               "Instance" = my_colours("orange"),
               "Forecast" = my_colours("red")
  )
  p <- p + ggplot2::scale_colour_manual(values = colours, breaks = breaks)
  g <- ggplot2::guide_legend("Data point")
  p <- p + ggplot2::guides(colour = g, shape = g)
  p <- p + ggplot2::labs(x = "Time", y = NULL)
  p
}
