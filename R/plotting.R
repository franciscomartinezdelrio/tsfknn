#' @importFrom ggplot2 autoplot
NULL

#' Create a ggplot object from a knnForecast object.
#'
#' It uses a knnForecast object to create a ggplot object that plots a time
#' series and its forecast using a KNN model.
#'
#' @param forecast The knnForecast object.
#'
#' @param highlight A boolean value indicating whether the data points
#'                  should be highlighted using points.
#'
#' @return The ggplot object representing a graph with the forecast.
#'
#' @examples
#' pred <- knn_forecasting(USAccDeaths, h = 12, lags = 1:12, k = 2, msas = "MIMO")
#' library(ggplot2)
#' autoplot(pred)
#' autoplot(pred, highlight = "points")
#' @export
autoplot.knnForecast <- function(forecast, highlight = "nothing", faceting = FALSE) {
  # extract the time series
  timeS <- data.frame(
    x = as.vector(time(forecast$timeS)),
    y = as.vector(forecast$timeS)
  )

  # extract the forecast
  pred <- data.frame(
    x = as.vector(time(forecast$prediction)),
    y = as.vector(forecast$prediction)
  )

  if (highlight %in% c("neighbours", "neighbors")) {
    if (ncol(forecast$model$examples$targets) == 1) {
      return(plot_recursive(timeS, pred, forecast, faceting))
     } else {
      return(plot_mimo(timeS, pred, forecast, faceting))
     }
  }

  p <- ggplot2::ggplot(timeS, ggplot2::aes(x, y))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour = "Original"))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour = "Forecast"), data = pred)
  if (highlight == "points") {
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Original"))
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Forecast"), data = pred)
  }
  breaks <- c("Original", "Forecast")
  colours <- c("Original" = "black", "Forecast" = "red")
  p <- p + ggplot2::scale_colour_manual(values = colours, breaks = breaks)
  p <- p + ggplot2::labs(x = "Time", y = NULL, colour = "Time series")
  p
}

plot_recursive <- function(timeS, predS, forecast, faceting) {
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
      d <- forecast$neighbours[h, k]
      feature <- timeS[d - forecast$model$lags, ]
      feature$k <- rep(k, nrow(feature))
      features <- rbind(features, feature)
      target  <- timeS[d + seq(ncol(forecast$model$examples$targets)) - 1, ]
      target$k <- rep(k, nrow(target))
      targets <- rbind(targets, target)
    }
    p <- plot_neighbours(timeS, predS, predS[h, ], example, features, targets, faceting)
    print(p)
    op <- par(ask = TRUE)
  }
  par(op)
}

plot_mimo <- function(timeS, predS, forecast, faceting) {
  # extract the example
  example <- timeS[nrow(timeS) + 1 - forecast$model$lags, ]

  # extract the K nearest neighbours
  features <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(features) <- c("x", "y", "k")
  targets <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(targets) <- c("x", "y", "k")
  for (k in seq(forecast$neighbours)) {
    d <- forecast$neighbours[k]
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
  p <- ggplot2::ggplot(timeS, ggplot2::aes(x, y))
  p <- p + ggplot2::geom_line()

  # plot the forecast
  p <- p + ggplot2::geom_line(data = pred, colour = "red")
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Forecast",
                                            shape = "Forecast"), data = pred2)

  # plot the example
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Example",
                                            shape = "Example"), data = example,
                               size = 2)

  # plot the K nearest neighbours
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Feature",
                                            shape = "Feature"), size = 2,
                               data = features)
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Target",
                                            shape = "Target"), size = 2,
                               data = targets)
  if (faceting) {
    p <- p + facet_grid(k ~ .)
  }

  shapes <- c("Feature" = 1, "Target" = 5, "Example" = 3, "Forecast" = 19)
  breaks <- c("Feature", "Target", "Example", "Forecast")
  p <- p + ggplot2::scale_shape_manual(values = shapes, breaks = breaks)
  colours <- c("Feature" = "blue", "Target" = "green", "Example" = "yellow",
               "Forecast" = "red")
  p <- p + ggplot2::scale_colour_manual(values = colours, breaks = breaks)
  g <- ggplot2::guide_legend("Data point")
  p <- p + ggplot2::guides(colour = g, shape = g)
  p <- p + ggplot2::labs(x = "Time", y = NULL)
  p
}
