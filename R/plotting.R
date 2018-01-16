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
  if (highlight %in% c("neighbours", "neighbors")) {
    return(plot_mimo(forecast, faceting))
  }
  timeS <- data.frame(
    x = as.vector(time(forecast$timeS)),
    y = as.vector(forecast$timeS)
  )
  pred <- data.frame(
    x = as.vector(time(forecast$prediction)),
    y = as.vector(forecast$prediction)
  )
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

plot_mimo <- function(forecast, faceting) {
  # plot the time series
  timeS <- data.frame(
    x = as.vector(time(forecast$model$ts)),
    y = as.vector(forecast$model$ts)
  )
  p <- ggplot2::ggplot(timeS, ggplot2::aes(x, y))
  p <- p + ggplot2::geom_line()

  # plot the forecast
  predS <- data.frame(
    x = as.vector(time(forecast$prediction)),
    y = as.vector(forecast$prediction)
  )
  p <- p + ggplot2::geom_line(data = predS, colour = "red")
  p <- p + ggplot2::geom_point(data = predS, ggplot2::aes(colour = "Forecast",
                                                          shape = "Forecast"))

  # plot the example
  example <- timeS[nrow(timeS) + 1 - forecast$model$lags, ]
  p <- p + ggplot2::geom_point(data = example, aes(colour = "Example",
                                                   shape = "Example"), size = 2)

  # plot the K nearest neighbours
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
  p <- p + ggplot2::geom_point(data = features, aes(colour = "Feature",
                                                   shape = "Feature"), size = 2)
  p <- p + ggplot2::geom_point(data = targets, aes(colour = "Target",
                                                  shape = "Target"), size = 2)
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
