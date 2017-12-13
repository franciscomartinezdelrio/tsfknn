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
#' pred <- knn_forecasting(USAccDeaths, h = 12, lags = 1:12, k = 2)
#' library(ggplot2)
#' autoplot(pred)
#' autoplot(pred, highlight = TRUE)
#' @export
autoplot.knnForecast <- function(forecast, highlight = FALSE) {
  data <- data.frame(
    tsx   = as.vector(time(forecast$timeS)),
    tsy   = as.vector(forecast$timeS),
    predx = as.vector(time(forecast$pred)),
    predy = as.vector(forecast$pred)
  )
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x = tsx, y = tsy, colour = "Original")) +
    ggplot2::geom_line(ggplot2::aes(x = predx, y = predy,
                                    colour = "Prediction")) +
    scale_colour_manual(values = c('black', 'red')) +
    ggplot2::labs(colour = "Time series") +
    ggplot2::xlab("Time") +
    ggplot2::ylab("")
  if (highlight) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x = tsx, y = tsy,
                          colour = "Original")) +
      ggplot2::geom_point(ggplot2::aes(x = predx, y = predy,
                                       colour = "Prediction"))
  }
  p
}

