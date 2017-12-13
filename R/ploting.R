#' @export
#' @importFrom ggplot2 autoplot
#' @examples
#' pred <- knn_forecasting(USAccDeaths, h = 12, lags = 1:12, k = 2)
#' library(ggplot2)
#' autoplot(pred)
#' autoplot(pred, highlight = T)
autoplot.knnForecast <- function(model, highlight = FALSE) {
  data <- data.frame(
    tsx   = as.vector(time(model$timeS)),
    tsy   = as.vector(model$timeS),
    predx = as.vector(time(model$pred)),
    predy = as.vector(model$pred)
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

