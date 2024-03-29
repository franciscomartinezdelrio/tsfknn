% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{autoplot.knnForecast}
\alias{autoplot.knnForecast}
\title{Create a ggplot object from a knnForecast object}
\usage{
\method{autoplot}{knnForecast}(object, ...)
}
\arguments{
\item{object}{An object of class \code{knnForecast}.}

\item{...}{additional parameter, see details.}
}
\value{
The ggplot object representing a plotting with the forecast.
}
\description{
It uses a knnForecast object to create a ggplot object that plots a time
series and its forecast using KNN regression.
}
\details{
Commonly used parameters are:
\itemize{
\item \code{highlight}. A character string indicating what elements should be highlighted. Possible values are
\code{"none"}, \code{"points"} and \code{"neighbors"}. The default value is \code{"none"}.
\item \code{faceting}. Logical. This applies only if the \code{highlight} parameter is
set to \code{"neighbors"}. It indicates whether the different nearest neighbors
should be seen in different plots (\code{TRUE}, the default value) or in one
plot.
}
}
\examples{
pred <- knn_forecasting(USAccDeaths, h = 12, lags = 1:12, k = 2)
library(ggplot2)
autoplot(pred)
autoplot(pred, highlight = "neighbors")
}
