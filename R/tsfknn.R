#' tsfknn: A package for time series forecasting using KNN regression.
#'
#' The tsfknn package allows univariate time series forecasting using KNN
#' regression.
#'
#' @section Functions:
#' \describe{
#'    \item{knnForecasting}{It is used to forecast a time series}
#'    \item{n_training_examples}{To compute how many training examples would
#'          have a model}
#'    \item{nearest_neighbors}{To see the nearest neighbors used to
#'           forecast a times series}
#'    \item{knn_examples}{To see the examples used by the KNN model}
#'    \item{rolling_origin}{To assess forecast accuracy using rolling origin
#'          evaluation}
#'    \item{autoplot}{To plot a prediction and the nearest neighbors
#'          used in the prediction}
#' }
#'
#' @docType package
#' @name tsfknn
NULL
