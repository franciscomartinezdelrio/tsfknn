## tsfknn 0.5.1

* autoplot.knnForecast has been modified to comply with CRAN

## tsfknn 0.5.0

* The default Multi-step ahead strategy is recursive
* An optional transformation to the training samples has been added. It improves forecast accuracy for time series with a trend
* When several k are used, only those k that are equal or lower than
the number of training samples are admitted

## tsfknn 0.4.0

* Using Rcpp for faster computation of nearest neighbors

## tsfknn 0.3.1

* Fix calculation of rolling origin prediction with recursive strategy

## tsfknn 0.3.0

* Now it is possible to assess the model using rolling origin evaluation
* A predict method has been added to generate new forecasts based on a
  previously built model

## tsfknn 0.2.0

* summary and print.summary methods are added for "knnForecast" objects
* String parameters are processed with match.arg
* Fix calculation of how many KNN examples has the model in knn_forecasting
* Weighted combination of the targets of nearest neighbors is implemented
* A function that computes the number of training instances that would have 
  a model has been added
