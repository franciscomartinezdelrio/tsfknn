## tsfknn 0.3.0

* Now it is possible to assess the model using rolling origin evaluation
* A predict method has been added to generate new forecasts based on a
  previously built model

## tsfknn 0.2.0

* summary and print.summary methods are added for "knnForecast" objects
* String parameters are processed with math.arg
* Fix calculation of how many KNN examples has the model in knn_forecasting
* Weighted combination of the targets of nearest neighbors is implemented
* A function that computes the number of training instances that would have 
  a model has been added
