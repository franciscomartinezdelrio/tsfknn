<!-- badges: start -->
[![R-CMD-check](https://github.com/franciscomartinezdelrio/tsfknn/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/franciscomartinezdelrio/tsfknn/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsfknn

The goal of tsfknn is to forecast time series using KNN regression

## Installation

You can install tsfknn from github with:

``` r
# install.packages("devtools")
devtools::install_github("franciscomartinezdelrio/tsfknn")
```

## Example

This is a basic example which shows you how to forecast with tsfknn:

``` r
library(tsfknn)
pred <- knn_forecasting(USAccDeaths, h = 12, k = 3)
pred$prediction # To see a time series with the forecasts
plot(pred) # To see a plot with the forecast
library(ggplot2)
autoplot(pred, highlight = "neighbors")  # To see the nearest neighbors
```
