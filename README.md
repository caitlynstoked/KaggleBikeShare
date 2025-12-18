# KaggleBikeShare
This contains my code and data for the Bike Share competition from Kaggle
# Bike Sharing Demand Forecasting

## Overview

This project addresses Kaggle’s *Bike Sharing Demand* competition, which focuses on forecasting hourly bike rental demand for Washington, D.C.’s Capital Bikeshare program. The task combines historical usage data with weather and temporal features to model urban mobility patterns.

The problem is framed as a **regression task**, where the goal is to predict the total number of bike rentals for each hour.

## Problem Statement

Given historical bike rental data and associated weather and calendar features, build a model that accurately predicts hourly rental demand. Performance is evaluated using **Root Mean Squared Logarithmic Error (RMSLE)**, which penalizes under- and over-predictions while handling skewed count data.

## Data

* **Training data:** Hourly bike rental counts with weather and calendar features
* **Test data:** Same features without rental counts
* **Target variable:** `count` (log-transformed during modeling)
* **Time span:** 2011–2012 Capital Bikeshare usage data

## Feature Engineering

Extensive feature engineering was performed using a `recipes` pipeline:

* Log-transformed the target variable to stabilize variance
* Extracted temporal features (hour, day of week, month, year)
* Engineered behavioral indicators (weekend, rush hour)
* Added event-based features (Cherry Blossom Festival)
* Incorporated **astronomical features** by computing dusk time using latitude/longitude
* Created interaction terms and polynomial features for weather variables
* Normalized numeric predictors and one-hot encoded categorical features

A custom recipe step (`step_dusk`) was implemented to calculate daily dusk times using solar position data, demonstrating advanced feature engineering within tidymodels.

## Modeling Approach

* Used **H2O AutoML** to automatically train and compare multiple regression models
* Limited runtime and model count to balance performance and efficiency
* Integrated AutoML seamlessly into a tidymodels workflow

## Evaluation Metric

* **Root Mean Squared Logarithmic Error (RMSLE)**

## Prediction & Submission

* Predictions were generated on the test set
* Log-transformed predictions were converted back to original scale
* Negative predictions were clipped to zero
* Output formatted to Kaggle’s required structure:

```
datetime,count
2011-01-20 00:00:00,0
2011-01-20 01:00:00,0
...
```

## Tools & Libraries

* R, tidyverse
* tidymodels & recipes
* h2o (AutoML)
* lubridate, suncalc
* vroom

## Key Takeaways

* Demonstrates **time-series–aware regression modeling**
* Applies advanced **feature engineering**, including astronomical data
* Integrates **AutoML** within a reproducible modeling pipeline
* Highlights best practices for handling skewed count data

## Citation

Will Cukierski. *Bike Sharing Demand*. [https://kaggle.com/competitions/bike-sharing-demand](https://kaggle.com/competitions/bike-sharing-demand), 2014. Kaggle.

