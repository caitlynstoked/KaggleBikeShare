library(tidyverse)
library(tidymodels)
library(vroom)
library(lubridate)
library(dplyr)
library(glmnet)
library(rpart)
library(agua)

train <- vroom("Bike Data/train.csv") %>%
  mutate(count = log1p(count)) %>%     # log-transform target
  select(-casual, -registered) 
test  <- vroom("Bike Data/test.csv")

#===============================
# Feature Engineering Recipe
#===============================
bike_recipe <- recipe(count ~ ., data = train) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather),
              weekend = ifelse(wday(datetime) %in% c(1,7), 1, 0),
              rush_hour = ifelse(hour(datetime) %in% c(7:9, 16:19), 1, 0)) %>%
  step_date(datetime, features = c('month', 'year', 'dow')) |> 
  step_time(datetime, features = "hour") |> 
  step_rm(datetime) |> 
  step_interact(terms = ~ temp:humidity + temp:windspeed + season:humidity) |> 
  step_poly(temp, humidity, degree = 2) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_normalize(all_numeric_predictors())


h2o::h2o.init()
## Define the model
## max_runtime_secs = how long to let h2o.ai run
## max_models = how many models to stack
auto_model <- auto_ml() %>%
  set_engine("h2o", max_runtime_secs=180, max_models=5) %>%
  set_mode("regression")
## Combine into Workflow
automl_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(auto_model) %>%
  fit(data=train)
## Predict
preds <- predict(automl_wf, new_data = test)

kaggle_h20 <- preds %>%
  bind_cols(test) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(
    count = exp(count),                    # back-transform
    count = pmax(0, count),
    datetime = format(datetime, "%Y-%m-%d %H:%M:%S")
  )



vroom_write(kaggle_h20, file = "./PredsPreds.csv", delim = ",")

