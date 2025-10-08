#===============================
# Libraries
#===============================
library(tidymodels)
library(tidyverse)
library(tidymodels)
library(vroom)
library(lubridate)
library(dplyr)
library(glmnet)
library(rpart)

#===============================
# Load Data
#===============================
train <- vroom("Bike Data/train.csv") |> 
  select(-casual, -registered)   # remove leakage columns
test  <- vroom("Bike Data/test.csv")

#===============================
# Cleaning
#===============================
# Change count to log(count) in training only
train <- train %>%
  mutate(count = log(count))

#===============================
# Feature Engineering Recipe
#===============================
bike_recipe <- recipe(count ~ ., data = train) %>%
  
  # 1. Recode weather "4" -> "3" and make factor
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = as.factor(weather)) %>%
  
  # 2. Extract hour from datetime
  step_mutate(hour = lubridate::hour(datetime)) %>%
  
  # 3. Make season a factor
  step_mutate(season = as.factor(season)) %>%
  
  # 4. Extra: weekday
  step_mutate(wday = lubridate::wday(datetime, label = TRUE)) %>%
  
  # Drop raw datetime
  step_rm(datetime) %>%
  
  # Encode all categorical variables
  step_dummy(all_nominal_predictors()) %>%
  
  # Normalize numeric predictors
  step_normalize(all_numeric_predictors())

# Prep & bake to check dataset
bike_recipe_prep <- prep(bike_recipe)
train_baked <- bake(bike_recipe_prep, new_data = train)
test_baked  <- bake(bike_recipe_prep, new_data = test)

# Select best parameters
best_params <- select_best(tuned_results, metric = "rmse")

# Finalize workflow with best params
final_wf <- finalize_workflow(penalized_wf, best_params)

# Fit final penalized model
final_fit <- fit(final_wf, data = train)

# Predict test set
penalized_preds <- predict(final_fit, new_data = test)

kaggle_penalized <- penalized_preds %>%
  bind_cols(test) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(
    count = exp(count),                     # back-transform
    count = pmax(0, count),
    datetime = format(datetime, "%Y-%m-%d %H:%M:%S")
  )

#===============================
# Fit Regression Tree / Random Forest
#===============================

my_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Workflow
my_wf <- workflow() %>%
  add_model(my_mod) %>%
  add_recipe(bike_recipe)

# Example tuning (fix your grid definition)
mygrid <- grid_regular(
  mtry(range = c(1, ncol(train_baked) - 1)),  # mtry ≤ number of predictors
  min_n(),
  levels = 5
)

# Suppose you already tuned → `best_params`
final_wf <- finalize_workflow(my_wf, best_params)

# Fit final model on all training data
final_fit <- fit(final_wf, data = train)

#===============================
# Predictions for Kaggle
#===============================
rf_preds <- predict(final_fit, new_data = test) %>%
  bind_cols(test %>% select(datetime)) %>%
  rename(count = .pred) %>%
  mutate(
    count = exp(count),                     # back-transform log(count)
    count = pmax(0, count),                 # no negatives
    datetime = format(datetime, "%Y-%m-%d %H:%M:%S")
  )

#===============================
# Save submission
#===============================
vroom_write(rf_preds,
            file = "./RegressTreePreds.csv",
            delim = ",")

