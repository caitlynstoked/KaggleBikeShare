#===============================
# Libraries
#===============================
install.packages("glmnet")
library(tidyverse)
library(tidymodels)
library(vroom)
library(lubridate)
library(dplyr)
library(glmnet)

#===============================
# Load Data
#===============================
train <- vroom("Bike Data/train.csv") %>%
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

# Show first 5 rows
head(train_baked, 5)

#===============================
# Linear Regression Model
#===============================
lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

bike_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data = train)

lin_preds <- predict(bike_wf, new_data = test)

kaggle_linear <- lin_preds %>%
  bind_cols(test) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(
    count = exp(count),                    # back-transform
    count = pmax(0, count),
    datetime = format(datetime, "%Y-%m-%d %H:%M:%S")
  )

vroom_write(kaggle_linear, file = "./LinearPreds.csv", delim = ",")

#===============================
# Penalized Regression Section
#===============================
penalized_model <- linear_reg(
  penalty = tune(),   # λ
  mixture = tune()    # α
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

penalized_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(penalized_model)

# 5-fold cross-validation
set.seed(123)
folds <- vfold_cv(train, v = 5)

# Grid: 5 penalties x 5 mixtures
grid <- expand_grid(
  penalty = c(0.001, 0.01, 0.1, 1, 10),
  mixture = c(0, 0.25, 0.5, 0.75, 1)
)

# Tune the model
tuned_results <- tune_grid(
  penalized_wf,
  resamples = folds,
  grid = grid,
  metrics = metric_set(rmse)
)

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

vroom_write(kaggle_penalized, file = "./PenalizedPreds.csv", delim = ",")
