library(tidyverse)
library(tidymodels)
library(vroom)
library(lubridate)

train <- vroom::vroom("Bike Data/train.csv")
test  <- vroom::vroom("Bike Data/test.csv")

# Remove casual & registered
train <- train %>%
  select(-casual, -registered)

# Define Recipe (log-transform inside recipe!)
bike_recipe <- recipe(count ~ ., data = train) %>%
  step_log(count, base = exp(1)) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = as.factor(weather)) %>%
  step_mutate(hour = lubridate::hour(datetime)) %>%
  step_mutate(season = as.factor(season)) %>%
  step_rm(datetime) %>%
  step_corr(all_numeric_predictors(), threshold = 0.5)

# Define Model
lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model)

# Fit
bike_fit <- fit(bike_workflow, data = train)

# Predict and back-transform
lin_preds <- predict(bike_fit, new_data = test) %>%
  mutate(.pred = exp(.pred))

# Build submission
kaggle_submission <- lin_preds %>%
  bind_cols(test %>% select(datetime)) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))

# Write CSV
vroom_write(kaggle_submission, file = "./LinearPreds.csv", delim = ",")






install.packages("rpart")
library(tidymodels)
my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=500 or 1000) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")