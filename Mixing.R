library(tidyverse)
library(tidymodels)
library(vroom)
library(lubridate)
library(dplyr)

#===============================
# Load Data
#===============================
train <- vroom("Bike Data/train.csv") %>%
  select(-casual, -registered)   # 1. Remove leakage columns
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
  
  # 4. EXTRA: create weekday (my choice)
  step_mutate(wday = lubridate::wday(datetime, label = TRUE)) %>%
  
  # Drop raw datetime
  step_rm(datetime)

# Prep & Bake to show dataset
bike_recipe_prep <- prep(bike_recipe)
train_baked <- bake(bike_recipe_prep, new_data = train)
test_baked  <- bake(bike_recipe_prep, new_data = test)

# Show first 5 rows (requirement)
head(train_baked, 5)

#===============================
# Define Model
#===============================
lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

#===============================
# Workflow
#===============================
bike_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data = train)

#===============================
# Predict on Test
#===============================
lin_preds <- predict(bike_wf, new_data = test)

kaggle_submission <- lin_preds %>%
  bind_cols(test) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(
    count = exp(count),                    # back-transform
    count = pmax(0, count),                # no negatives
    datetime = format(datetime, "%Y-%m-%d %H:%M:%S") # Kaggle format
  )

#===============================
# Save Submission
#===============================
vroom_write(kaggle_submission, file = "./RecipeLinearPreds.csv", delim = ",")



