# Load in Packages
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(lubridate)
library(ranger)


# Bring in Data
bikedata <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/BikeShare/bike-sharing-demand/train.csv")
testdata <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/BikeShare/bike-sharing-demand/test.csv")


# Data Wrangling
# Dump casual and registered
bikedata <- bikedata |>
  select(-casual, -registered)

# Change count to log(count)
bikedata <- bikedata |>
  mutate(count = log(count))


# Recipe to recode weather, extract hour, make season factor...
my_recipe <- recipe(count ~., data = bikedata) |>
  step_mutate(weather = if_else(weather == 4, 3, weather)) |>
  step_mutate(weather = factor(weather)) |>
  step_time(datetime, features = "hour") |>
  step_mutate(season = factor(season)) |>
  step_corr(all_numeric_predictors(), threshold = 0.5) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data = bikedata)


# BART Model
bart_mod <- bart(trees = tune()) |>
  set_engine("dbarts") |>
  set_mode("regression")

bart_wf <- workflow() |>
  add_recipe(my_recipe) |>
  add_model(bart_mod)

# Create a grid of tuning values
bart_grid <- expand_grid(trees = c(1, 5, 10, 30, 50, 100, 200))

folds <- vfold_cv(bikedata, v = 5, repeats=1)


# Run the Cross Validation
bart_CV_results <- bart_wf |>
  tune_grid(
    resamples = folds,
    grid = bart_grid,
    metrics = metric_set(rmse)
  )


# Find best tuning params
bart_best_tune <- bart_CV_results |>
  select_best(metric = "rmse")


# Finalize workflow
bart_final <- bart_wf |>
  finalize_workflow(bart_best_tune) |>
  fit(data = bikedata)


# Predict the test data
bart_predictions <- bart_final |>
  predict(new_data = testdata) |>
  mutate(.pred = exp(.pred))


# Format & submit to Kaggle
kaggle_submission <- bind_cols(bart_predictions, testdata) |>
  select(datetime, .pred) |>
  rename(count = .pred) |>
  mutate(count = pmax(0, count),
         datetime = as.character(format(datetime)))

vroom_write(x=kaggle_submission, file="./BART.csv", delim=",")
