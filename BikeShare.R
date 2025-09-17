#load in the packages
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(lubridate)

#bring in the data
bikedata <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/BikeShare/bike-sharing-demand/train.csv")
testdata <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/BikeShare/bike-sharing-demand/test.csv")


#DATA WRANGLING HOMEWORK SECTION
# Dump casual and registered
bikedata <- bikedata |>
  select(-casual, -registered)

# Change count to log(count)
bikedata <- bikedata |>
  mutate(count = log(count))

view(bikedata)

# Recipe to recode weather, extract hour, make season factor...
my_recipe <- recipe(count ~., data = bikedata) |>
  step_mutate(weather = if_else(weather == 4, 3, weather)) |>
  step_mutate(weather = factor(weather)) |>
  step_mutate(datetime_num = as.numeric(datetime)) |>
  step_rm(datetime) |>
  step_mutate(season = factor(season)) |>
  step_corr(all_numeric_predictors(), threshold = 0.5) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data = bikedata)


# Print out baked dataset
head(bikedata)

# Make some variables factors
bikedata <- bikedata |>
  mutate(weather = factor(weather,
                          levels = 1:4,
                          labels = c("Clear",
                                     "Mist/Cloudy",
                                     "Light Rain/Snow",
                                     "Heavy Rain/Snow")))


bikedata <- bikedata |>
  mutate(season = factor(season,
                         levels = 1:4,
                         labels = c("Spring",
                                    "Summer",
                                    "Fall",
                                    "Winter")))


# Do some EDA
dplyr::glimpse(bikedata)
DataExplorer::plot_intro(bikedata)
DataExplorer::plot_bar(bikedata)
DataExplorer::plot_histogram(bikedata)
DataExplorer::plot_missing(bikedata)

# Let's make some new variables & tables
newbikedata <- bikedata |>
  mutate(hour = lubridate::hour(datetime))

data_by_hour <- newbikedata |>
  group_by(hour) |>
  summarise(mean_count = mean(count))

avg_weather <- bikedata |>
  group_by(weather) |>
  summarise(mean_count = mean(count))

  
head(data_by_hour)
head(avg_weather)
View(avg_weather)
View(data_by_hour)

# Make and save plots to the panels

# Feels like temp vs count
p1 <- ggplot(data = bikedata, aes(x = atemp, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(
    title = "Feels like Temp by Count",
    x = "Feels like Temperature (C)",
    y = "Bike Count"
  )+
  theme_minimal()

# Hour of day vs count
p2 <- ggplot(data = data_by_hour, aes(x = factor(hour), y = mean_count, group = 1)) +
  geom_line() +
  labs(title = "Average Bike Count by Hour of Day",
       x = "Hour of Day (0-23)",
       y = "Average Count") +
  theme_minimal()

# Season vs Count
p3 <- ggplot(bikedata, aes(x = season, y = count)) +
  geom_boxplot() +
  labs(
    title = "Bike Counts by Season",
    x = "Season",
    y = "Bike Count"
  ) +
  theme_minimal()

# Weather vs Count
p4 <- ggplot(data = avg_weather, aes(x = weather, y = mean_count, fill = weather)) +
  geom_col() +
  labs(
    title = "Average Bike Count by Weather",
    x = "Weather",
    y = "Average Count"
  )+
  theme_minimal()

# Make the plots
(p1 + p2) / (p3 + p4)





# Lets do a Penalized Linear Regression!
library(tidymodels)

testdata <- testdata |>
  mutate(weather = factor(weather,
                          levels = 1:4,
                          labels = c("Clear",
                                     "Mist/Cloudy",
                                     "Light Rain/Snow",
                                     "Heavy Rain/Snow")))


testdata <- testdata |>
  mutate(season = factor(season,
                         levels = 1:4,
                         labels = c("Spring",
                                    "Summer",
                                    "Fall",
                                    "Winter")))


bikedata


# No penalty
pregmodel_1 <- linear_reg(penalty = 0, mixture = 0) |>
  set_engine("glmnet") |>
  set_mode("regression") |>
  fit(formula = count ~ ., data = bikedata)

bike_predictions <- predict(pregmodel_1,
                            new_data = testdata) |>
  mutate(.pred = exp(.pred))

# LASSO
pregmodel_2 <- linear_reg(penalty = 1, mixture = 1) |>
  set_engine("glmnet") |>
  set_mode("regression") |>
  fit(formula = count ~ ., data = bikedata)

bike_predictions <- predict(pregmodel_2,
                            new_data = testdata) |>
  mutate(.pred = exp(.pred))


# RIDGE
pregmodel_3 <- linear_reg(penalty = 1, mixture = 0) |>
  set_engine("glmnet") |>
  set_mode("regression") |>
  fit(formula = count ~ ., data = bikedata)

bike_predictions <- predict(pregmodel_3,
                            new_data = testdata) |>
  mutate(.pred = exp(.pred))


# Elastic Net
pregmodel_4 <- linear_reg(penalty = 1, mixture = .5) |>
  set_engine("glmnet") |>
  set_mode("regression") |>
  fit(formula = count ~ ., data = bikedata)

bike_predictions <- predict(pregmodel_4,
                            new_data = testdata) |>
  mutate(.pred = exp(.pred))

# Small Penalty
pregmodel_5 <- linear_reg(penalty = 0.1, mixture = .5) |>
  set_engine("glmnet") |>
  set_mode("regression") |>
  fit(formula = count ~ ., data = bikedata)

bike_predictions <- predict(pregmodel_5,
                            new_data = testdata) |>
  mutate(.pred = exp(.pred))


#OK, LETS DO A CROSS VALIDATION!!!
preg_model <- linear_reg(penalty = tune(),
                         mixture = tune()) |>
  set_engine("glmnet")

preg_wf <- workflow() |>
  add_recipe(my_recipe) |>
  add_model(preg_model)



grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 5)

folds <- vfold_cv(bikedata, v = 5, repeats = 1)

CV_results <- preg_wf |>
  tune_grid(resamples = folds,
            grid = grid_of_tuning_params,
            metrics = metric_set(rmse, mae))

collect_metrics(CV_results) |>
  filter(.metric=="rmse") |>
  ggplot(aes(x = penalty, y = mean, color = factor(mixture))) +
  geom_line()

bestTune <- CV_results |>
  select_best(metric = "rmse")

final_wf <-
  preg_wf |>
  finalize_workflow(bestTune) |>
  fit(data=bikedata)

final_wf |>
  predict(new_data = testdata)


# Let's submit
kaggle_submission <- bind_cols(bike_predictions, testdata) |>
  select(datetime, .pred) |>
  rename(count = .pred) |>
  mutate(count = pmax(0, count),
         datetime = as.character(format(datetime)))

vroom_write(x=kaggle_submission, file="./LinearPredictions.csv", delim=",")

