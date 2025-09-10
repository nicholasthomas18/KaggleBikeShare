#load in the packages
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(lubridate)

#bring in the data
bikedata <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/BikeShare/bike-sharing-demand/train.csv")

#make some variables factors
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


# Just look at the data
head(bikedata)
View(bikedata)

# Do some EDA
dplyr::glimpse(bikedata)
DataExplorer::plot_intro(bikedata)
DataExplorer::plot_bar(bikedata)
DataExplorer::plot_histogram(bikedata)
DataExplorer::plot_missing(bikedata)

# Let's make some new variables & tables
bikedata <- bikedata |>
  mutate(hour = lubridate::hour(datetime))

data_by_hour <- bikedata |>
  group_by(hour) |>
  summarise(mean_count = mean(count))

avg_weather <- bikedata |>
  group_by(weather) |>
  summarise(mean_count = mean(count))

  
head(data_by_hour)
head(avg_weather)
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
