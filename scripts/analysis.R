# Purpose: Analyze tha cleaned Cyclistic 2024 CSV dataset

# Load libraries
library(vroom)       # Fast CSV reading
library(dplyr)       # Data manipulation
library(ggplot2)     # Visualization
library(lubridate)   # Date/time handling
library(forcats)     # Factor manipulation

# Load dataset CSV
cyclistic_data <- vroom("data/cleaned/cyclistic_2024_cleaned.csv")

glimpse(cyclistic_data)

# split the data into buckets of day, start_hour, duration
cyclistic_time_buckets <- cyclistic_data %>%
  mutate(
    duration_bucket = case_when(
      ride_duration < 10 ~ "<10 min",
      ride_duration < 20 ~ "10-20 min",
      ride_duration < 30 ~ "20-30 min",
      ride_duration < 60 ~ "30-60 min",
      TRUE ~ ">60 min"
    )
  ) %>%
  group_by(start_day_of_week, start_hour, duration_bucket) %>%
  summarise(rides_count = n(), .groups = "drop")

# split the data into buckets of start locations ranked by location with most rides starting there
cyclistic_locations <- cyclistic_data %>%
  group_by(start_station_name) %>%
  summarise(total_rides = n(), avg_duration = mean(ride_duration, na.rm = TRUE)) %>%
  arrange(desc(total_rides))

# split data according to average ride duration by hour, day, month, sort by count
avg_duration_summary <- cyclistic_data %>%
  group_by(start_day_of_week, start_hour, start_month) %>%
  summarise(
    rides_count = n(),
    avg_duration = mean(ride_duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(rides_count))

# Find top twenty locations (classify locations based on [residential, commercial/office, mixed]) for starting rides
# identify top twenty locations (classify locations based on [residential, commercial/office, mixed]) for starting rides for different member types
# split above relationships based on member type, rideable type
# categories rides based on duration and member type and hour/day/month




