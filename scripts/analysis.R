# ============================================================
# PURPOSE: Perform exploratory and statistical analysis on the
#          cleaned Cyclistic 2024 dataset
# AUTHOR:  Onyedikachi Ikuru
# DATE:    11/14/2025
# ============================================================


# Load libraries
library(vroom) # Fast CSV reading
library(dplyr) # Data manipulation
library(ggplot2) # Visualization
library(lubridate) # Date/time handling
library(forcats) # Factor manipulation
library(here) # Paths relative to project root
library(tidyr) # Pivoting and reshaping

# Load dataset CSV
cyclistic_data <- vroom(here("data", "cleaned", "cyclistic_2024_cleaned.csv"))

# Convert key categorical columns to factors with proper order
weekday_levels <- c(
  "Monday", "Tuesday", "Wednesday", "Thursday",
  "Friday", "Saturday", "Sunday"
)

month_levels <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)

member_levels <- c("member", "casual")

duration_levels <- c(
  "<5 min", "5–10 min", "10–15 min", "15–30 min",
  "30–60 min", "1–2 hrs", ">2 hrs"
)

# Apply factor conversion
cyclistic <- cyclistic_data %>%
  mutate(
    # Duration buckets
    duration_bucket = cut(
      ride_duration,
      breaks = c(-Inf, 5, 10, 15, 30, 60, 120, Inf),
      labels = duration_levels,
      right = FALSE
    ),

    # Day of week
    start_day_of_week = factor(start_day_of_week, levels = weekday_levels),
    end_day_of_week = factor(end_day_of_week, levels = weekday_levels),

    # Month
    start_month = factor(start_month, levels = month_levels),
    end_month = factor(end_month, levels = month_levels),

    # Member type
    member_casual = factor(member_casual, levels = member_levels)
  )

glimpse(cyclistic_data)

# At this point cyclistic dataset contains:
# rideable_type, started_at, ended_at, start_station_name,
# end_station_name,member_casual, ride_duration, start_day_of_week,
# start_month, start_hour, end_day_of_week, end_month, end_hour, duration_bucket

# Count rides by bucket and type
duration_summary <- cyclistic %>%
  count(duration_bucket, member_casual) %>%
  ungroup() %>%
  arrange(duration_bucket)

# Pivot to wide format
duration_summary_wide <- duration_summary %>%
  pivot_wider(
    names_from = member_casual,
    values_from = n,
    values_fill = 0
  )

glimpse(duration_summary)
glimpse(duration_summary_wide)

# Rides by hour × weekday (heatmap-ready)
rides_heatmap <- cyclistic %>%
  count(start_day_of_week, start_hour) %>%
  arrange(start_day_of_week, start_hour)

glimpse(rides_heatmap)
