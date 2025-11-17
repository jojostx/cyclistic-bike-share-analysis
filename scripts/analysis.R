# ============================================================
# PURPOSE: Perform exploratory and statistical analysis on the
#          cleaned Cyclistic 2024 dataset, scoped by member type
#          Includes scaffolding for station classification workflow
# AUTHOR:  Onyedikachi Ikuru
# DATE:    11-14-2025
# ============================================================

# Load libraries
library(vroom)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(here)
library(tidyr)
library(readr)

# Load dataset CSV
cyclistic_data <- vroom(here("data", "cleaned", "cyclistic_2024_cleaned.csv"))
dir.create(here("data", "output"), recursive = TRUE, showWarnings = FALSE)

# Factor levels
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

# Apply factor conversion and create duration buckets
cyclistic <- cyclistic_data %>%
  mutate(
    duration_bucket = cut(
      ride_duration,
      breaks = c(-Inf, 5, 10, 15, 30, 60, 120, Inf),
      labels = duration_levels,
      right = FALSE
    ),
    start_day_of_week = factor(start_day_of_week, levels = weekday_levels),
    end_day_of_week = factor(end_day_of_week, levels = weekday_levels),
    start_month = factor(start_month, levels = month_levels),
    end_month = factor(end_month, levels = month_levels),
    member_casual = factor(member_casual, levels = member_levels)
  )

# =============================================================
# 1. Ride Duration Buckets by Member Type
# =============================================================
duration_summary <- cyclistic %>%
  group_by(member_casual, duration_bucket) %>%
  summarise(total_rides = n(), .groups = "drop") %>%
  group_by(member_casual) %>%
  mutate(share = total_rides / sum(total_rides)) %>%
  ungroup()

write.csv(duration_summary,
  here("data", "output", "duration_summary_by_member.csv"),
  row.names = FALSE
)

# =============================================================
# 2. Top 20 Start & End Locations by Member Type
# =============================================================
top_start_file <- here("data", "output", "top20_start_stations_by_member.csv")

classified_start_file <- here(
  "data",
  "output",
  "top20_start_stations_by_member_gemini_classified.csv"
)

top_start_location_by_member <- cyclistic %>%
  count(
    member_casual,
    start_station_name,
    name = "total_rides",
    sort = TRUE
  ) %>%
  group_by(member_casual) %>%
  slice_max(total_rides, n = 20, with_ties = FALSE) %>%
  ungroup() %>%
  rename(station_name = start_station_name)

write.csv(top_start_location_by_member, top_start_file, row.names = FALSE)

top_end_file <- here("data", "output", "top20_end_stations_by_member.csv")

classified_end_file <- here(
  "data",
  "output",
  "top20_end_stations_by_member_gemini_classified.csv"
)

top_end_location_by_member <- cyclistic %>%
  count(
    member_casual,
    end_station_name,
    name = "total_rides",
    sort = TRUE
  ) %>%
  group_by(member_casual) %>%
  slice_max(total_rides, n = 20, with_ties = FALSE) %>%
  ungroup() %>%
  rename(station_name = end_station_name)

write.csv(top_end_location_by_member, top_end_file, row.names = FALSE)

# ---------------------------
# Run Python classification IF classified file is missing
# ---------------------------
run_python_classification <- function(input_csv) {
  output_csv <- sub(".csv$", "_gemini_classified.csv", input_csv)
  if (!file.exists(output_csv)) {
    message(
      "Classification file missing. Running Python script for: ",
      input_csv
    )
    # Quote the input path
    status <- system2(
      "python",
      args = c(
        "scripts/address_classification.py",
        shQuote(input_csv)
      )
    )

    if (!identical(status, 0L)) {
      stop("Python classification failed for: ", input_csv)
    }
  } else {
    message("Classification file already exists: ", output_csv)
  }
}

run_python_classification(top_start_file)
run_python_classification(top_end_file)

# ---------------------------
# Load classified CSVs
# ---------------------------
classified_start <- read_csv(classified_start_file)
classified_end <- read_csv(classified_end_file)

top_start_with_category <- top_start_location_by_member %>%
  left_join(
    classified_start,
    by = "station_name",
    relationship = "many-to-many"
  )

write.csv(
  top_start_with_category,
  here("data", "output", "top_start_with_category.csv"),
  row.names = FALSE
)


top_end_with_category <- top_end_location_by_member %>%
  left_join(
    classified_end,
    by = "station_name",
    relationship = "many-to-many"
  )

write.csv(
  top_end_with_category,
  here("data", "output", "top_end_with_category.csv"),
  row.names = FALSE
)
# =============================================================
# 3. Ride Duration Stats by Member Type
# =============================================================
member_stats <- cyclistic %>%
  group_by(member_casual) %>%
  summarise(
    avg_duration = mean(ride_duration, na.rm = TRUE),
    median_duration = median(ride_duration, na.rm = TRUE),
    min_duration = min(ride_duration, na.rm = TRUE),
    max_duration = max(ride_duration, na.rm = TRUE),
    total_rides = n(),
    .groups = "drop"
  )

write.csv(
  member_stats,
  here("data", "output", "member_stats.csv"),
  row.names = FALSE
)

# =============================================================
# 4. Member Type × Bike Type Behavior
# =============================================================
bike_behavior <- cyclistic %>%
  group_by(member_casual, rideable_type) %>%
  summarise(
    avg_duration = mean(ride_duration),
    total_rides = n(),
    .groups = "drop"
  )

write.csv(
  bike_behavior,
  here("data", "output", "bike_behavior_by_member.csv"),
  row.names = FALSE
)

# =============================================================
# 5. Time-based Trends by Member Type
# =============================================================
hourly_by_member <- cyclistic %>%
  group_by(member_casual, start_hour) %>%
  summarise(
    total_rides = n(),
    avg_duration = mean(ride_duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(member_casual, start_hour = 0:23, fill = list(total_rides = 0))

write.csv(
  hourly_by_member,
  here("data", "output", "hourly_by_member.csv"),
  row.names = FALSE
)

daily_by_member <- cyclistic %>%
  group_by(member_casual, start_day_of_week) %>%
  summarise(
    total_rides = n(),
    avg_duration = mean(ride_duration),
    .groups = "drop"
  )

write.csv(
  daily_by_member,
  here("data", "output", "daily_by_member.csv"),
  row.names = FALSE
)

monthly_by_member <- cyclistic %>%
  group_by(member_casual, start_month) %>%
  summarise(
    total_rides = n(),
    avg_duration = mean(ride_duration),
    .groups = "drop"
  )

write.csv(
  monthly_by_member,
  here("data", "output", "monthly_by_member.csv"),
  row.names = FALSE
)

# =============================================================
# 6. Ride Duration × Time × Member Type
# =============================================================
duration_time_member <- cyclistic %>%
  group_by(member_casual, duration_bucket, start_day_of_week, start_hour) %>%
  summarise(
    total_rides = n(),
    avg_duration = mean(ride_duration),
    .groups = "drop"
  )

write.csv(
  duration_time_member,
  here("data", "output", "duration_time_by_member.csv"),
  row.names = FALSE
)
