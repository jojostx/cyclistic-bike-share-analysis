# Purpose: Load, clean, and combine all raw Cyclistic 2024 CSV files

library(vroom)
library(here)
library(dplyr)
library(lubridate)
library(janitor)

# Load all monthly CSVs efficiently
files <- list.files(
  path = here("data", "raw"),
  pattern = "\\.csv$",
  full.names = TRUE
)

cyclistic <- vroom(files) %>%
  clean_names()

# Inspect structure
if (interactive()) {
  glimpse(cyclistic)
}

# Select relevant columns (remove IDs and coordinates)
cyclistic_data <- cyclistic %>%
  select(
    -ride_id,
    -start_station_id,
    -(end_station_id:end_lng)
  )

# Clean character columns (trim spaces, remove newlines)
cyclistic_data <- cyclistic_data %>%
  mutate(across(
    where(is.character),
    ~ trimws(gsub("[\r\n]", "", .))
  ))

parse_datetime_safe <- function(x) {
  parse_date_time(
    x,
    orders = c(
      "Ymd HMS", "Ymd HM", "Ymd",
      "Y-m-d H:M:S", "Y-m-d H:M",
      "Y/m/d H:M:S", "Y/m/d H:M",
      "m/d/Y H:M:S", "m/d/Y H:M",
      "m-d-Y H:M:S", "m-d-Y H:M"
    ),
    tz = "UTC",
    quiet = TRUE
  )
}

cyclistic_data <- cyclistic_data %>%
  mutate(
    started_at = parse_datetime_safe(started_at),
    ended_at   = parse_datetime_safe(ended_at)
  ) %>%
  filter(!is.na(started_at), !is.na(ended_at))

# Add ride duration (minutes)
cyclistic_data <- cyclistic_data %>%
  mutate(
    ride_duration = as.numeric(difftime(ended_at, started_at, units = "mins"))
  ) %>%
  relocate(ride_duration, .after = ended_at)

# Remove negative or extremely long rides (> 1 day)
cyclistic_data <- cyclistic_data %>%
  filter(ride_duration > 0, ride_duration < 1440)

# Standardize member type and bike type values
cyclistic_data <- cyclistic_data %>%
  mutate(
    member_casual = tolower(member_casual),
    rideable_type = tolower(rideable_type)
  ) %>%
  filter(member_casual %in% c("member", "casual"))

# Replace missing station names with "Not Specified"
cyclistic_data <- cyclistic_data %>%
  mutate(across(
    c(start_station_name, end_station_name),
    ~ ifelse(is.na(.) | . == "", "Not Specified", .)
  ))

# Remove duplicates
cyclistic_data <- cyclistic_data %>%
  distinct()

# Add time-based features for start and end
cyclistic_data <- cyclistic_data %>%
  mutate(
    start_day_of_week = wday(started_at, label = TRUE, abbr = FALSE),
    start_month       = month(started_at, label = TRUE, abbr = FALSE),
    start_hour        = hour(started_at),
    end_day_of_week   = wday(ended_at, label = TRUE, abbr = FALSE),
    end_month         = month(ended_at, label = TRUE, abbr = FALSE),
    end_hour          = hour(ended_at)
  )

# Final sanity checks
cat("Rows:", nrow(cyclistic_data), "\n")
cat("Unique member types:\n")
print(unique(cyclistic_data$member_casual))
cat("Unique ride types:\n")
print(unique(cyclistic_data$rideable_type))
cat("Unique months:\n")
print(unique(cyclistic_data$end_month))


# Preview cleaned data
if (interactive()) {
  glimpse(cyclistic_data)
}

# Save cleaned dataset using project-relative path
dir.create(here("data", "cleaned"), recursive = TRUE, showWarnings = FALSE)
write.csv(
  cyclistic_data,
  here("data", "cleaned", "cyclistic_2024_cleaned.csv"),
  row.names = FALSE
)
