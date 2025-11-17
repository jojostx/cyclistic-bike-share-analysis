# ============================================================
# PURPOSE: Visualize Cyclistic 2024 ride data scoped by member type
#          Includes stacked bar and ridgeline plots for duration/time
# AUTHOR:  Onyedikachi Ikuru
# DATE:    11-16-2025
# ============================================================

# Load libraries
library(vroom)
library(dplyr)
library(ggplot2)
library(forcats)
library(here)
library(tidyr)
library(readr)
library(ggridges)

dir.create(here("plots"), showWarnings = FALSE)

# ---------------------------
# Load processed CSVs
# ---------------------------
duration_time_member <- read_csv(
  here(
    "data",
    "output",
    "duration_time_by_member.csv"
  )
)
duration_time_member <- duration_time_member %>%
  mutate(
    duration_bucket = factor(
      duration_bucket,
      levels = duration_levels,
      ordered = TRUE
    ),
    start_day_of_week = factor(
      start_day_of_week,
      levels = weekday_levels,
      ordered = TRUE
    ),
    member_casual = factor(
      member_casual,
      levels = member_levels,
      ordered = TRUE
    ),
    start_hour = as.integer(start_hour)
  )
member_stats <- read_csv(here("data", "output", "member_stats.csv"))
bike_behavior <- read_csv(here("data", "output", "bike_behavior_by_member.csv"))
hourly_by_member <- read_csv(here("data", "output", "hourly_by_member.csv"))
daily_by_member <- read_csv(here("data", "output", "daily_by_member.csv"))
monthly_by_member <- read_csv(here("data", "output", "monthly_by_member.csv"))
top_start_with_category <- read_csv(
  here("data", "output", "top_start_with_category.csv")
)
top_end_with_category <- read_csv(
  here("data", "output", "top_end_with_category.csv")
)

# ---------------------------
# Factor levels
# ---------------------------
duration_levels <- c(
  "<5 min",
  "5–10 min",
  "10–15 min",
  "15–30 min",
  "30–60 min",
  "1–2 hrs",
  ">2 hrs"
)
weekday_levels <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
)
member_levels <- c("member", "casual")

if (interactive()) {
  glimpse(top_start_with_category)
  print(table(top_start_with_category$category, useNA = "ifany"))
}
