# ============================================================
# PURPOSE: Visualize Cyclistic 2024 ride data scoped by member type
#          Uses ONLY outputs from analysis.R
#          Adds: peak/off-peak plot, 95% CI plot, weekday×month index heatmap
# AUTHOR:  Onyedikachi Ikuru
# DATE:    11-16-2025
# ============================================================

# Load libraries
library(dplyr)
library(ggplot2)
library(forcats)
library(here)
library(tidyr)
library(readr)
library(ggridges)
library(scales)

dir.create(here("plots"), showWarnings = FALSE)

# ---------------------------
# Factor levels (define BEFORE loading/mutating)
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
month_levels <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)
period_levels <- c("AM peak", "PM peak", "Off-peak")

# ---------------------------
# Load processed CSVs (from analysis.R)
# ---------------------------
duration_time_member <- read_csv(
  here("data", "output", "duration_time_by_member.csv"),
  show_col_types = FALSE
) %>%
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

member_stats <- read_csv(
  here("data", "output", "member_stats.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    member_casual = factor(member_casual,
      levels = member_levels,
      ordered = TRUE
    )
  )

bike_behavior <- read_csv(
  here("data", "output", "bike_behavior_by_member.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    member_casual = factor(
      member_casual,
      levels = member_levels,
      ordered = TRUE
    ),
    rideable_type = as.factor(rideable_type)
  )

hourly_by_member <- read_csv(
  here("data", "output", "hourly_by_member.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    member_casual = factor(
      member_casual,
      levels = member_levels,
      ordered = TRUE
    ),
    start_hour = as.integer(start_hour)
  )

daily_by_member <- read_csv(
  here("data", "output", "daily_by_member.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    member_casual = factor(
      member_casual,
      levels = member_levels,
      ordered = TRUE
    ),
    start_day_of_week = factor(
      start_day_of_week,
      levels = weekday_levels,
      ordered = TRUE
    )
  )

monthly_by_member <- read_csv(
  here("data", "output", "monthly_by_member.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    member_casual = factor(
      member_casual,
      levels = member_levels,
      ordered = TRUE
    ),
    start_month = factor(start_month, levels = month_levels, ordered = TRUE)
  )

top_start_with_category <- read_csv(
  here("data", "output", "top_start_with_category.csv"),
  show_col_types = FALSE
)
top_end_with_category <- read_csv(
  here("data", "output", "top_end_with_category.csv"),
  show_col_types = FALSE
)

# Nice-to-haves outputs
period_by_member <- read_csv(
  here("data", "output", "period_by_member.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    member_casual = factor(
      member_casual,
      levels = member_levels,
      ordered = TRUE
    ),
    period = factor(
      period,
      levels = period_levels,
      ordered = TRUE
    )
  )

weekday_month_index <- read_csv(
  here("data", "output", "weekday_month_index.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    member_casual = factor(
      member_casual,
      levels = member_levels,
      ordered = TRUE
    ),
    start_day_of_week = factor(
      start_day_of_week,
      levels = weekday_levels,
      ordered = TRUE
    ),
    start_month = factor(
      start_month,
      levels = month_levels,
      ordered = TRUE
    )
  )

if (interactive()) {
  glimpse(top_start_with_category)
  print(table(top_start_with_category$category, useNA = "ifany"))
}

# ============================================================
# PLOT 1: Ride share by period and member type (AM/PM peak vs Off-peak)
# ============================================================

p_period_share <- ggplot(
  period_by_member,
  aes(x = member_casual, y = share, fill = period)
) +
  geom_col() +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "Ride share by period and member type",
    x = NULL,
    y = "Share of rides",
    fill = "Period"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  here("plots", "period_share_by_member.png"),
  p_period_share,
  width = 8,
  height = 5,
  dpi = 320
)

# ============================================================
# PLOT 2: Average ride duration with 95% CI by member type
# (uses member_stats.csv produced by analysis.R with ci_low/ci_high)
# ============================================================
stopifnot(all(c("avg_duration", "ci_low", "ci_high") %in% names(member_stats)))

p_ci <- ggplot(member_stats, aes(x = member_casual, y = avg_duration)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  labs(
    title = "Average ride duration with 95% CI by member type",
    x = NULL,
    y = "Average ride duration (minutes)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  here("plots", "avg_duration_ci_by_member.png"),
  p_ci,
  width = 7,
  height = 5,
  dpi = 320
)

# ============================================================
# PLOT 3: Weekday distribution within each month by member type
# (index: share within month)
# ============================================================
p_index <- ggplot(
  weekday_month_index,
  aes(x = start_day_of_week, y = start_month, fill = share)
) +
  geom_tile() +
  facet_wrap(~member_casual, ncol = 2) +
  scale_fill_continuous(labels = label_percent()) +
  labs(
    title = "Weekday distribution within each month by member type",
    x = "Weekday",
    y = "Month",
    fill = "Share"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  here("plots", "weekday_month_index_heatmap.png"),
  p_index,
  width = 9,
  height = 6,
  dpi = 320
)
