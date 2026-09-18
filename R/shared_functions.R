library(data.table)
library(dplyr)
library(lubridate)

# Load Data

daily_losses_window_a <- fread("data/aggregated/daily_losses_window_a.csv")
daily_losses_window_b <- fread("data/aggregated/daily_losses_window_b.csv")
mer_insurance_window_b <- fread("data/aggregated/mer_insurance_window_b.csv")
material_prices_window_a <- fread("data/aggregated/material_prices_window_a.csv")
material_prices_window_b <- fread("data/aggregated/material_prices_window_b.csv.gz")
sde_blueprints_window_a <- fread("data/aggregated/sde_blueprints_window_a.csv")
sde_blueprints_window_b <- fread("data/aggregated/sde_blueprints_window_b.csv")
sde_types_window_a <- fread("data/aggregated/sde_types_window_a.csv")
sde_types_window_b <- fread("data/aggregated/sde_types_window_b.csv")
wars_window_a_active_by_week <- fread("data/aggregated/wars_window_a_active_by_week.csv")
wars_window_a_summary <- fread("data/aggregated/wars_window_a_summary.csv")
wars_window_b_active_by_week <- fread("data/aggregated/wars_window_b_active_by_week.csv")
wars_window_b_summary <- fread("data/aggregated/wars_window_b_summary.csv")

daily_losses_window_a$date <- as.Date(daily_losses_window_a$date)
daily_losses_window_b$date <- as.Date(daily_losses_window_b$date)
material_prices_window_a$date <- as.Date(material_prices_window_a$date)
material_prices_window_b$date <- as.Date(material_prices_window_b$date)
sde_blueprints_window_a$snapshot <- as.Date(sde_blueprints_window_a$snapshot)
sde_blueprints_window_b$snapshot <- as.Date(sde_blueprints_window_b$snapshot, format="%Y%m%d")
mer_insurance_window_b$date <- as.Date(mer_insurance_window_b$date)

# Mineral IDs

mineral_ids <- c(34, 35, 36, 37, 38, 39, 40, 11399)

# War Dates

### Studied Wars

war_dates <- data.table(
  war_name      = c("Battle of Asakai", "Fountain War", "Bloodbath of B-R5RB",
                    "World War Bee II (Casino War)", "Massacre at M2-XFE"),
  window        = c("a", "a", "a", "b", "b"),
  start_date    = as.Date(c("2013-01-26", "2013-06-01", "2014-01-27", "2020-07-01", "2020-12-30")),
  end_date      = as.Date(c("2013-01-27", "2013-10-31", "2014-01-27", "2021-08-31", "2020-12-31")),
  baseline_type = c("pre_war", "pre_war", "pre_war", "pre_war", "war_own_average"),
  parent_war    = c(NA, NA, NA, NA, "World War Bee II (Casino War)")
)

### Short Events

war_dates[, duration_days := as.numeric(end_date - start_date)]
war_dates[, is_short_event := duration_days <= 7]
war_dates[, midpoint_date := start_date + duration_days / 2]

war_dates[, .(war_name, duration_days, is_short_event, baseline_type)]

# Loss Ratio Functions

### Weekly Loss Ratio
aggregate_weekly <- function(hull_values, payout_col = "estimated_hull_value") {
  hv <- as.data.table(hull_values)
  hv[, week_start := floor_date(date, unit = "week", week_start = 6)]
  full_range <- hv[, .(min_date = min(date), max_date = max(date))]
  
  weekly <- hv[, .(total_payout = sum(get(payout_col)), n_days = uniqueN(date)),
               by = week_start]
  weekly[, is_complete_week := n_days == 7 &
           week_start >= full_range$min_date &
           week_start + 6 <= full_range$max_date]
  weekly
}

compute_loss_ratio <- function(weekly, window_weeks = 12) {
  setorder(weekly, week_start)
  weekly[, premium := shift(frollmean(total_payout, n = window_weeks, align = "right"), n = 1)]
  weekly[, loss_ratio := total_payout / premium]
  weekly
}

### Daily Loss Ratio
aggregate_daily <- function(hull_values, payout_col = "estimated_hull_value") {
  hv <- as.data.table(hull_values)
  hv[, .(total_payout = sum(get(payout_col))), by = date]
}

compute_loss_ratio_daily <- function(daily, window_days = 84) {
  setorder(daily, date)
  daily[, premium := shift(frollmean(total_payout, n = window_days, align = "right"), n = 1)]
  daily[, loss_ratio := total_payout / premium]
  daily
}
