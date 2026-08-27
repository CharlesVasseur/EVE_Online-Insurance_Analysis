source("R/load_data.R")

### Weekly Loss Ratio

aggregate_weekly <- function(hull_values) {
  hv <- as.data.table(hull_values)
  hv[, week_start := floor_date(date, unit = "week", week_start = 6)]
  hv[, .(total_payout = sum(estimated_hull_value)), by = week_start]
}

weekly_a <- aggregate_weekly(hull_value_window_a)
weekly_b <- aggregate_weekly(hull_value_window_b)

compute_loss_ratio <- function(weekly, window_weeks = 12) {
  setorder(weekly, week_start)
  weekly[, premium := shift(frollmean(total_payout, n = window_weeks, align = "right"), n = 1)]
  weekly[, loss_ratio := total_payout / premium]
  weekly
}

loss_ratio_a <- compute_loss_ratio(weekly_a)
loss_ratio_b <- compute_loss_ratio(weekly_b)

loss_ratio_a <- loss_ratio_a[!is.na(loss_ratio)]
loss_ratio_b <- loss_ratio_b[!is.na(loss_ratio)]

fwrite(loss_ratio_a, "data/aggregated/loss_ratio_window_a.csv")
fwrite(loss_ratio_b, "data/aggregated/loss_ratio_window_b.csv")

### Daily Loss Ratio

aggregate_daily <- function(hull_values) {
  hv <- as.data.table(hull_values)
  hv[, .(total_payout = sum(estimated_hull_value)), by = date]
}

compute_loss_ratio_daily <- function(daily, window_days = 84) {
  setorder(daily, date)
  daily[, premium := shift(frollmean(total_payout, n = window_days, align = "right"), n = 1)]
  daily[, loss_ratio := total_payout / premium]
  daily
}

daily_a <- aggregate_daily(hull_value_window_a)
daily_b <- aggregate_daily(hull_value_window_b)
loss_ratio_daily_a <- compute_loss_ratio_daily(daily_a)[!is.na(loss_ratio)]
loss_ratio_daily_b <- compute_loss_ratio_daily(daily_b)[!is.na(loss_ratio)]

fwrite(loss_ratio_daily_a, "data/aggregated/loss_ratio_daily_window_a.csv")
fwrite(loss_ratio_daily_b, "data/aggregated/loss_ratio_daily_window_b.csv")
