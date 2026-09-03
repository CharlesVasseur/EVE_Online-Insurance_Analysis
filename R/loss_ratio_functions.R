### Weekly Loss Ratio

aggregate_weekly <- function(hull_values) {
  hv <- as.data.table(hull_values)
  hv[, week_start := floor_date(date, unit = "week", week_start = 6)]
  hv[, .(total_payout = sum(estimated_hull_value)), by = week_start]
}

compute_loss_ratio <- function(weekly, window_weeks = 12) {
  setorder(weekly, week_start)
  weekly[, premium := shift(frollmean(total_payout, n = window_weeks, align = "right"), n = 1)]
  weekly[, loss_ratio := total_payout / premium]
  weekly
}

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