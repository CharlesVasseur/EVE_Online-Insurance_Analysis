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