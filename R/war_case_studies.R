source("R/load_data.R")

loss_ratio_window_a <- fread("data/aggregated/loss_ratio_window_a.csv")
loss_ratio_window_b <- fread("data/aggregated/loss_ratio_window_b.csv")
loss_ratio_daily_a <- fread("data/aggregated/loss_ratio_daily_window_a.csv")
loss_ratio_daily_b <- fread("data/aggregated/loss_ratio_daily_window_b.csv")

loss_ratio_window_a$week_start <- as.Date(loss_ratio_window_a$week_start)
loss_ratio_window_b$week_start <- as.Date(loss_ratio_window_b$week_start)
loss_ratio_daily_a$date <- as.Date(loss_ratio_daily_a$date)
loss_ratio_daily_b$date <- as.Date(loss_ratio_daily_b$date)

### War dates

war_dates <- data.table(
  war_name    = c("Fountain War", "Bloodbath of B-R5RB",
                  "World War Bee II (Casino War)", "Massacre at M2-XFE"),
  window      = c("a", "a", "b", "b"),
  start_date  = as.Date(c("2013-06-01", "2014-01-27", "2020-07-01", "2020-12-30")),
  end_date    = as.Date(c("2013-10-31", "2014-01-27", "2021-08-31", "2020-12-31")),
  baseline_type = c("pre_war", "pre_war", "pre_war", "war_own_average"),
  parent_war    = c(NA, NA, NA, "World War Bee II (Casino War)")
)

### Threshold

is_calm <- function(dates, war_dates_subset) {
  !Reduce(`|`, lapply(seq_len(nrow(war_dates_subset)), function(i) {
    dates >= war_dates_subset$start_date[i] & dates <= war_dates_subset$end_date[i]
  }))
}

calm_daily_a <- loss_ratio_daily_a[is_calm(date, war_dates[window == "a"])]
calm_daily_b <- loss_ratio_daily_b[is_calm(date, war_dates[window == "b"])]

threshold_a <- calm_daily_a[, mean(loss_ratio, na.rm = TRUE) + 2 * sd(loss_ratio, na.rm = TRUE)]
threshold_b <- calm_daily_b[, mean(loss_ratio, na.rm = TRUE) + 2 * sd(loss_ratio, na.rm = TRUE)]
thresholds <- c(a = threshold_a, b = threshold_b)

thresholds
quantile(calm_daily_a$loss_ratio, c(.9,.95,.99))
quantile(calm_daily_b$loss_ratio, c(.9,.95,.99))

### Weekly analysis

baseline_weeks <- 12
war_dates[, baseline_start := start_date - weeks(baseline_weeks)]
war_dates[, baseline_end   := start_date - 1]

compute_war_stats <- function(war_dates, ratio_a, ratio_b, date_col, thresholds) {
  lookup <- list(a = ratio_a, b = ratio_b)
  rbindlist(lapply(seq_len(nrow(war_dates)), function(i) {
    w  <- war_dates[i]
    lr <- lookup[[w$window]]
    dc <- lr[[date_col]]
    
    during <- lr[dc >= w$start_date & dc <= w$end_date]
    
    if (w$baseline_type == "war_own_average") {
      parent <- war_dates[war_name == w$parent_war]
      baseline <- lr[dc >= parent$start_date & dc <= parent$end_date]
    } else {
      baseline <- lr[dc >= w$baseline_start & dc <= w$baseline_end]
    }
    
    thr <- thresholds[[w$window]]
    
    data.table(
      war_name                   = w$war_name,
      window                     = w$window,
      baseline_type              = w$baseline_type,
      baseline_periods_available = nrow(baseline),
      baseline_avg_ratio         = mean(baseline$loss_ratio),
      war_periods_available      = nrow(during),
      war_avg_ratio              = mean(during$loss_ratio),
      war_peak_ratio             = max(during$loss_ratio),
      ratio_increase_vs_baseline = mean(during$loss_ratio) / mean(baseline$loss_ratio),
      pct_periods_above_threshold = mean(during$loss_ratio > thr, na.rm = TRUE)
    )
  }))
}

war_stats_weekly <- compute_war_stats(war_dates, loss_ratio_window_a, loss_ratio_window_b,
                                      date_col = "week_start", thresholds = thresholds)
war_stats_weekly
fwrite(war_stats_weekly, "data/aggregated/war_case_studies_weekly.csv")

### Daily analysis

baseline_days <- 84
war_dates[, baseline_start := start_date - days(baseline_days)]
war_dates[, baseline_end   := start_date - 1]

war_stats_daily <- compute_war_stats(war_dates, loss_ratio_daily_a, loss_ratio_daily_b,
                                     date_col = "date", thresholds = thresholds)
war_stats_daily
fwrite(war_stats_daily, "data/aggregated/war_case_studies_daily.csv")
