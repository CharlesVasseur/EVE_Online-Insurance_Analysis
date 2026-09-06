library(data.table)

dir.create("data/aggregated", recursive = TRUE, showWarnings = FALSE)

aggregate_wars_window <- function(in_file, window_start, window_end, out_prefix) {
  wars <- fread(in_file)
  wars[, `:=`(started = as.Date(started), finished = as.Date(finished), declared = as.Date(declared))]
  
  window_start <- as.Date(window_start)
  window_end <- as.Date(window_end)
  
  active <- wars[started <= window_end & (is.na(finished) | finished >= window_start)]
  message(sprintf("%d of %d wars were active at some point in the window", nrow(active), nrow(wars)))
  
  weeks <- seq(window_start, window_end, by = "week")
  active_by_week <- rbindlist(lapply(weeks, function(wk) {
    wk_end <- wk + 6
    n_active <- active[started <= wk_end & (is.na(finished) | finished >= wk), .N]
    data.table(week_start = wk, n_wars_active = n_active)
  }))
  fwrite(active_by_week, sprintf("%s_active_by_week.csv", out_prefix))
  
  war_summary <- active[, .(
    war_id, declared, started, finished,
    started_before_window = started < window_start,
    total_isk_destroyed = fifelse(started < window_start, NA_real_,
                                  aggressor_isk_destroyed + defender_isk_destroyed),
    total_ships_killed = fifelse(started < window_start, NA_integer_,
                                 as.integer(aggressor_ships_killed + defender_ships_killed))
  )]
  setorder(war_summary, -total_isk_destroyed, na.last = TRUE)
  fwrite(war_summary, sprintf("%s_summary.csv", out_prefix))
  
  n_flagged <- sum(war_summary$started_before_window)
  message(sprintf("Wrote %d weekly rows and %d war summary rows (%d flagged NA: started before window)",
                  nrow(active_by_week), nrow(war_summary), n_flagged))
  invisible(list(weekly = active_by_week, summary = war_summary))
}

aggregate_wars_window("data/processed/wars_window_a.csv", "2011-01-01", "2015-12-31", "data/aggregated/wars_window_a")
aggregate_wars_window("data/processed/wars_window_b.csv", "2018-01-01", "2022-12-31", "data/aggregated/wars_window_b")