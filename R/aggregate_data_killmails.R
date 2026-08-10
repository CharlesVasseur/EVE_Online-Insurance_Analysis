library(data.table)

dir.create("data/aggregated", recursive = TRUE, showWarnings = FALSE)

aggregate_killmails <- function(in_file, out_file) {
  dt <- fread(in_file, select = c("killmail_time", "victim_ship_type_id"))
  dt[, date := as.Date(killmail_time)]
  agg <- dt[, .(losses = .N), by = .(date, ship_type_id = victim_ship_type_id)]
  setorder(agg, date, ship_type_id)
  fwrite(agg, out_file)
  message(sprintf("%s: %d raw rows -> %d aggregated rows -> %s", in_file, nrow(dt), nrow(agg), out_file))
  invisible(agg)
}

aggregate_killmails("data/processed/killmails_window_a.csv", "data/aggregated/daily_losses_window_a.csv")

aggregate_killmails("data/processed/killmails_window_b.csv", "data/aggregated/daily_losses_window_b.csv")