library(data.table)

dir.create("data/aggregated", recursive = TRUE, showWarnings = FALSE)

mineral_ids <- c(34, 35, 36, 37, 38, 39, 40, 11399)
jita_region_id <- 10000002

aggregate_market_history <- function(in_file, out_file) {
  df <- fread(in_file)
  filtered <- df[region_id == jita_region_id & type_id %in% mineral_ids]
  setorder(filtered, date, type_id)
  fwrite(filtered, out_file)
  message(sprintf("%s: %d rows (from full file) -> %s", basename(in_file), nrow(filtered), out_file))
  invisible(filtered)
}

aggregate_market_history("data/processed/market_history_window_a.csv", "data/aggregated/mineral_prices_window_a.csv")
aggregate_market_history("data/processed/market_history_window_b.csv", "data/aggregated/mineral_prices_window_b.csv")