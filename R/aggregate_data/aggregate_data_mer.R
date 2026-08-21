library(data.table)

dir.create("data/aggregated", recursive = TRUE, showWarnings = FALSE)

aggregate_mer_insurance <- function(in_file, out_file) {
  df <- fread(in_file)
  setnames(df, c("date", "value"), c("date", "insurance_net_isk"))
  setorder(df, date)
  fwrite(df, out_file)
  message(sprintf("MER insurance: %d rows, %s to %s -> %s",
                  nrow(df), min(df$date), max(df$date), out_file))
  invisible(df)
}

aggregate_mer_insurance("data/processed/mer_insurance_window_b.csv", "data/aggregated/mer_insurance_window_b.csv")