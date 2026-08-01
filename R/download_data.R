library(jsonlite)

#Functions
fetch_year_files <- function(dataset, year) {
  index_url <- sprintf("https://data.everef.net/%s/%d/index.json", dataset, year)
  fromJSON(index_url)$files
}

download_dataset_window <- function(dataset, start_year, end_year, dest_folder, limit = Inf) {
  dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)
  for (yr in start_year:end_year) {
    files <- fetch_year_files(dataset, yr)
    n <- min(nrow(files), limit)
    message(sprintf("Year %d: downloading %d of %d files (%.1f MB total for the year)",
                    yr, n, nrow(files), sum(files$size) / 1e6))
    for (i in seq_len(n)) {
      dest_file <- file.path(dest_folder, files$name[i])
      if (!file.exists(dest_file)) {
        tryCatch(
          download.file(files$url[i], dest_file, mode = "wb", quiet = TRUE),
          error = function(e) message("Failed: ", files$name[i])
        )
        Sys.sleep(0.1)
      }
    }
  }
}


# Time Windows
## Window A: Fountain War + Bloodbath of B-R5RB
window_a <- c(2011, 2015)

## Window B: Casino War + Massacre at M2-XFE
window_b <- c(2018, 2022)

# Datasets
## Killmails
download_dataset_window("killmails", window_a[1], window_a[2], "data/raw/killmails/window_a")
download_dataset_window("killmails", window_b[1], window_b[2], "data/raw/killmails/window_b")

## Market History
download_dataset_window("market-history", window_a[1], window_a[2], "data/raw/market-history/window_a")
download_dataset_window("market-history", window_b[1], window_b[2], "data/raw/market-history/window_b")
