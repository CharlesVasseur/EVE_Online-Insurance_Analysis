library(jsonlite)

#Functions
fetch_year_files <- function(dataset_path, year) {
  index_url <- sprintf("https://data.everef.net/%s/%d/index.json", dataset_path, year)
  fromJSON(index_url)$files
}

download_dataset_window <- function(dataset_path, start_year, end_year, dest_folder, limit = Inf) {
  dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)
  for (yr in start_year:end_year) {
    files <- fetch_year_files(dataset_path, yr)
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

download_wars_yearly <- function(years, dest_folder) {
  dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)
  for (yr in years) {
    url <- sprintf("https://data.everef.net/wars/history/wars-%d.tar.bz2", yr)
    dest_file <- file.path(dest_folder, sprintf("wars-%d.tar.bz2", yr))
    if (!file.exists(dest_file)) {
      tryCatch(
        download.file(url, dest_file, mode = "wb", quiet = TRUE),
        error = function(e) message("Failed: ", yr)
      )
    }
  }
}

fetch_index <- function(path) {
  index_url <- sprintf("https://data.everef.net/%s/index.json", path)
  fromJSON(index_url)$files
}

download_index_files <- function(files_df, dest_folder) {
  dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(files_df))) {
    dest_file <- file.path(dest_folder, files_df$name[i])
    if (!file.exists(dest_file)) {
      tryCatch(
        download.file(files_df$url[i], dest_file, mode = "wb", quiet = TRUE),
        error = function(e) message("Failed: ", files_df$name[i])
      )
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

## CCP Monthly Economic Report
download_dataset_window("ccp/mer", 2018, 2022, "data/raw/mer/window_b")

files <- fetch_year_files("wars/history", 2013)
str(files)

## Wars
download_wars_yearly(2011:2015, "data/raw/wars/window_a")

download_wars_yearly(2018:2020, "data/raw/wars/window_b")
download_dataset_window("wars/history", 2021, 2022, "data/raw/wars/window_b")

## CCP Static Data Export
download_index_files(fetch_index("ccp/sde/older"), "data/raw/sde/window_a")

download_dataset_window("ccp/sde", 2018, 2022, "data/raw/sde/window_b")
