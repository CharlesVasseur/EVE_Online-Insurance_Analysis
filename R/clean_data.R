library(jsonlite)
library(dplyr)
library(readr)
library(yaml)
library(purrr)
library(parallel)
library(data.table)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

# Killmails

parse_tar_bytes <- function(raw_bytes) {
  pos <- 1L
  n <- length(raw_bytes)
  out <- vector("list", 0)
  while (pos + 511L <= n) {
    header <- raw_bytes[pos:(pos + 511L)]
    if (all(header == as.raw(0))) break  # end-of-archive marker
    
    name_raw <- header[1:100]
    name <- rawToChar(name_raw[name_raw != as.raw(0)])
    size_raw <- header[125:136]
    size_str <- trimws(rawToChar(size_raw[size_raw != as.raw(0)]))
    size <- suppressWarnings(strtoi(size_str, base = 8L))
    typeflag <- rawToChar(header[157])
    pos <- pos + 512L
    
    if (!is.na(size) && size > 0 && typeflag %in% c("0", "")) {
      out[[name]] <- raw_bytes[pos:(pos + size - 1L)]
    }
    if (!is.na(size) && size > 0) pos <- pos + ceiling(size / 512) * 512L
  }
  out
}

parse_killmail_raw <- function(raw_content) {
  km <- tryCatch(fromJSON(rawToChar(raw_content)), error = function(e) NULL)
  if (is.null(km)) return(NULL)
  list(
    killmail_id = km$killmail_id,
    killmail_time = km$killmail_time,
    solar_system_id = km$solar_system_id,
    victim_character_id = km$victim$character_id %||% NA,
    victim_corporation_id = km$victim$corporation_id %||% NA,
    victim_ship_type_id = km$victim$ship_type_id,
    n_attackers = if (!is.null(km$attackers)) nrow(km$attackers) else NA
  )
}

clean_killmails_archive <- function(archive_path) {
  compressed <- readBin(archive_path, "raw", n = file.info(archive_path)$size)
  raw_tar <- memDecompress(compressed, type = "bzip2")
  entries <- parse_tar_bytes(raw_tar)
  json_entries <- entries[grepl("\\.json$", names(entries))]
  rbindlist(lapply(json_entries, parse_killmail_raw), fill = TRUE)
}

clean_killmails_window <- function(raw_folder, out_file, n_workers = max(1, detectCores() - 2), batch_size = n_workers * 2) {
  archives <- list.files(raw_folder, full.names = TRUE)
  done_log <- paste0(out_file, ".done.txt")
  done <- if (file.exists(done_log)) readLines(done_log) else character(0)
  remaining <- archives[!basename(archives) %in% done]
  
  if (length(remaining) == 0) { message("Already complete."); return(invisible(NULL)) }
  message(sprintf("%d of %d archives remaining. Using %d cores.", length(remaining), length(archives), n_workers))
  
  cl <- makeCluster(n_workers)
  clusterEvalQ(cl, { library(jsonlite); library(data.table) })
  clusterExport(cl, c("parse_tar_bytes", "parse_killmail_raw", "clean_killmails_archive", "%||%"))
  on.exit(stopCluster(cl))
  
  file_exists_already <- file.exists(out_file)
  batches <- split(remaining, ceiling(seq_along(remaining) / batch_size))
  
  for (batch in batches) {
    t0 <- Sys.time()
    results <- parLapply(cl, batch, clean_killmails_archive)
    batch_data <- rbindlist(results, fill = TRUE)
    fwrite(batch_data, out_file, append = file_exists_already)
    file_exists_already <- TRUE
    cat(basename(batch), file = done_log, sep = "\n", append = TRUE)
    message(sprintf("Batch of %d done in %.1fs (%d rows so far written)",
                    length(batch), as.numeric(Sys.time() - t0, units = "secs"), nrow(batch_data)))
  }
  message("Done: ", out_file)
}

km_a <- clean_killmails_window("data/raw/killmails/window_a", "data/processed/killmails_window_a.csv")
km_b <- clean_killmails_window("data/raw/killmails/window_b", "data/processed/killmails_window_b.csv")

# Market History

read_market_history_file <- function(path) {
  read_csv(path, show_col_types = FALSE)
}

clean_market_history <- function(raw_folder, out_file) {
  files <- list.files(raw_folder, full.names = TRUE)
  message(sprintf("Reading %d market history files...", length(files)))
  all_data <- map_dfr(files, read_market_history_file)
  write_csv(all_data, out_file)
  message(sprintf("Wrote %d rows to %s", nrow(all_data), out_file))
  all_data
}

mh_a <- clean_market_history("data/raw/market-history/window_a", "data/processed/market_history_window_a.csv")
mh_b <- clean_market_history("data/raw/market-history/window_b", "data/processed/market_history_window_b.csv")

file.remove(list.files("data/processed", pattern = "\\.done\\.txt$", full.names = TRUE))

files <- list.files("data/processed", recursive = TRUE, full.names = TRUE)
sizes <- data.frame(file = files, size_MB = round(file.info(files)$size / 1e6, 1))
sizes[order(-sizes$size_MB), ]

compress_file <- function(in_path, out_path, chunk_lines = 200000) {
  in_con <- file(in_path, "r")
  out_con <- gzfile(out_path, "w")
  repeat {
    lines <- readLines(in_con, n = chunk_lines)
    if (length(lines) == 0) break
    writeLines(lines, out_con)
  }
  close(in_con); close(out_con)
}

compress_file("data/processed/killmails_window_a.csv", "data/processed/killmails_window_a.csv.gz")
compress_file("data/processed/killmails_window_b.csv", "data/processed/killmails_window_b.csv.gz")
compress_file("data/processed/market_history_window_a.csv", "data/processed/market_history_window_a.csv.gz")
compress_file("data/processed/market_history_window_b.csv", "data/processed/market_history_window_b.csv.gz")
