library(jsonlite)
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

clean_killmails_window("data/raw/killmails/window_a", "data/processed/killmails_window_a.csv")
clean_killmails_window("data/raw/killmails/window_b", "data/processed/killmails_window_b.csv")

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
  invisible(NULL)
}

clean_market_history("data/raw/market-history/window_a", "data/processed/market_history_window_a.csv")
clean_market_history("data/raw/market-history/window_b", "data/processed/market_history_window_b.csv")

files <- list.files("data/processed", recursive = TRUE, full.names = TRUE)
sizes <- data.frame(file = files, size_MB = round(file.info(files)$size / 1e6, 1))
sizes[order(-sizes$size_MB), ]

# CCP SDE

big_int_handler <- function(x) {
  val <- suppressWarnings(as.integer(x))
  if (is.na(val)) as.numeric(x) else val
}

read_yaml_safe <- function(path) yaml::read_yaml(path, handlers = list(int = big_int_handler))

extract_sde_snapshot <- function(zip_path, out_prefix, tmp_dir = "data/raw/_tmp_sde") {
  dir.create(dirname(out_prefix), recursive = TRUE, showWarnings = FALSE)
  unlink(tmp_dir, recursive = TRUE)
  all_names <- unzip(zip_path, list = TRUE)$Name
  
  bp_path <- grep("(^|/)blueprints\\.yaml$", all_names, value = TRUE)[1]
  types_path <- grep("(^|/)typeIDs\\.yaml$", all_names, value = TRUE)[1]
  
  unzip(zip_path, files = c(bp_path, types_path), exdir = tmp_dir)
  saveRDS(read_yaml_safe(file.path(tmp_dir, bp_path)), paste0(out_prefix, "_blueprints.rds"))
  saveRDS(read_yaml_safe(file.path(tmp_dir, types_path)), paste0(out_prefix, "_types.rds"))
  unlink(tmp_dir, recursive = TRUE)
  message("Saved SDE snapshot: ", out_prefix)
}

extract_sde_snapshot("data/raw/sde/window_a/Crius_1.0_beta3.zip", "data/processed/sde/window_a_2014")

sde_b_files <- list.files("data/raw/sde/window_b", full.names = TRUE)

dates_only <- regmatches(basename(sde_b_files), regexpr("\\d{8}", basename(sde_b_files)))
sum(duplicated(dates_only))

for (f in sde_b_files) {
  id_str <- gsub("^sde-|-TRANQUILITY\\.zip$", "", basename(f))
  extract_sde_snapshot(f, sprintf("data/processed/sde/window_b_%s", id_str))
}

# Wars

parse_tar_bytes_wars <- function(raw_bytes) {
  pos <- 1L; n <- length(raw_bytes)
  cap <- 1000L
  names_vec <- character(cap)
  contents_vec <- vector("list", cap)
  count <- 0L
  
  while (pos + 511L <= n) {
    header <- raw_bytes[pos:(pos + 511L)]
    if (all(header == as.raw(0))) break
    
    name_raw <- header[1:100]
    name <- rawToChar(name_raw[name_raw != as.raw(0)])
    size_raw <- header[125:136]
    size_str <- trimws(rawToChar(size_raw[size_raw != as.raw(0)]))
    size <- suppressWarnings(strtoi(size_str, base = 8L))
    typeflag <- rawToChar(header[157])
    pos <- pos + 512L
    
    if (!is.na(size) && size > 0 && typeflag %in% c("0", "")) {
      count <- count + 1L
      if (count > cap) {
        cap <- cap * 2L
        length(names_vec) <- cap
        length(contents_vec) <- cap
      }
      names_vec[count] <- name
      contents_vec[[count]] <- raw_bytes[pos:(pos + size - 1L)]
    }
    if (!is.na(size) && size > 0) pos <- pos + ceiling(size / 512) * 512L
  }
  
  contents_vec <- contents_vec[seq_len(count)]
  names(contents_vec) <- names_vec[seq_len(count)]
  contents_vec
}

clean_wars_archive_fast <- function(archive_path) {
  compressed <- readBin(archive_path, "raw", n = file.info(archive_path)$size)
  raw_tar <- memDecompress(compressed, type = "bzip2")
  entries <- parse_tar_bytes_wars(raw_tar)
  
  json_entries <- entries[grepl("\\.json$", names(entries)) & !grepl("/killmails/", names(entries))]
  
  rbindlist(lapply(json_entries, function(raw_content) {
    w <- tryCatch(fromJSON(rawToChar(raw_content)), error = function(e) NULL)
    if (is.null(w)) return(NULL)
    list(
      war_id = w$id, declared = w$declared, started = w$started,
      finished = w$finished %||% NA,
      aggressor_isk_destroyed = w$aggressor$isk_destroyed %||% NA,
      defender_isk_destroyed = w$defender$isk_destroyed %||% NA,
      aggressor_ships_killed = w$aggressor$ships_killed %||% NA,
      defender_ships_killed = w$defender$ships_killed %||% NA,
      last_modified = w$http_last_modified %||% NA
    )
  }), fill = TRUE)
}

clean_wars_window <- function(raw_folder, out_file) {
  archives <- list.files(raw_folder, full.names = TRUE)
  done_log <- paste0(out_file, ".done.txt")
  done <- if (file.exists(done_log)) readLines(done_log) else character(0)
  remaining <- archives[!basename(archives) %in% done]
  
  if (length(remaining) == 0) { message("Already complete."); return(invisible(NULL)) }
  
  file_exists_already <- file.exists(out_file) && length(done) > 0
  for (a in remaining) {
    t0 <- Sys.time()
    result <- clean_wars_archive_fast(a)
    fwrite(result, out_file, append = file_exists_already)
    file_exists_already <- TRUE
    cat(basename(a), file = done_log, sep = "\n", append = TRUE)
    message(sprintf("%s: %d rows in %.1fs", basename(a), nrow(result), as.numeric(Sys.time() - t0, units = "secs")))
  }
  message("Done: ", out_file)
}

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

clean_wars_window("data/raw/wars/window_a", "data/processed/wars_window_a_raw.csv")

wars_a_raw <- fread("data/processed/wars_window_a_raw.csv")
setorder(wars_a_raw, war_id, -last_modified)
wars_a <- wars_a_raw[!is.na(war_id)][!duplicated(war_id)]
fwrite(wars_a, "data/processed/wars_window_a.csv")

clean_wars_window("data/raw/wars/window_b", "data/processed/wars_window_b_raw.csv")

wars_b_raw <- fread("data/processed/wars_window_b_raw.csv")
setorder(wars_b_raw, war_id, -last_modified)
wars_b <- wars_b_raw[!is.na(war_id)][!duplicated(war_id)]
fwrite(wars_b, "data/processed/wars_window_b.csv")

# CCP MER

find_sinks_file <- function(mer_zip) {
  contents <- unzip(mer_zip, list = TRUE)$Name
  candidates <- c(
    grep("(^|/)TopSinksFaucetsOverTime\\.csv$", contents, value = TRUE),
    grep("(^|/)sinks_and_faucets_over_time\\.csv$", contents, value = TRUE),
    grep("(^|/)sinks_and_faucets_history\\.csv$", contents, value = TRUE)
  )
  if (length(candidates) == 0) return(NA)
  candidates[1]
}

tmp <- tempdir()
unzip("data/raw/mer/window_b/EVEOnline_MER_Apr2021.zip", files = "sinks_and_faucets_over_time.csv", exdir = tmp)
df21 <- fread(file.path(tmp, "sinks_and_faucets_over_time.csv"))
unique(grep("insur", df21$entry_name, ignore.case = TRUE, value = TRUE))

year_file_picks <- c(
  "2018" = "EVEOnline_MER_Dec2018.zip",
  "2019" = "EVEOnline_MER_Dec2019b.zip",
  "2020" = "EVEOnline_MER_Dec2020.zip",
  "2021" = "EVEOnline_MER_Dec2021_Updated.zip",
  "2022" = "EVEOnline_MER_Dec2022.zip"
)

mer_dir <- "data/raw/mer/window_b"  # adjust if your raw MER files live elsewhere

insurance_list <- list()

for (yr in rev(names(year_file_picks))) {
  f <- file.path(mer_dir, year_file_picks[yr])
  if (!file.exists(f)) {
    warning(sprintf("Expected file not found for %s: %s", yr, f))
    next
  }
  fname <- find_sinks_file(f)
  tmp <- tempdir()
  unzip(f, files = fname, exdir = tmp)
  df <- fread(file.path(tmp, fname))
  
  if ("keyText" %in% names(df)) {
    sub <- df[grepl("insur", keyText, ignore.case = TRUE),
              .(date = as.Date(date), value = as.numeric(value))]
  } else {
    sub <- df[grepl("insur", entry_name, ignore.case = TRUE),
              .(date = as.Date(history_date),
                value = as.numeric(entry_faucet_value) + as.numeric(entry_sink_value))]
  }
  sub[, source_year_file := yr]
  message(sprintf("%s (%s): %d insurance rows found, date range %s to %s",
                  yr, year_file_picks[yr], nrow(sub), min(sub$date), max(sub$date)))
  insurance_list[[yr]] <- sub
}

insurance_all <- rbindlist(insurance_list, fill = TRUE)
setorder(insurance_all, date)
insurance_final <- unique(insurance_all, by = "date")
setorder(insurance_final, date)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
fwrite(insurance_final[, .(date, value)], "data/processed/mer_insurance_window_b.csv")

file.remove(list.files("data/processed", pattern = "\\.done\\.txt$", full.names = TRUE, recursive = TRUE))