# Prepare BTS Airline On-Time Performance data for the local Shiny app.
#
# The app uses a compact, GitHub-friendly monthly sample of BTS Reporting
# Carrier On-Time Performance records from 2023 through 2025. The full raw
# monthly archives are large, so this script downloads them from BTS PREZIP as
# needed and writes only the processed sample used by the app.

prep_bts_data <- function(
  start_year = 2023,
  end_year = 2025,
  sample_months = 1:12,
  max_rows_per_month = as.numeric(Sys.getenv("BTS_MAX_ROWS_PER_MONTH", "1000")),
  cache_raw = identical(tolower(Sys.getenv("BTS_CACHE_RAW", "false")), "true")
) {
options(timeout = max(getOption("timeout"), 300))

raw_dir <- file.path("data", "raw")
processed_dir <- file.path("data", "processed")
processed_path <- file.path(processed_dir, "bts_airline_flights_2023_2025_sample.csv")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

major_airports <- c(
  "ATL", "DFW", "DEN", "ORD", "LAX", "JFK", "SFO", "SEA", "LAS",
  "MCO", "CLT", "PHX", "MIA", "EWR", "BOS", "DCA", "IAD", "BWI"
)

months_to_process <- expand.grid(
  year = seq.int(start_year, end_year),
  month = sample_months
)
months_to_process <- months_to_process[order(months_to_process$year, months_to_process$month), ]

base_url <- "https://transtats.bts.gov/PREZIP"
zip_file_name <- function(year, month) {
  sprintf(
    "On_Time_Reporting_Carrier_On_Time_Performance_1987_present_%d_%d.zip",
    year,
    month
  )
}

clean_names_base <- function(x) {
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)

  fixes <- c(
    dayof_month = "day_of_month",
    dayofmonth = "day_of_month",
    dayof_week = "day_of_week",
    dayofweek = "day_of_week",
    flight_date = "fl_date",
    crsdep_time = "crs_dep_time",
    crsdeptime = "crs_dep_time",
    airtime = "air_time",
    air_time = "air_time",
    nasdelay = "nas_delay",
    securitydelay = "security_delay",
    carrierdelay = "carrier_delay",
    weatherdelay = "weather_delay",
    lateaircraftdelay = "late_aircraft_delay"
  )

  ifelse(x %in% names(fixes), fixes[x], x)
}

standardize_names <- function(x) {
  cleaned <- if (requireNamespace("janitor", quietly = TRUE)) {
    janitor::make_clean_names(x)
  } else {
    clean_names_base(x)
  }

  clean_names_base(cleaned)
}

parse_bts_date <- function(x) {
  out <- suppressWarnings(as.Date(x))
  missing <- is.na(out)
  if (any(missing)) {
    out[missing] <- suppressWarnings(as.Date(x[missing], format = "%m/%d/%Y"))
  }
  out
}

pick_col <- function(df, aliases, default = NA) {
  for (nm in aliases) {
    if (nm %in% names(df)) {
      return(df[[nm]])
    }
  }
  rep(default, nrow(df))
}

download_or_find_zip <- function(year, month) {
  zip_name <- zip_file_name(year, month)
  local_zip <- file.path(raw_dir, zip_name)
  if (file.exists(local_zip)) {
    message("Using local raw file: ", local_zip)
    return(local_zip)
  }

  url <- paste(base_url, zip_name, sep = "/")
  target <- if (cache_raw) local_zip else tempfile(fileext = ".zip")
  message("Downloading ", url)
  tryCatch(
    {
      download.file(url, target, mode = "wb", quiet = TRUE)
      target
    },
    error = function(e) {
      warning("Skipping unavailable BTS file: ", zip_name, call. = FALSE)
      NA_character_
    }
  )
}

read_needed_columns <- function(zip_path) {
  zip_listing <- unzip(zip_path, list = TRUE)
  csv_name <- zip_listing$Name[grepl("\\.csv$", zip_listing$Name, ignore.case = TRUE)][1]
  if (is.na(csv_name)) {
    stop("No CSV found in ", zip_path, call. = FALSE)
  }

  csv_path <- unzip(zip_path, files = csv_name, exdir = tempdir(), overwrite = TRUE)
  on.exit(unlink(csv_path), add = TRUE)

  header <- read.csv(csv_path, nrows = 0, check.names = FALSE)
  clean_header <- standardize_names(names(header))

  aliases <- list(
    year = "year",
    month = "month",
    day_of_month = c("day_of_month", "dayof_month"),
    day_of_week = c("day_of_week", "dayof_week"),
    op_unique_carrier = c("op_unique_carrier", "reporting_airline", "unique_carrier", "carrier"),
    origin = "origin",
    dest = "dest",
    crs_dep_time = c("crs_dep_time", "crsdep_time"),
    dep_delay = c("dep_delay", "depdelay"),
    arr_delay = c("arr_delay", "arrdelay"),
    cancelled = "cancelled",
    diverted = "diverted",
    air_time = c("air_time", "airtime"),
    distance = "distance",
    carrier_delay = c("carrier_delay", "carrierdelay"),
    weather_delay = c("weather_delay", "weatherdelay"),
    nas_delay = c("nas_delay", "nasdelay"),
    security_delay = c("security_delay", "securitydelay"),
    late_aircraft_delay = c("late_aircraft_delay", "lateaircraftdelay"),
    fl_date = c("fl_date", "flight_date")
  )

  wanted_clean <- unique(unlist(lapply(aliases, function(x) x[x %in% clean_header][1])))
  wanted_clean <- wanted_clean[!is.na(wanted_clean)]
  col_classes <- rep("NULL", length(clean_header))
  col_classes[clean_header %in% wanted_clean] <- NA
  selected_cols <- is.na(col_classes) | col_classes != "NULL"

  raw <- read.csv(
    csv_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = col_classes
  )
  names(raw) <- clean_header[selected_cols]

  missing_core <- setdiff(
    c("year", "month", "day_of_month", "day_of_week", "origin", "dest", "fl_date"),
    names(raw)
  )
  if (length(missing_core) > 0) {
    stop("Missing expected BTS columns in ", basename(zip_path), ": ", paste(missing_core, collapse = ", "), call. = FALSE)
  }

  out <- data.frame(
    year = pick_col(raw, "year"),
    month = pick_col(raw, "month"),
    day_of_month = pick_col(raw, c("day_of_month", "dayof_month")),
    day_of_week = pick_col(raw, c("day_of_week", "dayof_week")),
    op_unique_carrier = pick_col(raw, c("op_unique_carrier", "reporting_airline", "unique_carrier", "carrier")),
    origin = pick_col(raw, "origin"),
    dest = pick_col(raw, "dest"),
    crs_dep_time = pick_col(raw, c("crs_dep_time", "crsdep_time")),
    dep_delay = pick_col(raw, c("dep_delay", "depdelay")),
    arr_delay = pick_col(raw, c("arr_delay", "arrdelay")),
    cancelled = pick_col(raw, "cancelled"),
    diverted = pick_col(raw, "diverted"),
    air_time = pick_col(raw, c("air_time", "airtime")),
    distance = pick_col(raw, "distance"),
    carrier_delay = pick_col(raw, c("carrier_delay", "carrierdelay"), 0),
    weather_delay = pick_col(raw, c("weather_delay", "weatherdelay"), 0),
    nas_delay = pick_col(raw, c("nas_delay", "nasdelay"), 0),
    security_delay = pick_col(raw, c("security_delay", "securitydelay"), 0),
    late_aircraft_delay = pick_col(raw, c("late_aircraft_delay", "lateaircraftdelay"), 0),
    fl_date = pick_col(raw, c("fl_date", "flight_date")),
    stringsAsFactors = FALSE
  )

  numeric_cols <- setdiff(names(out), c("op_unique_carrier", "origin", "dest", "fl_date"))
  out[numeric_cols] <- lapply(out[numeric_cols], function(x) suppressWarnings(as.numeric(x)))

  out$fl_date <- parse_bts_date(out$fl_date)
  missing_date <- is.na(out$fl_date) & !is.na(out$year) & !is.na(out$month) & !is.na(out$day_of_month)
  out$fl_date[missing_date] <- as.Date(sprintf(
    "%04d-%02d-%02d",
    out$year[missing_date],
    out$month[missing_date],
    out$day_of_month[missing_date]
  ))

  out
}

derive_fields <- function(df) {
  cause_cols <- c(
    "carrier_delay", "weather_delay", "nas_delay",
    "security_delay", "late_aircraft_delay"
  )

  for (nm in cause_cols) {
    df[[nm]][is.na(df[[nm]])] <- 0
  }

  df$carrier <- df$op_unique_carrier
  df$crs_dep_hour <- floor(df$crs_dep_time / 100)
  df$crs_dep_hour[df$crs_dep_hour < 0 | df$crs_dep_hour > 23] <- NA
  df$cancelled_flag <- df$cancelled == 1
  df$diverted_flag <- df$diverted == 1
  df$dep_delayed_15 <- !is.na(df$dep_delay) & df$dep_delay >= 15
  df$arr_delayed_15 <- !is.na(df$arr_delay) & df$arr_delay >= 15
  df$route <- paste(df$origin, df$dest, sep = "-")
  df$total_cause_delay <- rowSums(df[cause_cols], na.rm = TRUE)
  df$month_start <- as.Date(sprintf("%04d-%02d-01", df$year, df$month))
  df$year_month <- format(df$month_start, "%Y-%m")
  df$quarter <- paste0("Q", ceiling(df$month / 3))
  df$month_name <- factor(month.abb[df$month], levels = month.abb)
  df$season <- factor(
    ifelse(df$month %in% c(12, 1, 2), "Winter",
      ifelse(df$month %in% c(3, 4, 5), "Spring",
        ifelse(df$month %in% c(6, 7, 8), "Summer", "Fall")
      )
    ),
    levels = c("Winter", "Spring", "Summer", "Fall")
  )
  df$weekend_flag <- df$day_of_week %in% c(6, 7)
  df$time_of_day <- factor(
    ifelse(df$crs_dep_hour < 6, "Overnight",
      ifelse(df$crs_dep_hour < 12, "Morning",
        ifelse(df$crs_dep_hour < 18, "Afternoon", "Evening")
      )
    ),
    levels = c("Overnight", "Morning", "Afternoon", "Evening")
  )
  df$distance_band <- cut(
    df$distance,
    breaks = c(-Inf, 500, 1000, 1500, 2500, Inf),
    labels = c("0-500", "501-1000", "1001-1500", "1501-2500", "2500+"),
    right = TRUE
  )

  df
}

filter_app_rows <- function(df) {
  major_airport_row <- df$origin %in% major_airports | df$dest %in% major_airports
  date_row <- df$fl_date >= as.Date(sprintf("%d-01-01", start_year)) &
    df$fl_date <= as.Date(sprintf("%d-12-31", end_year))

  df[major_airport_row & date_row, , drop = FALSE]
}

sample_month <- function(df) {
  if (!is.finite(max_rows_per_month) || nrow(df) <= max_rows_per_month) {
    return(df)
  }

  df[sample.int(nrow(df), max_rows_per_month), , drop = FALSE]
}

message("Preparing BTS Airline Operations Explorer data...")
set.seed(6395)

processed_parts <- list()
for (i in seq_len(nrow(months_to_process))) {
  yr <- months_to_process$year[i]
  mo <- months_to_process$month[i]
  zip_path <- download_or_find_zip(yr, mo)

  if (is.na(zip_path)) {
    next
  }

  month_df <- read_needed_columns(zip_path)
  month_df <- filter_app_rows(month_df)
  month_df <- sample_month(month_df)
  processed_parts[[length(processed_parts) + 1]] <- month_df
  message("  ", yr, "-", sprintf("%02d", mo), ": ", nrow(month_df), " sampled rows")
}

if (length(processed_parts) == 0) {
  stop("No BTS monthly files were processed.", call. = FALSE)
}

processed <- do.call(rbind, processed_parts)
processed <- derive_fields(processed)
processed <- processed[order(processed$fl_date, processed$origin, processed$dest), , drop = FALSE]
rownames(processed) <- NULL

if (nrow(processed) < 5000) {
  stop("Processed data has fewer than 5,000 rows: ", nrow(processed), call. = FALSE)
}

write.csv(processed, processed_path, row.names = FALSE, na = "")

message("Wrote ", nrow(processed), " rows to ", processed_path)
}
