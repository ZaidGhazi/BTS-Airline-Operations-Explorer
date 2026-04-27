# BTS Airline Operations Explorer
#
# This local Shiny app supports exploratory analysis of BTS Airline On-Time
# Performance records. It lets users filter flights in natural language,
# build a plot, and ask a cautious multimodal coach to interpret the plot.

required_packages <- c(
  "shiny", "bslib", "querychat", "ellmer", "shinychat",
  "ggplot2", "dplyr", "readr", "tidyr", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install required packages before running the app:\n",
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    ', "duckdb"))',
    call. = FALSE
  )
}

library(shiny)
library(bslib)
library(querychat)
library(ellmer)
library(shinychat)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)

HAIKU_MODEL <- "claude-haiku-4-5"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(as.character(x)[1])) {
    y
  } else {
    x
  }
}

data_path <- file.path("data", "processed", "bts_airline_flights_2023_2025_sample.csv")
greeting_path <- "greeting.md"
dictionary_path <- file.path("data", "bts_data_dictionary.md")
querychat_instructions_path <- "querychat_instructions.md"

if (!file.exists(data_path)) {
  stop(
    "Missing processed data at ", data_path, ". ",
    "Run source('R/prep_bts_data.R'); prep_bts_data() first.",
    call. = FALSE
  )
}

querychat_context_paths <- c(greeting_path, dictionary_path, querychat_instructions_path)
missing_context_paths <- querychat_context_paths[!file.exists(querychat_context_paths)]
if (length(missing_context_paths) > 0) {
  stop(
    "Missing QueryChat context file(s): ",
    paste(missing_context_paths, collapse = ", "),
    call. = FALSE
  )
}

raw_flights <- read_csv(data_path, show_col_types = FALSE, progress = FALSE)

cause_cols <- c(
  "carrier_delay", "weather_delay", "nas_delay",
  "security_delay", "late_aircraft_delay"
)

base_required_cols <- c(
  "year", "month", "day_of_month", "day_of_week", "op_unique_carrier",
  "origin", "dest", "crs_dep_time", "dep_delay", "arr_delay",
  "cancelled", "diverted", "air_time", "distance", "fl_date"
)

missing_base_cols <- setdiff(base_required_cols, names(raw_flights))
if (length(missing_base_cols) > 0) {
  stop(
    "The processed data is missing required columns: ",
    paste(missing_base_cols, collapse = ", "),
    call. = FALSE
  )
}

startup_warnings <- character()
missing_cause_cols <- setdiff(cause_cols, names(raw_flights))
if (length(missing_cause_cols) > 0) {
  startup_warnings <- c(
    startup_warnings,
    paste(
      "Missing delay-cause columns were filled with zero:",
      paste(missing_cause_cols, collapse = ", ")
    )
  )
  for (nm in missing_cause_cols) {
    raw_flights[[nm]] <- 0
  }
}

prepare_app_data <- function(df) {
  # Recompute derived variables here so the app remains robust if the processed
  # CSV is regenerated from raw BTS files with only the core source columns.
  season_levels <- c("Winter", "Spring", "Summer", "Fall")
  time_levels <- c("Overnight", "Morning", "Afternoon", "Evening")

  df %>%
    mutate(
      fl_date = as.Date(fl_date),
      across(all_of(cause_cols), ~ replace_na(as.numeric(.x), 0)),
      carrier = op_unique_carrier,
      crs_dep_hour = floor(as.numeric(crs_dep_time) / 100),
      crs_dep_hour = if_else(crs_dep_hour >= 0 & crs_dep_hour <= 23, crs_dep_hour, NA_real_),
      cancelled_flag = as.numeric(cancelled) == 1,
      diverted_flag = as.numeric(diverted) == 1,
      dep_delayed_15 = !is.na(dep_delay) & dep_delay >= 15,
      arr_delayed_15 = !is.na(arr_delay) & arr_delay >= 15,
      route = paste(origin, dest, sep = "-"),
      total_cause_delay = carrier_delay + weather_delay + nas_delay +
        security_delay + late_aircraft_delay,
      month_start = as.Date(sprintf("%04d-%02d-01", year, month)),
      year_month = format(month_start, "%Y-%m"),
      quarter = paste0("Q", ceiling(month / 3)),
      month_name = factor(month.abb[month], levels = month.abb),
      season = factor(
        case_when(
          month %in% c(12, 1, 2) ~ "Winter",
          month %in% c(3, 4, 5) ~ "Spring",
          month %in% c(6, 7, 8) ~ "Summer",
          month %in% c(9, 10, 11) ~ "Fall",
          TRUE ~ NA_character_
        ),
        levels = season_levels
      ),
      weekend_flag = day_of_week %in% c(6, 7),
      time_of_day = factor(
        case_when(
          crs_dep_hour < 6 ~ "Overnight",
          crs_dep_hour < 12 ~ "Morning",
          crs_dep_hour < 18 ~ "Afternoon",
          crs_dep_hour <= 23 ~ "Evening",
          TRUE ~ NA_character_
        ),
        levels = time_levels
      ),
      distance_band = cut(
        distance,
        breaks = c(-Inf, 500, 1000, 1500, 2500, Inf),
        labels = c("0-500", "501-1000", "1001-1500", "1501-2500", "2500+"),
        right = TRUE
      )
    ) %>%
    filter(!is.na(fl_date))
}

flights <- prepare_app_data(raw_flights)

if (nrow(flights) < 5000) {
  startup_warnings <- c(
    startup_warnings,
    paste("Processed data has only", nrow(flights), "rows; the app is intended for samples of at least 5,000 rows.")
  )
}

if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
  startup_warnings <- c(
    startup_warnings,
    "ANTHROPIC_API_KEY is not set; QueryChat and plot interpretation need it."
  )
}

# QueryChat context:
# - greeting.md addresses the custom greeting requirement.
# - data/bts_data_dictionary.md and querychat_instructions.md address the
#   data-specific context used by QueryChat.
# - Storing the greeting as a file follows the QueryChat docs and avoids
#   regenerating greeting text for every app session.
qc <- querychat::QueryChat$new(
  flights,
  table_name = "flights",
  client = ellmer::chat_anthropic(model = HAIKU_MODEL),
  greeting = greeting_path,
  data_description = dictionary_path,
  extra_instructions = querychat_instructions_path
)

plot_field_choices <- c(
  "Flight date" = "fl_date",
  "Month" = "month_start",
  "Year" = "year",
  "Quarter" = "quarter",
  "Season" = "season",
  "Weekend" = "weekend_flag",
  "Time of day" = "time_of_day",
  "Carrier" = "carrier",
  "Origin airport" = "origin",
  "Destination airport" = "dest",
  "Route" = "route",
  "Month number" = "month",
  "Month label" = "month_name",
  "Day of week" = "day_of_week",
  "Scheduled departure hour" = "crs_dep_hour",
  "Distance band" = "distance_band",
  "Distance" = "distance",
  "Air time" = "air_time",
  "Departure delay" = "dep_delay",
  "Arrival delay" = "arr_delay",
  "Carrier delay" = "carrier_delay",
  "Weather delay" = "weather_delay",
  "NAS delay" = "nas_delay",
  "Security delay" = "security_delay",
  "Late aircraft delay" = "late_aircraft_delay",
  "Total cause delay" = "total_cause_delay"
)

y_field_choices <- c(
  "Flight count" = ".count",
  "Delay rate (dep. or arr. 15+ min)" = ".delay_rate",
  "Cancellation rate" = ".cancel_rate",
  "Median departure delay" = ".median_dep_delay",
  "Median arrival delay" = ".median_arr_delay",
  "Flight date" = "fl_date",
  "Month" = "month_start",
  "Year" = "year",
  "Quarter" = "quarter",
  "Season" = "season",
  "Weekend" = "weekend_flag",
  "Time of day" = "time_of_day",
  "Carrier" = "carrier",
  "Origin airport" = "origin",
  "Destination airport" = "dest",
  "Route" = "route",
  "Month number" = "month",
  "Month label" = "month_name",
  "Day of week" = "day_of_week",
  "Scheduled departure hour" = "crs_dep_hour",
  "Distance band" = "distance_band",
  "Distance" = "distance",
  "Air time" = "air_time",
  "Departure delay" = "dep_delay",
  "Arrival delay" = "arr_delay",
  "Carrier delay" = "carrier_delay",
  "Weather delay" = "weather_delay",
  "NAS delay" = "nas_delay",
  "Security delay" = "security_delay",
  "Late aircraft delay" = "late_aircraft_delay",
  "Total cause delay" = "total_cause_delay"
)

color_choices <- c(
  "None" = ".none",
  "Year" = "year",
  "Quarter" = "quarter",
  "Season" = "season",
  "Weekend" = "weekend_flag",
  "Time of day" = "time_of_day",
  "Distance band" = "distance_band",
  "Carrier" = "carrier",
  "Origin airport" = "origin",
  "Destination airport" = "dest",
  "Month number" = "month",
  "Day of week" = "day_of_week",
  "Cancelled" = "cancelled_flag",
  "Departed 15+ min late" = "dep_delayed_15",
  "Arrived 15+ min late" = "arr_delayed_15"
)

geom_choices <- c(
  "Bars" = "bar",
  "Line" = "line",
  "Points" = "point",
  "Points + smooth" = "point_smooth",
  "Boxplot" = "boxplot",
  "Heatmap" = "heatmap"
)

summary_choices <- c(
  "Mean" = "mean",
  "Median" = "median",
  "Sum" = "sum"
)

safe_percent <- function(x, accuracy = 0.1) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) {
    "NA"
  } else {
    percent(x, accuracy = accuracy)
  }
}

safe_number <- function(x, suffix = "", accuracy = 0.1) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) {
    "NA"
  } else {
    paste0(number(x, accuracy = accuracy), suffix)
  }
}

empty_plot <- function(message) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 5, color = "#475569") +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void(base_size = 13)
}

label_for_value <- function(value, choices) {
  label <- names(choices)[unname(choices) == value]
  if (length(label) == 0) value else label[[1]]
}

is_plot_numeric <- function(df, var) {
  var %in% names(df) && is.numeric(df[[var]]) && !inherits(df[[var]], "Date")
}

is_plot_continuous_x <- function(df, var) {
  var %in% names(df) && (is.numeric(df[[var]]) || inherits(df[[var]], "Date"))
}

top_categories <- function(df, var, n = 25) {
  df %>%
    filter(!is.na(.data[[var]])) %>%
    count(.data[[var]], sort = TRUE, name = "rows") %>%
    slice_head(n = n) %>%
    pull(.data[[var]])
}

limit_plot_categories <- function(df, x, color = ".none", max_x = 25, max_color = 10) {
  # High-cardinality fields like route can overwhelm a plot. Limit to the
  # most common categories while preserving the user's filtered data.
  if (x %in% names(df) && !is_plot_continuous_x(df, x) && n_distinct(df[[x]], na.rm = TRUE) > max_x) {
    df <- df %>% filter(.data[[x]] %in% top_categories(df, x, max_x))
  }

  if (color %in% names(df) && !is_plot_continuous_x(df, color) &&
      n_distinct(df[[color]], na.rm = TRUE) > max_color) {
    df <- df %>% filter(.data[[color]] %in% top_categories(df, color, max_color))
  }

  df
}

summarise_y <- function(df, x, y, color = ".none", summary_stat = "mean") {
  group_cols <- c(x, if (color != ".none") color)

  df <- df %>% filter(!is.na(.data[[x]]))
  if (color != ".none") {
    df <- df %>% filter(!is.na(.data[[color]]))
  }

  if (y == ".count") {
    out <- df %>% group_by(across(all_of(group_cols))) %>% summarise(value = n(), .groups = "drop")
  } else if (y == ".delay_rate") {
    out <- df %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(value = mean(dep_delayed_15 | arr_delayed_15, na.rm = TRUE), .groups = "drop")
  } else if (y == ".cancel_rate") {
    out <- df %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(value = mean(cancelled_flag, na.rm = TRUE), .groups = "drop")
  } else if (y == ".median_dep_delay") {
    out <- df %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(value = median(dep_delay, na.rm = TRUE), .groups = "drop")
  } else if (y == ".median_arr_delay") {
    out <- df %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(value = median(arr_delay, na.rm = TRUE), .groups = "drop")
  } else {
    out <- df %>%
      filter(!is.na(.data[[y]])) %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(
        value = case_when(
          summary_stat == "median" ~ median(.data[[y]], na.rm = TRUE),
          summary_stat == "sum" ~ sum(.data[[y]], na.rm = TRUE),
          TRUE ~ mean(.data[[y]], na.rm = TRUE)
        ),
        .groups = "drop"
      )
  }

  out %>% filter(!is.na(value), is.finite(value))
}

build_user_plot <- function(df, x, y, color, geom, summary_stat) {
  if (nrow(df) == 0) {
    return(empty_plot("No matching flights. Try broadening the QueryChat filter."))
  }

  if (!x %in% names(df)) {
    return(empty_plot("Choose an X variable from the dataset."))
  }

  has_color <- color != ".none" && color %in% names(df)
  y_label <- label_for_value(y, y_field_choices)
  x_label <- label_for_value(x, plot_field_choices)
  color_label <- if (has_color) label_for_value(color, color_choices) else NULL
  base_theme <- theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      axis.text.x = element_text(angle = 25, hjust = 1)
    )

  df <- limit_plot_categories(df, x, if (has_color) color else ".none")

  if (geom %in% c("point", "point_smooth", "boxplot") && !is_plot_numeric(df, y)) {
    return(empty_plot("For points, smooths, or boxplots, choose a numeric Y variable such as dep_delay, arr_delay, distance, or nas_delay."))
  }

  if (geom %in% c("bar", "line") && y %in% names(df) && !is_plot_numeric(df, y)) {
    return(empty_plot("For bars or lines, choose Flight count, a rate, a median delay, or a numeric Y variable."))
  }

  if (geom == "boxplot" && is_plot_continuous_x(df, x)) {
    return(empty_plot("For a boxplot, choose a categorical X variable such as carrier, origin, season, or time of day."))
  }

  if (geom == "heatmap") {
    if (is_plot_continuous_x(df, x) || !y %in% names(df) || is_plot_continuous_x(df, y)) {
      return(empty_plot("For a heatmap, choose categorical X and Y fields, such as season and carrier."))
    }

    heat_df <- df %>%
      filter(!is.na(.data[[x]]), !is.na(.data[[y]])) %>%
      group_by(across(all_of(c(x, y)))) %>%
      summarise(flights = n(), .groups = "drop")

    if (nrow(heat_df) == 0) {
      return(empty_plot("No category pairs are available for this heatmap."))
    }

    return(
      ggplot(heat_df, aes(.data[[x]], .data[[y]], fill = flights)) +
        geom_tile(color = "white", linewidth = 0.35) +
        scale_fill_gradient(low = "#e0f2fe", high = "#b91c1c", labels = comma) +
        labs(
          title = paste("Flight count by", x_label, "and", label_for_value(y, plot_field_choices)),
          subtitle = paste("Showing", comma(nrow(df)), "filtered rows; large category fields are limited to top values."),
          x = x_label,
          y = label_for_value(y, plot_field_choices),
          fill = "Flights"
        ) +
        base_theme
    )
  }

  if (geom %in% c("point", "point_smooth")) {
    point_df <- df %>%
      filter(!is.na(.data[[x]]), !is.na(.data[[y]]))

    if (nrow(point_df) == 0) {
      return(empty_plot("No rows have both selected X and Y values."))
    }

    if (nrow(point_df) > 5000) {
      point_df <- point_df %>% slice_sample(n = 5000)
    }

    p <- ggplot(point_df, aes(.data[[x]], .data[[y]]))
    if (has_color) {
      p <- p + aes(color = .data[[color]])
    }
    p <- p +
      geom_point(alpha = 0.45, size = 1.6, position = position_jitter(width = 0.08, height = 0)) +
      labs(
        title = paste(y_label, "by", x_label),
        subtitle = paste("Showing up to 5,000 plotted points from", comma(nrow(df)), "filtered rows."),
        x = x_label,
        y = y_label,
        color = color_label
      ) +
      base_theme

    if (geom == "point_smooth" && is_plot_numeric(point_df, x)) {
      p <- p + geom_smooth(se = FALSE, linewidth = 0.8)
    }

    return(p)
  }

  if (geom == "boxplot") {
    box_df <- df %>% filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
    if (nrow(box_df) == 0) {
      return(empty_plot("No rows have both selected X and Y values."))
    }

    p <- ggplot(box_df, aes(.data[[x]], .data[[y]]))
    if (has_color) {
      p <- p + aes(fill = .data[[color]])
    }

    return(
      p +
        geom_boxplot(outlier.alpha = 0.2) +
        labs(
          title = paste(y_label, "distribution by", x_label),
          subtitle = paste("Showing", comma(nrow(box_df)), "filtered rows."),
          x = x_label,
          y = y_label,
          fill = color_label
        ) +
        base_theme
    )
  }

  plot_df <- summarise_y(df, x, y, if (has_color) color else ".none", summary_stat)
  if (nrow(plot_df) == 0) {
    return(empty_plot("No summarized values are available for this plot."))
  }

  p <- ggplot(plot_df, aes(.data[[x]], value))
  if (has_color) {
    if (geom == "line") {
      p <- p + aes(color = .data[[color]], group = .data[[color]])
    } else {
      p <- p + aes(fill = .data[[color]])
    }
  }

  if (geom == "line") {
    p <- p +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.8)
  } else {
    p <- p + geom_col(position = "dodge", width = 0.72, color = NA)
  }

  p +
    labs(
      title = paste(y_label, "by", x_label),
      subtitle = paste("Summarized from", comma(nrow(df)), "filtered rows."),
      x = x_label,
      y = y_label,
      fill = if (has_color && geom != "line") color_label else NULL,
      color = if (has_color && geom == "line") color_label else NULL
    ) +
    scale_y_continuous(labels = if (y %in% c(".delay_rate", ".cancel_rate")) percent_format() else label_comma()) +
    base_theme
}

evaluation_cases <- data.frame(
  `Test case` = c(
    "Washington-area origin filter",
    "NAS-focused delay filter",
    "Carrier comparison",
    "Zero-row handling",
    "Route heatmap sanity check"
  ),
  `Filter query` = c(
    "Show flights from DCA, IAD, or BWI",
    "Keep flights with NAS delay greater than 30 minutes",
    "Only American, Delta, United, and Southwest",
    "Show flights from ZZZ",
    "Flights from ATL to DFW or DFW to ATL"
  ),
  `Suggested plot controls` = c(
    "X = Month; Y = Delay rate; Color = Year; Geom = Line",
    "X = Season; Y = NAS delay; Color = Season; Geom = Bars; Summary = Sum",
    "X = Carrier; Y = Delay rate; Color = Year; Geom = Bars",
    "X = Month; Y = Delay rate; Color = None; Geom = Line",
    "X = Time of day; Y = Route; Color = None; Geom = Heatmap"
  ),
  `Interpretation question` = c(
    "What seasonal delay pattern is visible?",
    "Which season has the most NAS delay minutes in this filtered view?",
    "Which carrier differences look largest?",
    "What should I do if no rows match?",
    "Does either direction look different by departure time?"
  ),
  check.names = FALSE
)

app_theme <- bs_theme(
  version = 5,
  primary = "#1f6f8b",
  secondary = "#475569",
  success = "#2f855a",
  danger = "#b91c1c"
)

app_css <- "
.navbar-brand { font-weight: 700; letter-spacing: 0; }
.explore-page { padding-top: 1rem; }
.welcome-card,
.main-query-card,
.control-card { margin-bottom: 1rem; }
.welcome-card,
.main-query-card,
.control-card,
.fold-panel { position: relative; z-index: 1; }
.welcome-card h4 {
  font-weight: 750;
  margin-bottom: 0.45rem;
}
.welcome-card p {
  max-width: 78rem;
  margin-bottom: 0.65rem;
  color: #334155;
}
.suggestion-row {
  display: flex;
  flex-wrap: wrap;
  gap: 0.45rem;
  margin: 0.75rem 0;
}
.suggestion-row .suggestion {
  display: inline-block;
  border: 1px solid #cbd5e1;
  border-radius: 999px;
  background: #f8fafc;
  color: #0f172a;
  font-size: 0.9rem;
  padding: 0.25rem 0.65rem;
}
.field-guide {
  border-top: 1px solid #e5e7eb;
  margin-top: 1rem;
  padding-top: 0.85rem;
}
.field-guide h5 {
  font-size: 0.98rem;
  font-weight: 750;
  margin-bottom: 0.65rem;
}
.field-guide-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 0.75rem;
}
.filter-grid .field-guide-grid {
  grid-template-columns: 1fr;
}
.field-guide-item {
  border: 1px solid #e2e8f0;
  border-radius: 6px;
  background: #f8fafc;
  padding: 0.65rem 0.75rem;
}
.field-guide-item strong {
  display: block;
  margin-bottom: 0.35rem;
  color: #0f172a;
}
.field-guide-item p {
  margin-bottom: 0;
  color: #475569;
  font-size: 0.9rem;
}
.field-guide code {
  color: #0f172a;
  background: #f1f5f9;
  border-radius: 4px;
  padding: 0.05rem 0.25rem;
}
.filter-grid {
  display: grid;
  grid-template-columns: minmax(0, 1.2fr) minmax(320px, 0.8fr);
  gap: 1rem;
  align-items: start;
}
.analysis-grid {
  display: grid;
  grid-template-columns: minmax(0, 1.35fr) minmax(360px, 0.65fr);
  gap: 1rem;
  align-items: start;
}
.guide-copy {
  color: #475569;
  font-size: 0.94rem;
  margin-bottom: 0.75rem;
}
.step-intro {
  color: #334155;
  font-size: 0.95rem;
  margin-bottom: 0.75rem;
}
.inline-guide {
  border: 1px solid #e2e8f0;
  border-radius: 6px;
  background: #f8fafc;
  margin-bottom: 0.8rem;
  padding: 0.6rem 0.75rem;
}
.inline-guide summary {
  cursor: pointer;
  font-weight: 700;
}
.inline-guide ul {
  color: #475569;
  font-size: 0.88rem;
  margin: 0.55rem 0 0 1rem;
  padding: 0;
}
.inline-guide li { margin-bottom: 0.35rem; }
.inline-guide code {
  color: #0f172a;
  background: #eef2f7;
  border-radius: 4px;
  padding: 0.05rem 0.25rem;
}
.querychat-shell { height: 640px; overflow: hidden; }
.coach-shell { height: 560px; overflow: hidden; }
.step-card { min-height: 760px; }
.plot-controls {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 0.9rem 1rem;
  margin-bottom: 1rem;
}
.plot-area {
  min-height: 430px;
  display: flex;
  align-items: center;
}
.plot-caption {
  border-top: 1px solid #e5e7eb;
  color: #334155;
  font-size: 0.98rem;
  margin-top: 0.75rem;
  padding-top: 0.75rem;
}
.filter-caption { color: #475569; font-size: 0.95rem; margin-top: 0.5rem; }
.warning-note { color: #9a3412; font-weight: 600; }
.metrics-grid {
  display: grid;
  grid-template-columns: repeat(6, minmax(0, 1fr));
  gap: 1rem;
  margin-bottom: 1rem;
}
.metrics-grid > * {
  min-width: 0;
  min-height: 132px;
}
.bslib-value-box .value-box-title { font-size: 0.8rem; }
.bslib-value-box .value-box-value { font-size: 1.35rem; }
.fold-panel {
  border: 1px solid #e5e7eb;
  border-radius: 8px;
  background: #ffffff;
  margin-top: 1rem;
  box-shadow: 0 1px 4px rgba(15, 23, 42, 0.08);
}
.fold-panel > summary {
  cursor: pointer;
  font-weight: 650;
  padding: 0.9rem 1rem;
  list-style-position: inside;
}
.fold-panel[open] > summary { border-bottom: 1px solid #e5e7eb; }
.fold-body { padding: 1rem; }
.table-scroll {
  max-height: 420px;
  overflow: auto;
}
.table-scroll table {
  white-space: nowrap;
  margin-bottom: 0;
}
@media (max-width: 1100px) {
  .filter-grid,
  .analysis-grid { grid-template-columns: 1fr; }
  .metrics-grid { grid-template-columns: repeat(3, minmax(0, 1fr)); }
  .field-guide-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
}
@media (max-width: 760px) {
  .querychat-shell,
  .coach-shell { height: 520px; }
  .step-card { min-height: auto; }
  .plot-controls { grid-template-columns: 1fr; }
  .metrics-grid { grid-template-columns: 1fr; }
  .field-guide-grid { grid-template-columns: 1fr; }
}
"

ui <- page_navbar(
  title = "BTS Airline Operations Explorer",
  theme = app_theme,
  fillable = FALSE,
  header = tags$head(tags$style(HTML(app_css))),

  nav_panel(
    "Explore",
    div(
      class = "explore-page",
      uiOutput("startup_warnings"),
      card(
        class = "main-query-card",
        fill = FALSE,
        card_header("1. Filter the data"),
        div(
          class = "filter-grid",
          div(
            class = "querychat-shell",
            qc$ui(fill = FALSE)
          ),
          div(
            class = "field-guide",
            h5("Useful fields and what they mean"),
            p(
              class = "guide-copy",
              "QueryChat filters the rows that feed the value boxes, plot, table, and interpretation coach. These are the most useful variables to mention in plain English."
            ),
            div(
              class = "field-guide-grid",
              div(
                class = "field-guide-item",
                strong("Airlines and airports"),
                p(tags$code("carrier"), " is the airline code. ", tags$code("origin"), " and ", tags$code("dest"), " are airport codes. ", tags$code("route"), " is an origin-destination pair such as ATL-DFW.")
              ),
              div(
                class = "field-guide-item",
                strong("Dates and schedules"),
                p(tags$code("fl_date"), " is the flight date. ", tags$code("month_start"), " is the first day of the flight month, and ", tags$code("day_of_week"), " / ", tags$code("crs_dep_hour"), " support day and time-of-day filters.")
              ),
              div(
                class = "field-guide-item",
                strong("Delay minutes"),
                p(tags$code("dep_delay"), " and ", tags$code("arr_delay"), " are minutes late or early. Positive means late; negative means early.")
              ),
              div(
                class = "field-guide-item",
                strong("Delay flags"),
                p(tags$code("dep_delayed_15"), " and ", tags$code("arr_delayed_15"), " mark flights delayed at least 15 minutes. Use these for broad delayed/not-delayed filters.")
              ),
              div(
                class = "field-guide-item",
                strong("Cancellations and diversions"),
                p(tags$code("cancelled_flag"), " identifies cancelled flights. ", tags$code("diverted_flag"), " identifies flights that landed somewhere other than the scheduled destination.")
              ),
                div(
                  class = "field-guide-item",
                  strong("Calendar context"),
                  p(tags$code("year"), ", ", tags$code("month_start"), ", ", tags$code("quarter"), ", and ", tags$code("season"), " support comparisons across the monthly sample.")
                ),
              div(
                class = "field-guide-item",
                strong("Delay causes"),
                p(tags$code("nas_delay"), ", ", tags$code("security_delay"), ", ", tags$code("weather_delay"), ", ", tags$code("carrier_delay"), ", and ", tags$code("late_aircraft_delay"), " are reported cause minutes, usually populated for delayed flights.")
              ),
              div(
                class = "field-guide-item",
                strong("Good starter filters"),
                p("Try airports, carriers, years, seasons, cancelled flights, or thresholds like departure delay greater than 30 minutes.")
              )
            )
          )
        )
      ),
      uiOutput("value_boxes"),
      div(
        class = "analysis-grid",
        card(
          class = "control-card",
          fill = FALSE,
          card_header("2. Build a plot"),
          div(
            class = "plot-controls",
            selectInput("plot_x", "X:", choices = plot_field_choices, selected = "month_start"),
            selectInput("plot_y", "Y:", choices = y_field_choices, selected = ".delay_rate"),
            selectInput("plot_color", "Color by:", choices = color_choices, selected = "year"),
            selectInput("plot_geom", "Geom:", choices = geom_choices, selected = "line"),
            selectInput("plot_summary", "Numeric summary:", choices = summary_choices, selected = "mean")
          ),
          div(
            class = "plot-area",
            plotOutput("diagnostic_plot", height = "430px", width = "100%")
          ),
          div(
            class = "plot-caption",
            textOutput("filter_caption")
          )
        ),
        card(
          class = "control-card",
          fill = FALSE,
          card_header("3. Ask about this plot"),
          p(
            class = "step-intro",
            "After the plot renders, ask the Haiku interpretation coach what visual pattern, comparison, caveat, or follow-up check stands out."
          ),
          div(
            class = "coach-shell",
            shinychat::chat_ui("interp", height = "560px", fill = FALSE)
          )
        )
      ),
      tags$details(
        class = "fold-panel",
        tags$summary("Show filtered data preview"),
        div(
          class = "fold-body table-scroll",
          tableOutput("preview_table")
        )
      )
    )
  ),

  nav_panel(
    "Evaluation",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Structured evaluation checklist"),
        tableOutput("evaluation_table")
      ),
      card(
        card_header("Manual QA reminders"),
        tags$ul(
          tags$li("Confirm QueryChat uses Claude Haiku by setting ANTHROPIC_API_KEY before launching."),
          tags$li("Try a zero-row filter and verify value boxes, plots, and chat stay friendly."),
          tags$li("Check that interpretation responses stay to four bullets and avoid causal language."),
          tags$li("Compare time periods, airports, carriers, and route subsets before interpreting differences."),
          tags$li("Document any failures in evaluation_plan.md before submission.")
        )
      )
    )
  ),

  nav_panel(
    "About",
    card(
      card_header("Project framing"),
      p("This app uses BTS Airline On-Time Performance data for local exploratory analysis of U.S. domestic flight operations."),
      p("The comparisons are descriptive. They should be read with caveats about seasonality, weather, airport mix, carrier mix, route composition, and sampling."),
      p("The app is designed to run locally from source code and should not be published online.")
    )
  )
)

server <- function(input, output, session) {
  # Activate QueryChat's Shiny module. qc_vals$df() is the natural-language
  # filtered data frame used by the plot builder, value boxes, preview table,
  # and plot interpretation coach.
  qc_vals <- qc$server()

  query_filtered <- reactive({
    qc_vals$df() %||% flights
  })

  selected_data <- reactive({
    query_filtered()
  })

  output$startup_warnings <- renderUI({
    if (length(startup_warnings) == 0) {
      return(NULL)
    }

    tags$div(
      class = "warning-note",
      paste(startup_warnings, collapse = " ")
    )
  })

  output$value_boxes <- renderUI({
    df <- selected_data()
    n <- nrow(df)
    delayed <- if (n == 0) NA_real_ else mean(df$dep_delayed_15 | df$arr_delayed_15, na.rm = TRUE)
    median_dep <- if (n == 0) NA_real_ else median(df$dep_delay, na.rm = TRUE)
    cancel_rate <- if (n == 0) NA_real_ else mean(df$cancelled_flag, na.rm = TRUE)
    total_cause <- if (n == 0) 0 else sum(df$total_cause_delay, na.rm = TRUE)
    nas_share <- if (total_cause > 0) sum(df$nas_delay, na.rm = TRUE) / total_cause else NA_real_
    security_share <- if (total_cause > 0) sum(df$security_delay, na.rm = TRUE) / total_cause else NA_real_

    div(
      class = "metrics-grid",
      value_box("Filtered flights", comma(n), theme = "primary"),
      value_box("Delayed 15+ min", safe_percent(delayed), theme = "secondary"),
      value_box("Median dep. delay", safe_number(median_dep, " min"), theme = "success"),
      value_box("Cancellation rate", safe_percent(cancel_rate, accuracy = 0.01), theme = "danger"),
      value_box("NAS delay share", safe_percent(nas_share), theme = "secondary"),
      value_box("Security delay share", safe_percent(security_share), theme = "secondary")
    )
  })

  current_plot <- reactive({
    req(input$plot_x, input$plot_y, input$plot_color, input$plot_geom, input$plot_summary)
    build_user_plot(
      selected_data(),
      x = input$plot_x,
      y = input$plot_y,
      color = input$plot_color,
      geom = input$plot_geom,
      summary_stat = input$plot_summary
    )
  })

  output$diagnostic_plot <- renderPlot({
    print(current_plot())
  }, res = 96)

  output$filter_caption <- renderText({
    df <- selected_data()
    query_title <- qc_vals$title() %||% "All QueryChat rows"
    paste0(
      "Currently showing ", comma(nrow(df)), " of ", comma(nrow(flights)),
      " processed rows. Filter: ", query_title, "."
    )
  })

  output$preview_table <- renderTable({
    selected_data() %>%
      select(
        fl_date, year_month, season, carrier, origin, dest, route,
        dep_delay, arr_delay, cancelled_flag, nas_delay, security_delay
      ) %>%
      mutate(
        fl_date = format(fl_date, "%Y-%m-%d"),
        dep_delay = round(dep_delay, 1),
        arr_delay = round(arr_delay, 1),
        nas_delay = round(nas_delay, 1),
        security_delay = round(security_delay, 1)
      ) %>%
      head(12)
  }, striped = TRUE, spacing = "s")

  output$evaluation_table <- renderTable({
    evaluation_cases
  }, striped = TRUE, spacing = "s")

  interp_chat <- ellmer::chat_anthropic(
    model = HAIKU_MODEL,
    system_prompt = paste(
      "You are a cautious aviation statistics coach.",
      "Interpret plots as exploratory descriptive evidence, never causal evidence.",
      "Use at most 4 bullets: observed pattern, comparison, caveat, follow-up check.",
      "Caveats may include seasonality, weather, airport mix, carrier mix, route composition, and sampling."
    )
  )

  observeEvent(input$interp_user_input, {
    df <- selected_data()

    if (nrow(df) == 0) {
      shinychat::chat_append(
        "interp",
        "No flight rows match the current filters. Broaden the QueryChat filter or choose a different airport, carrier, date, or delay threshold, then ask again."
      )
      return()
    }

    # content_image_plot() records the current graphics device, so print the
    # reactive plot immediately before sending it to Haiku.
    print(current_plot())

    date_range <- if (nrow(df) == 0 || all(is.na(df$fl_date))) {
      "no matching dates"
    } else {
      paste(format(range(df$fl_date, na.rm = TRUE)), collapse = " to ")
    }

    plot_context <- paste(
      "Question:", input$interp_user_input,
      "\nPlot X:", label_for_value(input$plot_x, plot_field_choices),
      "\nPlot Y:", label_for_value(input$plot_y, y_field_choices),
      "\nColor by:", label_for_value(input$plot_color, color_choices),
      "\nGeom:", label_for_value(input$plot_geom, geom_choices),
      "\nRows:", nrow(df),
      "\nDate range:", date_range,
      "\nQueryChat filter:", qc_vals$title() %||% "All rows",
      "\nReturn <=4 bullets with one observed pattern, one comparison, one caveat, and one follow-up check."
    )

    shinychat::chat_append(
      "interp",
      interp_chat$stream_async(
        ellmer::content_image_plot(width = 768, height = 520),
        plot_context
      )
    )
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
