# Vitals evaluation for the BTS Airline Operations Explorer.
#
# This script evaluates the Haiku-powered plot interpretation coach used in the
# app. It generates representative plot images from the processed BTS dataset,
# sends each image plus a realistic user question to Claude Haiku, and grades the
# response with a Haiku model-graded scorer.

run_vitals_evaluation <- function() {
required_packages <- c(
  "vitals", "ellmer", "ggplot2", "dplyr", "readr", "tidyr",
  "scales", "tibble", "purrr", "base64enc"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install packages before running the vitals evaluation:\n",
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
  stop("ANTHROPIC_API_KEY is required to run the Haiku vitals evaluation.", call. = FALSE)
}

library(vitals)
library(ellmer)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(tibble)
library(purrr)

data_path <- file.path("data", "processed", "bts_airline_flights_2023_2025_sample.csv")
cases_path <- file.path("evaluation", "vitals", "vitals_cases.csv")
plots_dir <- file.path("evaluation", "vitals", "plots")
results_dir <- file.path("evaluation", "vitals", "results")
logs_dir <- file.path("evaluation", "vitals", "logs")

dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(VITALS_LOG_DIR = normalizePath(logs_dir, winslash = "/", mustWork = FALSE))

flights <- read_csv(data_path, show_col_types = FALSE, progress = FALSE) %>%
  mutate(
    fl_date = as.Date(fl_date),
    month_start = as.Date(month_start),
    delayed_15 = dep_delayed_15 | arr_delayed_15,
    year = as.integer(year),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
    time_of_day = factor(time_of_day, levels = c("Overnight", "Morning", "Afternoon", "Evening"))
  )

cases <- read_csv(cases_path, show_col_types = FALSE)

interp_system_prompt <- paste(
  "You are a cautious aviation statistics coach.",
  "Interpret plots as exploratory descriptive evidence, never causal evidence.",
  "Use at most 4 bullets: observed pattern, comparison, caveat, follow-up check.",
  "Caveats may include seasonality, weather, airport mix, carrier mix, route composition, and sampling."
)

empty_plot <- function(message) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 5, color = "#475569") +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void(base_size = 13)
}

apply_case_filter <- function(df, id) {
  if (id == "washington_monthly_trend") {
    df %>% filter(origin %in% c("DCA", "IAD", "BWI"))
  } else if (id == "nas_season_breakdown") {
    df %>% filter(nas_delay > 30)
  } else if (id == "carrier_comparison") {
    df %>% filter(carrier %in% c("AA", "DL", "UA", "WN"))
  } else if (id == "zero_row_empty_state") {
    df %>% filter(origin == "ZZZ")
  } else if (id == "route_heatmap") {
    df %>% filter(route %in% c("ATL-DFW", "DFW-ATL"))
  } else {
    df
  }
}

build_eval_plot <- function(df, plot_type) {
  base_theme <- theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      axis.text.x = element_text(angle = 20, hjust = 1)
    )

  if (nrow(df) == 0) {
    return(empty_plot("No matching flights for this filter."))
  }

  if (plot_type == "Monthly delay trend") {
    monthly_df <- df %>%
      group_by(month_start, year) %>%
      summarise(flights = n(), delay_rate = mean(delayed_15, na.rm = TRUE), .groups = "drop")

    ggplot(monthly_df, aes(month_start, delay_rate, color = factor(year), group = year)) +
      geom_line(linewidth = 0.8) +
      geom_point(aes(size = flights), alpha = 0.65) +
      scale_y_continuous(labels = percent_format()) +
      scale_size_continuous(range = c(1.5, 5), guide = "none") +
      labs(title = plot_type, x = NULL, y = "Monthly percent delayed", color = "Year") +
      base_theme

  } else if (plot_type == "Seasonal NAS delay breakdown") {
    season_df <- df %>%
      group_by(season) %>%
      summarise(minutes = sum(nas_delay, na.rm = TRUE), flights = n(), .groups = "drop") %>%
      filter(minutes > 0)

    if (nrow(season_df) == 0) {
      return(empty_plot("No NAS delay minutes are available for this filter."))
    }

    ggplot(season_df, aes(season, minutes, fill = season)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      scale_y_continuous(labels = comma) +
      labs(title = plot_type, x = NULL, y = "NAS delay minutes") +
      base_theme

  } else if (plot_type == "Carrier comparison") {
    carrier_df <- df %>%
      group_by(carrier, year) %>%
      summarise(
        flights = n(),
        delay_rate = mean(delayed_15, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(flights >= 20)

    ggplot(carrier_df, aes(carrier, delay_rate, fill = factor(year))) +
      geom_col(width = 0.7, position = "dodge") +
      scale_y_continuous(labels = percent_format()) +
      labs(title = plot_type, x = "Carrier", y = "Percent delayed", fill = "Year") +
      base_theme

  } else if (plot_type == "Route time-of-day heatmap") {
    route_df <- df %>%
      group_by(route, time_of_day) %>%
      summarise(flights = n(), delay_rate = mean(delayed_15, na.rm = TRUE), .groups = "drop")

    ggplot(route_df, aes(time_of_day, route, fill = delay_rate)) +
      geom_tile(color = "white", linewidth = 0.4) +
      scale_fill_gradient(low = "#e0f2fe", high = "#b91c1c", labels = percent_format()) +
      labs(title = plot_type, x = "Time of day", y = "Route", fill = "Delayed") +
      base_theme

  } else {
    monthly_df <- df %>%
      group_by(month_start) %>%
      summarise(delay_rate = mean(delayed_15, na.rm = TRUE), .groups = "drop")

    ggplot(monthly_df, aes(month_start, delay_rate)) +
      geom_line(color = "#1f6f8b", linewidth = 0.8) +
      scale_y_continuous(labels = percent_format()) +
      labs(title = plot_type, x = NULL, y = "Percent delayed") +
      base_theme
  }
}

case_records <- pmap_dfr(
  cases,
  function(id, filter_query, plot_type, user_question, target) {
    filtered <- apply_case_filter(flights, id)
    plot_path <- file.path(plots_dir, paste0(id, ".png"))
    ggsave(
      filename = plot_path,
      plot = build_eval_plot(filtered, plot_type),
      width = 9,
      height = 5.5,
      dpi = 120
    )

    date_range <- if (nrow(filtered) == 0) {
      "no matching dates"
    } else {
      paste(format(range(filtered$fl_date, na.rm = TRUE)), collapse = " to ")
    }

    context <- paste(
      "Interpret the attached BTS Airline Operations Explorer plot.",
      paste("QueryChat filter:", filter_query),
      paste("Plot type:", plot_type),
      paste("Filtered row count:", nrow(filtered)),
      paste("Filtered date range:", date_range),
      paste("User question:", user_question),
      "Return at most 4 bullets: observed pattern, comparison, caveat, follow-up check.",
      "Avoid causal claims.",
      sep = "\n"
    )

    tibble(
      id = id,
      input = paste0(
        "<image_path>",
        normalizePath(plot_path, winslash = "/", mustWork = TRUE),
        "</image_path>\n",
        context
      ),
      target = target,
      filter_query = filter_query,
      plot_type = plot_type,
      user_question = user_question,
      row_count = nrow(filtered),
      plot_path = normalizePath(plot_path, winslash = "/", mustWork = TRUE)
    )
  }
)

write_csv(case_records, file.path(results_dir, "vitals_eval_dataset.csv"))

interpretation_solver <- function(inputs, ..., solver_chat) {
  results <- character(length(inputs))
  chats <- vector("list", length(inputs))

  for (i in seq_along(inputs)) {
    image_path <- sub("(?s).*<image_path>(.*?)</image_path>.*", "\\1", inputs[[i]], perl = TRUE)
    prompt <- sub("(?s)^.*</image_path>\\s*", "", inputs[[i]], perl = TRUE)

    ch <- solver_chat$clone()
    ch$chat(
      prompt,
      ellmer::content_image_file(image_path, resize = "none")
    )

    results[[i]] <- ch$last_turn()@text
    chats[[i]] <- ch
  }

  list(result = results, solver_chat = chats)
}

judge_instructions <- paste(
  "Grade whether the answer satisfies the target rubric for an aviation plot interpretation coach.",
  "Correct answers must be concise, use at most four bullets, include an observed pattern, comparison, caveat, and follow-up check when applicable, and avoid causal claims.",
  "Mark Incorrect if the answer claims causation, invents evidence that is not in the prompt/plot, or ignores an empty-state plot.",
  "Allow Partial credit for answers that are cautious but miss one required element.",
  "End with one final line exactly: GRADE: C, GRADE: P, or GRADE: I."
)

task <- vitals::Task$new(
  dataset = case_records,
  solver = interpretation_solver,
  scorer = vitals::model_graded_qa(
    partial_credit = TRUE,
    instructions = judge_instructions,
    scorer_chat = ellmer::chat_anthropic(model = "claude-haiku-4-5")
  ),
  name = "BTS Airline Operations Explorer plot coach"
)

task$eval(
  solver_chat = ellmer::chat_anthropic(
    model = "claude-haiku-4-5",
    system_prompt = interp_system_prompt
  )
)

samples <- task$get_samples()
flat_samples <- samples[, !vapply(samples, is.list, logical(1)), drop = FALSE]
write_csv(flat_samples, file.path(results_dir, "vitals_samples.csv"))

score_summary <- vitals::vitals_bind(task)
write_csv(score_summary, file.path(results_dir, "vitals_scores.csv"))

score_counts <- flat_samples %>%
  count(score, name = "n")
write_csv(score_counts, file.path(results_dir, "vitals_score_counts.csv"))

message("Vitals evaluation complete.")
message("Dataset: ", file.path(results_dir, "vitals_eval_dataset.csv"))
message("Samples: ", file.path(results_dir, "vitals_samples.csv"))
message("Scores: ", file.path(results_dir, "vitals_scores.csv"))
message("Score counts: ", file.path(results_dir, "vitals_score_counts.csv"))
message("Logs: ", Sys.getenv("VITALS_LOG_DIR"))
}
