# BTS Airline Operations Explorer

This repository contains a local Shiny app for STAT 6395 Homework #6. The app explores Bureau of Transportation Statistics Airline On-Time Performance records for U.S. domestic flights involving selected major airports.

The app is an exploratory descriptive analysis tool. It does not estimate or claim causal effects. Interpretation prompts and app text frame observed differences with caveats about seasonality, weather, airport mix, carrier mix, route composition, and sampling.

## Repository Contents

- `app.R`: local Shiny app.
- `data/processed/bts_airline_flights_2023_2025_sample.csv`: app-ready processed dataset with 36,000 rows.
- `data/raw/README.md`: notes on raw BTS files and reproducibility.
- `greeting.md`: custom QueryChat greeting with clickable suggestions.
- `data/bts_data_dictionary.md`: data dictionary and QueryChat context.
- `querychat_instructions.md`: extra QueryChat instructions for cautious, dataset-specific filtering.
- `R/prep_bts_data.R`: reproducible data preparation script.
- `R/evaluate_vitals.R`: vitals evaluation script for the plot interpretation coach.
- `evaluation_plan.md`: structured manual evaluation plan.
- `evaluation/vitals/vitals_cases.csv`: vitals evaluation cases and grading rubrics.

The Shiny app reads the compact processed CSV for performance. Full raw BTS monthly archives are large, so the repository should host the processed monthly sample and documentation for grading, while `R/prep_bts_data.R` documents how to regenerate the sample from BTS PREZIP source files.

For grading, the GitHub repository should include the source files, processed CSV, and markdown documentation. The app is not deployed or published online.

## Assignment Features

- Runs locally from source code.
- Uses BTS Airline On-Time Performance data with more than 5,000 rows.
- Provides QueryChat natural-language filtering.
- Uses `ellmer::chat_anthropic(model = "claude-haiku-4-5")` for both QueryChat and plot interpretation.
- Uses `shinychat::chat_ui()` and `shinychat::chat_append()`.
- Sends the current plot to the interpretation coach with `ellmer::content_image_plot()`.
- Includes a custom QueryChat greeting and domain-specific data context.
- Uses a concise plot interpretation system prompt requiring at most four bullets.
- Adds value boxes for filtered flights, delay rate, median departure delay, cancellation rate, NAS delay share, and security delay share.
- Lets the user build plots with selectable X, Y, color, geom, and numeric summary controls.
- Includes structured evaluation in `evaluation_plan.md`, an Evaluation tab in the app, and a vitals evaluation script.
- Uses an aviation-specific workflow layout: QueryChat and field guide first, a value-box metric strip, then a plot builder paired with the plot interpretation coach. This keeps the example app's clear filter/plot/ask progression without copying the baseline layout.

## Data Source and Reproducibility

Raw data come from the BTS PREZIP archive:

```text
https://transtats.bts.gov/PREZIP/
```

The default processing script targets every BTS Reporting Carrier On-Time Performance monthly file from January 2023 through December 2025. It filters to flights involving selected major airports and samples up to 1,000 rows per month by default, producing a compact app dataset suitable for GitHub and local Shiny use.

To reproduce `data/processed/bts_airline_flights_2023_2025_sample.csv`:

```r
source("R/prep_bts_data.R")
prep_bts_data()
```

To keep all filtered rows instead of sampling:

```r
Sys.setenv(BTS_MAX_ROWS_PER_MONTH = "Inf")
source("R/prep_bts_data.R")
prep_bts_data()
```

To cache downloaded raw BTS zip archives in `data/raw/` while regenerating:

```r
Sys.setenv(BTS_CACHE_RAW = "true")
source("R/prep_bts_data.R")
prep_bts_data()
```

Full raw archives can be hundreds of megabytes to more than a gigabyte for multi-year ranges, so they are not required for the app to run.

## Install Packages

```r
install.packages(c(
  "shiny", "bslib", "querychat", "ellmer", "shinychat",
  "ggplot2", "dplyr", "readr", "tidyr", "scales", "duckdb",
  "vitals", "tibble", "purrr", "base64enc"
))
```

If QueryChat needs an alternate local database backend:

```r
install.packages("RSQLite")
```

## API Key

The LLM features require an Anthropic API key:

```r
Sys.setenv(ANTHROPIC_API_KEY = "your-key-here")
```

The key can also be stored in a local `.Renviron` file:

```text
ANTHROPIC_API_KEY=your-key-here
```

## Custom Greeting and Context

QueryChat loads the custom greeting from `greeting.md` with `greeting =`. The file includes app-specific instructions and clickable suggestions using QueryChat's `<span class="suggestion">...</span>` syntax.

The specific data context comes from `data/bts_data_dictionary.md` with `data_description =`. Additional behavior instructions come from `querychat_instructions.md` with `extra_instructions =`.

These markdown files are committed to GitHub so the app can run from source with the same greeting and context. `.Renviron` is not committed because it may contain private API keys.

The app also shows a short field guide beside QueryChat so a grader can quickly see useful variables for natural-language filtering, including carrier and airport fields, date fields, delay minutes, delay flags, cancellation/diversion flags, calendar labels, and delay-cause minutes.

## Run the Vitals Evaluation

The vitals evaluation checks the Haiku-powered plot interpretation coach against five representative cases:

```r
source("R/evaluate_vitals.R")
run_vitals_evaluation()
```

The evaluation writes generated plot images, vitals logs, model outputs, and score summaries under `evaluation/vitals/`. Both the solver and scorer use Claude Haiku to satisfy the model constraint for this assignment. The prep and evaluation files are written as functions so Shiny can safely auto-source files in `R/` without rerunning data preparation or vitals evaluation during app startup.

## Run the App

From the repository root:

```r
shiny::runApp()
```

The app should be run locally from source.
