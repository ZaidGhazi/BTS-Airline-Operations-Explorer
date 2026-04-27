# Vitals Evaluation Results

This folder contains the outputs from running:

```r
source("R/evaluate_vitals.R")
run_vitals_evaluation()
```

The evaluation cases now target the general BTS Airline Operations Explorer workflow: monthly trends, seasonal NAS delay minutes, carrier comparisons, zero-row handling, and route/time-of-day heatmaps. Re-run the script after regenerating the 2023-2025 processed dataset so the saved result CSVs reflect the current app narrative.

Files:

- `vitals_eval_dataset.csv`: generated vitals dataset with prompts, plot paths, rubrics, and row counts.
- `vitals_samples.csv`: model responses and parsed vitals scores.
- `vitals_scores.csv`: compact score output from `vitals::vitals_bind()`.
- `vitals_score_counts.csv`: score count summary.

The associated plot images are in `evaluation/vitals/plots/`, and Inspect-compatible vitals logs are in `evaluation/vitals/logs/`.
