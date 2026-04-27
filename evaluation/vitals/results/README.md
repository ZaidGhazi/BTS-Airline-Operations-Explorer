# Vitals Evaluation Results

This folder is the default output location for:

```r
source("R/evaluate_vitals.R")
run_vitals_evaluation()
```

The evaluation cases target the BTS Airline Operations Explorer workflow: monthly trends, seasonal NAS delay minutes, carrier comparisons, zero-row handling, and route/time-of-day heatmaps.

The latest local vitals run completed all five cases successfully:

- Correct: 5
- Partial: 0
- Incorrect: 0

Generated result files are ignored by git because they include local paths, logs, plots, and model outputs. Re-run the evaluation command above to recreate them on another machine.

Generated files:

- `vitals_eval_dataset.csv`: generated vitals dataset with prompts, plot paths, scoring criteria, and row counts.
- `vitals_samples.csv`: model responses and parsed vitals scores.
- `vitals_scores.csv`: compact score output from `vitals::vitals_bind()`.
- `vitals_score_counts.csv`: score count summary.

The associated plot images are in `evaluation/vitals/plots/`, and Inspect-compatible vitals logs are in `evaluation/vitals/logs/`.
