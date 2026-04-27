# Evaluation Plan

Use these manual checks after installing packages, setting `ANTHROPIC_API_KEY`, and launching the app locally. Record notes on whether the QueryChat filter, plot, value boxes, and interpretation coach behave as expected.

| Test case | Filter query | Suggested plot controls | Question to ask the interpretation coach | Expected behavior | Failure modes to watch for |
| --- | --- | --- | --- | --- | --- |
| Washington-area origin filter | Show flights from DCA, IAD, or BWI | X = Month; Y = Delay rate; Color = Year; Geom = Line | What seasonal delay pattern is visible? | QueryChat returns Washington-area rows; value boxes update; the plot shows a monthly delay-rate pattern; Haiku gives no more than four cautious bullets. | QueryChat filters the wrong airport field; plot crashes on sparse months; response implies a cause without evidence. |
| NAS-focused delay filter | Keep flights with NAS delay greater than 30 minutes | X = Season; Y = NAS delay; Color = Season; Geom = Bars; Summary = Sum | Which season has the most NAS delay minutes in this filtered view? | NAS delay value box should be substantial; the coach describes the visible pattern and notes that cause minutes are populated mainly for delayed flights. | Missing cause columns produce errors; all shares are reported as `NA`; LLM overstates mechanisms. |
| Carrier comparison | Only American, Delta, United, and Southwest | X = Carrier; Y = Delay rate; Color = Year; Geom = Bars | Which carrier differences look largest? | Carrier plot shows a manageable set of carriers with delay rates split by year; low-count categories are limited gracefully. | Carrier codes are misunderstood; too-few rows cause empty plot without explanation; color legend is unreadable. |
| Zero-row handling | Show flights from ZZZ | X = Month; Y = Delay rate; Color = None; Geom = Line | What should I do if no rows match? | Value boxes show zero or `NA`; plot displays a friendly empty-state message; chat says to broaden filters instead of crashing. | App errors in `median()` or `mean()`; `content_image_plot()` tries to interpret an empty chart; preview table fails. |
| Route heatmap sanity check | Flights from ATL to DFW or DFW to ATL | X = Time of day; Y = Route; Color = None; Geom = Heatmap | Does either direction look different by departure time? | Heatmap shows route-time cells when enough rows match; interpretation uses cautious comparison language. | QueryChat returns only one direction unexpectedly; heatmap breaks with one route; response ignores route composition caveat. |

## Vitals Evaluation Setup

The repository includes a vitals-based evaluation for the plot interpretation coach:

```r
source("R/evaluate_vitals.R")
run_vitals_evaluation()
```

The script uses the five cases above as `evaluation/vitals/vitals_cases.csv`, generates a representative plot image for each case, asks the Claude Haiku interpretation coach to respond, and grades the response with `vitals::model_graded_qa()` using Claude Haiku as the scorer. The app itself lets the user recreate similar views with X, Y, color, geom, and summary controls. The evaluation criteria check that responses:

- stay within four bullets,
- avoid causal claims,
- include a visual pattern and comparison when rows exist,
- include a caveat such as seasonality, weather, airport mix, carrier mix, route composition, or sampling,
- suggest a follow-up check,
- handle the zero-row empty-state case without inventing a pattern.

Generated outputs are written to `evaluation/vitals/results/`, plot images to `evaluation/vitals/plots/`, and vitals logs to `evaluation/vitals/logs/`.
