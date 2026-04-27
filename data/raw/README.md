# Raw BTS Files

The app runs from `data/processed/bts_airline_flights_2023_2025_sample.csv` and does not need raw files at startup.

Raw BTS Airline On-Time Performance monthly zip archives come from:

```text
https://transtats.bts.gov/PREZIP/
```

Run `source("R/prep_bts_data.R"); prep_bts_data()` from the repository root to regenerate the processed CSV. By default the script downloads monthly archives from January 2023 through December 2025 as temporary files, filters to selected major airports, samples rows by month, and writes the compact processed dataset.

Set `BTS_CACHE_RAW = "true"` before running the prep script if you want to cache downloaded raw zip archives in this folder for local audit work. Full multi-year raw archives are large, so they are not required for the app to run and do not need to be committed for grading.
