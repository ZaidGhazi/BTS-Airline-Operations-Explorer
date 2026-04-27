# BTS Airline Operations Data Dictionary

This dataset is derived from Bureau of Transportation Statistics (BTS) Airline On-Time Performance data. The unit of observation is one scheduled domestic flight.

The BTS Airline Operations Explorer uses a compact sample of flights from 2023 through 2025 involving selected major U.S. airports. The app supports exploratory descriptive comparisons across dates, airports, carriers, routes, seasons, time of day, cancellations, diversions, and reported delay causes.

The app does not support causal inference. Differences across carriers, airports, routes, or calendar periods may reflect seasonality, weather, airport mix, carrier mix, route composition, demand patterns, sampling, or other operational factors.

Positive delay values mean the flight was late. Negative delay values mean the flight was early. Unless the user specifies a different threshold, a flight is treated as delayed when `dep_delay >= 15`.

Delay-cause minutes are usually populated only for delayed flights, so cause-share plots should be interpreted carefully. A high share for one cause can reflect the filter definition, reporting conventions, or the subset of flights that have cause fields populated.

## Sample Scope

- Source: BTS Airline On-Time Performance / Reporting Carrier On-Time Performance data.
- Date range: every month from January 2023 through December 2025 by default.
- Sampling: the prep script samples up to `BTS_MAX_ROWS_PER_MONTH` rows per month after filtering to selected major airports so the app remains responsive and GitHub-friendly.
- Major airport filter: ATL, DFW, DEN, ORD, LAX, JFK, SFO, SEA, LAS, MCO, CLT, PHX, MIA, EWR, BOS, DCA, IAD, and BWI.

## Columns

- `fl_date`: flight date.
- `year`: flight year.
- `month`: flight month number.
- `month_start`: first day of the flight month, useful for trend plots.
- `year_month`: year-month label in `YYYY-MM` format.
- `quarter`: calendar quarter label.
- `month_name`: abbreviated month label.
- `season`: Winter, Spring, Summer, or Fall based on month.
- `day_of_month`: day number within the month.
- `day_of_week`: BTS day-of-week code.
- `weekend_flag`: `TRUE` for Saturday or Sunday according to the BTS day-of-week code.
- `carrier`: operating carrier code used by the app; this is copied from `op_unique_carrier`.
- `op_unique_carrier`: BTS operating or reporting carrier code, depending on the source file schema.
- `origin`: origin airport code.
- `dest`: destination airport code.
- `route`: route label in `ORIGIN-DEST` format.
- `crs_dep_time`: scheduled departure time in local airport time, stored as HHMM.
- `crs_dep_hour`: scheduled departure hour derived from `crs_dep_time`.
- `time_of_day`: scheduled departure bucket: Overnight, Morning, Afternoon, or Evening.
- `dep_delay`: departure delay in minutes. Positive values mean late; negative values mean early.
- `arr_delay`: arrival delay in minutes. Positive values mean late; negative values mean early.
- `dep_delayed_15`: `TRUE` when `dep_delay >= 15`; otherwise `FALSE`.
- `arr_delayed_15`: `TRUE` when `arr_delay >= 15`; otherwise `FALSE`.
- `cancelled`: BTS cancellation indicator, where 1 means the flight was cancelled.
- `cancelled_flag`: logical cancellation indicator derived from `cancelled`.
- `diverted`: BTS diversion indicator, where 1 means the flight was diverted.
- `diverted_flag`: logical diversion indicator derived from `diverted`.
- `distance`: route distance in miles.
- `distance_band`: binned route distance.
- `air_time`: airborne time in minutes.
- `carrier_delay`: carrier-attributed delay minutes.
- `weather_delay`: weather-attributed delay minutes.
- `nas_delay`: National Airspace System delay minutes.
- `security_delay`: security-attributed delay minutes.
- `late_aircraft_delay`: late-arriving-aircraft delay minutes.
- `total_cause_delay`: sum of `carrier_delay`, `weather_delay`, `nas_delay`, `security_delay`, and `late_aircraft_delay`.

## Query Examples

- Show flights from DCA, IAD, or BWI.
- Keep only flights with NAS delay greater than 30 minutes.
- Show American, Delta, United, and Southwest only.
- Flights from ATL to DFW or DFW to ATL.
- Keep flights departing before 8 AM.
- Show flights in summer 2025.
