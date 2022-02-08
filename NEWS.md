# rcaaqs 0.3.1.9000

* Finalized methods for SO2 and NO2
* Output CAAQS status and managment status for each metric
* Function wrappers for each CAAQS metric that return an object of 
  class `caaqs` plus an additional class based on the metric (one of `"pm2.5_annual"
  "pm2.5_24h", "o3", "so2_1yr", "so2_3yr", "no2_1yr", "no2_3yr"`). This object 
  contains the final caaqs results plus the intermediate data sets.
* "Extractor" functions to get the the caaqs results or intermediate data from 
  the `caaqs` object:
    - `get_caaqs()`:
      - caaqs results
    - `get_hourly()`:
      - so2_1yr, no2_1yr: Hourly values used for annual average
    - `get_daily()`:
      - pm2.5_24h, pm2.5_annual: Daily average
      - o3, so2_3yr, no2_3yr: Daily maximum
    - `get_yearly()`:
      - pm2.5_24h: Annual 98th percentile daily average
      - pm2.5_annual: Annual average of daily averages
      - o3: Annual 4th highest daily maximum
      - so2_1yr, no2_1yr: Annual average of hourly values
      - so2_3yr: Annual 99th percentile of daily maximums
      - no2_3yr: Annual 98th percentile of daily maximums
    - `get_three_yr_rolling()`:
      - pm2.5_24h, pm2.5_annual, o3, so2_3yr, no2_3yr: Three-year rolling 
      average of the yearly value.
* New function `caaqs_management()` to add management categories to a `caaqs` 
  object, and optionally exclude dates from the analysis which have been deemed
  influenced by exceptional events (such as wildfire) or transboundary flows 
  (see the CAAQS manuals for how these are determined).
* `plot_ts()` now expects a `caaaqs` object as input (the result of running one 
  of the 
  `*_caaqs()` functions).
* New function, `assign_airzone` to determine where sites are located in airzone 
  shapefiles
* Removed airzone map, use maps from `bcmaps` package instead
* Updates to documentation
* Improved coding consistency with respect to argument names
* Use lubridate functions rather than internal date/time functions
* Update CAAQS for PM2.5 and Ozone
* Update to dplyr v1.0 and tidyr v1.1

# rcaaqs 0.3.1

* Fixed plotting functions to more reliably plot special characters (e.g., mu and superscript)

# rcaaqs 0.3.0

* Made `format_date` function defunct, and replaced it with `format_caaqs_dt`, which puts the 
datetime into the correct timezone in POSIXct, and automatically assigns readings with the 
previous hour by subtracting one second from the timestamp.
* export `round_caaqs` function.
* Add more robust checking of columns in core functions (#23)

# rcaaqs 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Methods for PM2.5 now deal with exceptions to data completeness criteria (#13, #14)
* Methods for ozone (#2, #14)
* Preliminary methods for SO2 (#11, #14) and NO2 (#12, #14)
* Methods for dealing with exceptional events and transboundary flows (#5, #14)
* Added Aman Verma (@nograpes) as a package author for work on #2, #5, #11, #12, #13, #14 (via #17)
* Ensure rounding follows the CCME 'Guidance Document on Achievement Determination Canadian Ambient Air Quality Standards for Fine Particulate Matter and Ozone' standards (#21)
