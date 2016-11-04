<!-- README.md is generated from README.Rmd. Please edit that file -->
<a rel="Exploration" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="http://bcdevexchange.org/badge/2.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a>

[![Travis-CI Build Status](https://travis-ci.org/bcgov/rcaaqs.svg?branch=master)](https://travis-ci.org/bcgov/rcaaqs)

------------------------------------------------------------------------

rcaaqs
======

An [R](https://www.r-project.org/) package to facilitate the calculation of air quality metrics according to the Canadian Ambient Air Quality Standards ([CAAQS](http://www.ccme.ca/en/current_priorities/air/caaqs.html))

### Features

-   General functions for doing things like formatting dates, filling in sequences, etc.
-   Functions for stepwise calculation of CAAQS metrics for different pollutants. Currently these are only complete for PM<sub>2.5</sub> (annual and 24hr) metrics, and are in development for ozone.
-   Functions for assigning metrics for different pollutants into achievement and management categories.

### Installation

The package is not available on CRAN, but can be installed using the [devtools](https://github.com/hadley/devtools) package:

``` r
install.packages("devtools") # if not already installed

library(devtools)
install_github("bcgov/rcaaqs")
```

### Usage

This is a simple example using the included sample data set for PM<sub>2.5</sub>. For a fuller example with a larger dataset requiring more cleaning, see the [PM<sub>2.5</sub> analysis we completed for 2011-2013](https://github.com/bcgov/pm25-caaqs-analysis).

``` r
library(rcaaqs)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

## Load sample data:
data("pm25_sample_data")
glimpse(pm25_sample_data)
#> Observations: 229,496
#> Variables: 6
#> $ ems_id    <chr> "0220205", "0220205", "0220205", "0220205", "0220205...
#> $ date_time <dttm> 2011-03-01 00:59:59, 2011-03-01 01:59:59, 2011-03-0...
#> $ site      <chr> "Powell River Wildwood_60", "Powell River Wildwood_6...
#> $ year      <int> 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011...
#> $ parameter <chr> "PM25", "PM25", "PM25", "PM25", "PM25", "PM25", "PM2...
#> $ value     <dbl> 2, 2, 0, 3, 1, 1, 2, 2, 2, 2, 1, 3, 1, 1, 0, 0, 0, 0...

## Calculate the data completeness criteria for the pm2.5 data:
pm_completeness <- pm_data_complete(pm25_sample_data, 
                                    by = c("ems_id", "site"))

## Columns are added showing the completeness evaluation annually 
## and quarterly
glimpse(pm_completeness)
#> Observations: 30
#> Variables: 12
#> $ ems_id               <chr> "0220205", "0220205", "0220205", "0310162...
#> $ site                 <chr> "Powell River Wildwood_60", "Powell River...
#> $ year                 <int> 2011, 2012, 2013, 2011, 2012, 2013, 2011,...
#> $ n_days               <int> 281, 344, 253, 362, 365, 352, 351, 344, 3...
#> $ percent_valid_annual <dbl> 76.986301, 93.989071, 69.315068, 99.17808...
#> $ percent_valid_q1     <dbl> 33.33333, 98.90110, 57.77778, 96.66667, 9...
#> $ percent_valid_q2     <dbl> 86.81319, 100.00000, 18.68132, 100.00000,...
#> $ percent_valid_q3     <dbl> 86.95652, 98.91304, 100.00000, 100.00000,...
#> $ percent_valid_q4     <dbl> 100.000000, 78.260870, 100.000000, 100.00...
#> $ annual_valid         <lgl> TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE...
#> $ quarters_valid       <lgl> FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRU...
#> $ use_annual           <lgl> FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRU...

## Compute daily averages by station (these are used for both the 24 hr 
## metric and the Annual metric:
avgdaily <- pm_daily_avg(pm25_sample_data, by = c("ems_id", "site"))
glimpse(avgdaily)
#> Observations: 9,714
#> Variables: 6
#> $ ems_id     <chr> "0220205", "0220205", "0220205", "0220205", "022020...
#> $ site       <chr> "Powell River Wildwood_60", "Powell River Wildwood_...
#> $ date       <date> 2011-03-01, 2011-03-02, 2011-03-03, 2011-03-04, 20...
#> $ n_readings <int> 24, 22, 24, 24, 24, 24, 24, 24, 23, 24, 24, 24, 24,...
#> $ avg_24h    <dbl> 1.0, 2.3, 2.0, 1.0, 1.6, 1.0, 1.6, 2.2, 1.3, 2.6, 0...
#> $ year       <int> 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 201...

# Calculate the PM25 24 Hour Metric -----------------------------------

## Compute annual 98th percentile of 24h averages for each site
ann_98_per <- pm_98_percentile(avgdaily, by = "ems_id")
glimpse(ann_98_per)
#> Observations: 30
#> Variables: 5
#> $ ems_id            <chr> "0220205", "0220205", "0220205", "0310162", ...
#> $ year              <int> 2011, 2012, 2013, 2011, 2012, 2013, 2011, 20...
#> $ n_days            <int> 281, 342, 252, 362, 364, 352, 352, 342, 345,...
#> $ ann_98_percentile <dbl> 7.4, 8.8, 8.8, 10.3, 15.0, 13.6, 12.6, 15.9,...
#> $ exceed            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA...

## Then join the results to the data completeness data frame and select 
## only the results that are valid. This process could be more efficent
ann_98_per <- left_join(ann_98_per, pm_completeness, 
                        by = c("ems_id", "year"))
ann_98_per <- filter(ann_98_per, use_annual | (exceed & annual_valid))
glimpse(ann_98_per)
#> Observations: 23
#> Variables: 15
#> $ ems_id               <chr> "0220205", "0310162", "0310162", "0310162...
#> $ year                 <int> 2012, 2011, 2012, 2013, 2011, 2012, 2013,...
#> $ n_days.x             <int> 342, 362, 364, 352, 352, 342, 345, 357, 3...
#> $ ann_98_percentile    <dbl> 8.8, 10.3, 15.0, 13.6, 12.6, 15.9, 15.2, ...
#> $ exceed               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,...
#> $ site                 <chr> "Powell River Wildwood_60", "Port Moody R...
#> $ n_days.y             <int> 344, 362, 365, 352, 351, 344, 345, 358, 3...
#> $ percent_valid_annual <dbl> 93.98907, 99.17808, 99.72678, 96.43836, 9...
#> $ percent_valid_q1     <dbl> 98.90110, 96.66667, 98.90110, 96.66667, 9...
#> $ percent_valid_q2     <dbl> 100.00000, 100.00000, 100.00000, 97.80220...
#> $ percent_valid_q3     <dbl> 98.91304, 100.00000, 100.00000, 93.47826,...
#> $ percent_valid_q4     <dbl> 78.26087, 100.00000, 100.00000, 97.82609,...
#> $ annual_valid         <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,...
#> $ quarters_valid       <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,...
#> $ use_annual           <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,...

## Finally, calcualte the 24 hr CAAQS metric: 'The 3-year average of the 
## annual 98th percentile of the daily 24-hour average concentrations'
pm_caaq_daily <- pm_24h_caaq(ann_98_per, by = "ems_id", cyear = 2013)
glimpse(pm_caaq_daily)
#> Observations: 10
#> Variables: 9
#> $ ems_id       <chr> "0220205", "0310162", "0310172", "0310175", "0310...
#> $ caaq_year    <dbl> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2...
#> $ min_year     <int> 2012, 2011, 2011, 2011, 2011, 2011, 2011, 2013, 2...
#> $ max_year     <int> 2012, 2013, 2013, 2013, 2011, 2013, 2013, 2013, 2...
#> $ n_years      <int> 1, 3, 3, 3, 1, 2, 3, 1, 3, 3
#> $ metric       <chr> "pm2.5_24h", "pm2.5_24h", "pm2.5_24h", "pm2.5_24h...
#> $ metric_value <dbl> NA, 13, 15, 13, NA, 12, 17, NA, 12, 15
#> $ caaqs        <ord> Insufficient Data, Achieved, Achieved, Achieved, ...
#> $ mgmt         <ord> Insufficient Data, Actions for Preventing Air Qua...

# Calculate the PM25 Annual Metric ------------------------------------

## Compute the annaul average
annual_avg <- pm_annual_average(avgdaily, by = "ems_id")
glimpse(annual_avg)
#> Observations: 30
#> Variables: 4
#> $ ems_id  <chr> "0220205", "0220205", "0220205", "0310162", "0310162",...
#> $ year    <int> 2011, 2012, 2013, 2011, 2012, 2013, 2011, 2012, 2013, ...
#> $ n_days  <int> 281, 342, 252, 362, 364, 352, 352, 342, 345, 357, 351,...
#> $ ann_avg <dbl> 2.6, 2.7, 3.2, 4.5, 4.6, 6.3, 6.9, 6.3, 6.4, 4.4, 4.1,...

## Then join the results to the data completeness data frame and select 
## only the results that are valid. This process could be more efficent
annual_avg <- left_join(annual_avg, pm_completeness, 
                        by = c("ems_id","year"))
annual_avg <- filter(annual_avg, use_annual)
glimpse(annual_avg)
#> Observations: 23
#> Variables: 14
#> $ ems_id               <chr> "0220205", "0310162", "0310162", "0310162...
#> $ year                 <int> 2012, 2011, 2012, 2013, 2011, 2012, 2013,...
#> $ n_days.x             <int> 342, 362, 364, 352, 352, 342, 345, 357, 3...
#> $ ann_avg              <dbl> 2.7, 4.5, 4.6, 6.3, 6.9, 6.3, 6.4, 4.4, 4...
#> $ site                 <chr> "Powell River Wildwood_60", "Port Moody R...
#> $ n_days.y             <int> 344, 362, 365, 352, 351, 344, 345, 358, 3...
#> $ percent_valid_annual <dbl> 93.98907, 99.17808, 99.72678, 96.43836, 9...
#> $ percent_valid_q1     <dbl> 98.90110, 96.66667, 98.90110, 96.66667, 9...
#> $ percent_valid_q2     <dbl> 100.00000, 100.00000, 100.00000, 97.80220...
#> $ percent_valid_q3     <dbl> 98.91304, 100.00000, 100.00000, 93.47826,...
#> $ percent_valid_q4     <dbl> 78.26087, 100.00000, 100.00000, 97.82609,...
#> $ annual_valid         <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,...
#> $ quarters_valid       <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,...
#> $ use_annual           <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,...

## Finally, calculate the Annual CAAQS metric: The 3-year average of the
## annual average concentrations. 
pm_caaq_annual <- pm_annual_caaq(annual_avg, by = "ems_id", cyear = 2013)
glimpse(pm_caaq_annual)
#> Observations: 10
#> Variables: 9
#> $ ems_id       <chr> "0220205", "0310162", "0310172", "0310175", "0310...
#> $ caaq_year    <dbl> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2...
#> $ min_year     <int> 2012, 2011, 2011, 2011, 2011, 2011, 2011, 2013, 2...
#> $ max_year     <int> 2012, 2013, 2013, 2013, 2011, 2013, 2013, 2013, 2...
#> $ n_years      <int> 1, 3, 3, 3, 1, 2, 3, 1, 3, 3
#> $ metric       <chr> "pm2.5_annual", "pm2.5_annual", "pm2.5_annual", "...
#> $ metric_value <dbl> NA, 5.1, 6.5, 5.0, NA, 5.4, 4.6, NA, 4.0, 4.8
#> $ caaqs        <ord> Insufficient Data, Achieved, Achieved, Achieved, ...
#> $ mgmt         <ord> Insufficient Data, Actions for Preventing Air Qua...
```

You can view the CAAQS Achievement Levels as well as Management Levels in the included data frames. These are used internally in `pm_annual_caaq()` and `pm_24hr_caaq()` to assign metric values at each station to the appropriate CAAQS Achievement Level and Management Level:

``` r
achievement_levels
#>      parameter       labels lower_breaks upper_breaks    units_html
#> 1           o3     Achieved            0           63           ppb
#> 2           o3 Not Achieved           63          Inf           ppb
#> 3 pm2.5_annual     Achieved            0           10 &mu;g/m&sup3;
#> 4 pm2.5_annual Not Achieved           10          Inf &mu;g/m&sup3;
#> 5    pm2.5_24h     Achieved            0           28 &mu;g/m&sup3;
#> 6    pm2.5_24h Not Achieved           28          Inf &mu;g/m&sup3;
#>   units_unicode       val_labels_html val_labels_unicode  colour
#> 1           ppb           &leq; 63ppb            = 63ppb #377EB8
#> 2           ppb            &gt; 63ppb            > 63ppb #E41A1C
#> 3         µg/m³ &leq; 10&mu;g/m&sup3;          = 10µg/m³ #377EB8
#> 4         µg/m³  &gt; 10&mu;g/m&sup3;          > 10µg/m³ #E41A1C
#> 5         µg/m³ &leq; 28&mu;g/m&sup3;          = 28µg/m³ #377EB8
#> 6         µg/m³  &gt; 28&mu;g/m&sup3;          > 28µg/m³ #E41A1C

management_levels
#>       parameter                                           labels
#> 1            o3            Actions for Keeping Clean Areas Clean
#> 2            o3 Actions for Preventing Air Quality Deterioration
#> 3            o3          Actions for Preventing CAAQS Exceedance
#> 4            o3             Actions for Achieving Air Zone CAAQS
#> 5  pm2.5_annual            Actions for Keeping Clean Areas Clean
#> 6  pm2.5_annual Actions for Preventing Air Quality Deterioration
#> 7  pm2.5_annual          Actions for Preventing CAAQS Exceedance
#> 8  pm2.5_annual             Actions for Achieving Air Zone CAAQS
#> 9     pm2.5_24h            Actions for Keeping Clean Areas Clean
#> 10    pm2.5_24h Actions for Preventing Air Quality Deterioration
#> 11    pm2.5_24h          Actions for Preventing CAAQS Exceedance
#> 12    pm2.5_24h             Actions for Achieving Air Zone CAAQS
#>    lower_breaks upper_breaks    units_html units_unicode
#> 1           0.0         50.0           ppb           ppb
#> 2          50.0         56.0           ppb           ppb
#> 3          56.0         63.0           ppb           ppb
#> 4          63.0          Inf           ppb           ppb
#> 5           0.0          4.0 &mu;g/m&sup3;         µg/m³
#> 6           4.0          6.4 &mu;g/m&sup3;         µg/m³
#> 7           6.4         10.0 &mu;g/m&sup3;         µg/m³
#> 8          10.0          Inf &mu;g/m&sup3;         µg/m³
#> 9           0.0         10.0 &mu;g/m&sup3;         µg/m³
#> 10         10.0         19.0 &mu;g/m&sup3;         µg/m³
#> 11         19.0         28.0 &mu;g/m&sup3;         µg/m³
#> 12         28.0          Inf &mu;g/m&sup3;         µg/m³
#>                                      val_labels_html
#> 1                                        &leq; 50ppb
#> 2                       &gt; 50ppb &amp; &leq; 56ppb
#> 3                       &gt; 56ppb &amp; &leq; 63ppb
#> 4                                         &gt; 63ppb
#> 5                               &leq; 4&mu;g/m&sup3;
#> 6   &gt; 4&mu;g/m&sup3; &amp; &leq; 6.4&mu;g/m&sup3;
#> 7  &gt; 6.4&mu;g/m&sup3; &amp; &leq; 10&mu;g/m&sup3;
#> 8                               &gt; 10&mu;g/m&sup3;
#> 9                              &leq; 10&mu;g/m&sup3;
#> 10  &gt; 10&mu;g/m&sup3; &amp; &leq; 19&mu;g/m&sup3;
#> 11  &gt; 19&mu;g/m&sup3; &amp; &leq; 28&mu;g/m&sup3;
#> 12                              &gt; 28&mu;g/m&sup3;
#>        val_labels_unicode  colour
#> 1                 = 50ppb #A6D96A
#> 2       > 50ppb & = 56ppb #FEE08B
#> 3       > 56ppb & = 63ppb #F46D43
#> 4                 > 63ppb #A50026
#> 5                = 4µg/m³ #A6D96A
#> 6   > 4µg/m³ & = 6.4µg/m³ #FEE08B
#> 7  > 6.4µg/m³ & = 10µg/m³ #F46D43
#> 8               > 10µg/m³ #A50026
#> 9               = 10µg/m³ #A6D96A
#> 10  > 10µg/m³ & = 19µg/m³ #FEE08B
#> 11  > 19µg/m³ & = 28µg/m³ #F46D43
#> 12              > 28µg/m³ #A50026
```

### Project Status

The package is under active development. It is working well for the calculation of PM<sub>2.5</sub> metrics, and the assignment of metrics into categories. We are working on the ozone metric calculations.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/rcaaqs/issues/).

### How to Contribute

If you would like to contribute to the package, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

    Copyright 2015 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC-RepoList) for a complete list of our repositories on GitHub.
