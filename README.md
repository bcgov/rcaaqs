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

This is a simple example using the included sample data set for PM<sub>2.5</sub>.

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

# Compute the daily average
avgdaily <- 
  pm_daily_avg(pm25_sample_data, 
               by = c("ems_id", "site"))
glimpse(avgdaily)
#> Observations: 9,714
#> Variables: 8
#> $ ems_id                   <chr> "0220205", "0220205", "0220205", "022...
#> $ site                     <chr> "Powell River Wildwood_60", "Powell R...
#> $ date                     <date> 2011-03-01, 2011-03-02, 2011-03-03, ...
#> $ n_readings               <int> 24, 22, 24, 24, 24, 24, 24, 24, 23, 2...
#> $ avg_24h                  <dbl> 1.0, 2.3, 2.0, 1.0, 1.6, 1.0, 1.6, 2....
#> $ exceed                   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FA...
#> $ valid_avg_24h            <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T...
#> $ flag_avg_24hr_incomplete <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FA...

## Compute yearly 98 percentiles of daily average by station 
pm_98 <- 
  pm_yearly_98(avgdaily, 
               by = c("ems_id", "site"))
glimpse(pm_98)
#> Observations: 30
#> Variables: 12
#> $ ems_id                             <chr> "0220205", "0220205", "0220...
#> $ site                               <chr> "Powell River Wildwood_60",...
#> $ year                               <int> 2011, 2012, 2013, 2011, 201...
#> $ ann_98_percentile                  <dbl> 7.4, 8.8, 8.8, 10.3, 15.0, ...
#> $ valid_in_year                      <dbl> 0.78082192, 0.94535519, 0.6...
#> $ quarter_1                          <dbl> 0.3444444, 1.0000000, 0.588...
#> $ quarter_2                          <dbl> 0.8901099, 1.0000000, 0.197...
#> $ quarter_3                          <dbl> 0.8804348, 1.0000000, 1.000...
#> $ quarter_4                          <dbl> 1.00000000, 0.78260870, 1.0...
#> $ valid_year                         <lgl> FALSE, TRUE, FALSE, TRUE, T...
#> $ exceed                             <lgl> FALSE, FALSE, FALSE, FALSE,...
#> $ flag_year_based_on_incomplete_data <lgl> FALSE, FALSE, FALSE, FALSE,...

## Compute annual averages by station 
annual_avg  <- 
  pm_yearly_avg(avgdaily, 
                by = c("ems_id", "site"))
glimpse(annual_avg)
#> Observations: 30
#> Variables: 10
#> $ ems_id        <chr> "0220205", "0220205", "0220205", "0310162", "031...
#> $ site          <chr> "Powell River Wildwood_60", "Powell River Wildwo...
#> $ year          <int> 2011, 2012, 2013, 2011, 2012, 2013, 2011, 2012, ...
#> $ ann_avg       <dbl> 2.6, 2.7, 3.2, 4.5, 4.6, 6.3, 6.9, 6.3, 6.4, 4.4...
#> $ valid_in_year <dbl> 0.78082192, 0.94535519, 0.69863014, 1.00000000, ...
#> $ quarter_1     <dbl> 0.3444444, 1.0000000, 0.5888889, 1.0000000, 1.00...
#> $ quarter_2     <dbl> 0.8901099, 1.0000000, 0.1978022, 1.0000000, 1.00...
#> $ quarter_3     <dbl> 0.8804348, 1.0000000, 1.0000000, 1.0000000, 1.00...
#> $ quarter_4     <dbl> 1.00000000, 0.78260870, 1.00000000, 1.00000000, ...
#> $ valid_year    <lgl> FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE...

## Calculate the Annual CAAQS metric: The 3-year average of the
## percentiles average concentrations. 
pm_98_just_valid <- pm_98[pm_98$exceed | pm_98$valid_year,]
pm_caaq_daily <- 
  pm_24h_caaq(pm_98_just_valid, by = "ems_id", cyear = 2013)

## Finally, calculate the Annual CAAQS metric: The 3-year average of the
## annual average concentrations. 
annual_avg_just_valid <- annual_avg[annual_avg$valid_year,]
pm_caaq_annual <- pm_annual_caaq(annual_avg_just_valid, by = "ems_id", cyear = 2013)
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

This is a simple example using the included sample data set for O<sub>3</sub>.

``` r
library(rcaaqs)
library(dplyr)

## Load sample data:
data(o3_sample_data)
glimpse(o3_sample_data)
#> Observations: 5,223,768
#> Variables: 8
#> $ ems_id    <chr> "E231866", "E231866", "E231866", "E231866", "E231866...
#> $ site      <chr> "Victoria Topaz", "Victoria Topaz", "Victoria Topaz"...
#> $ parameter <chr> "O3", "O3", "O3", "O3", "O3", "O3", "O3", "O3", "O3"...
#> $ date_time <dttm> 1998-05-01 01:00:00, 1998-05-01 02:00:00, 1998-05-0...
#> $ year      <int> 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998...
#> $ month     <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5...
#> $ value     <dbl> 0, 0, 12, 33, 33, 6, 4, 35, 34, 38, 43, 52, 55, 57, ...
#> $ units     <chr> "ppb", "ppb", "ppb", "ppb", "ppb", "ppb", "ppb", "pp...

# Compute the daily rolling 8 hour average
rolling_avg <- o3_rolling_8hr_avg(o3_sample_data, by = c("ems_id", "site"))
glimpse(rolling_avg)
#> Observations: 5,223,768
#> Variables: 10
#> $ ems_id         <chr> "E231866", "E231866", "E231866", "E231866", "E2...
#> $ site           <chr> "Victoria Topaz", "Victoria Topaz", "Victoria T...
#> $ parameter      <chr> "O3", "O3", "O3", "O3", "O3", "O3", "O3", "O3",...
#> $ date_time      <dttm> 1998-05-01 01:00:00, 1998-05-01 02:00:00, 1998...
#> $ year           <int> 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998,...
#> $ month          <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,...
#> $ value          <dbl> 0, 0, 12, 33, 33, 6, 4, 35, 34, 38, 43, 52, 55,...
#> $ units          <chr> "ppb", "ppb", "ppb", "ppb", "ppb", "ppb", "ppb"...
#> $ rolling8       <dbl> NA, NA, NA, NA, NA, 14.0, 12.6, 15.4, 19.6, 24....
#> $ flag_valid_8hr <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, ...

# Compute the daily maximum
daily_max_o3 <- o3_daily_max(rolling_avg, by = c("ems_id", "site"))
glimpse(daily_max_o3)
#> Observations: 225,511
#> Variables: 8
#> $ ems_id                 <chr> "0110031", "0110031", "0110031", "01100...
#> $ site                   <chr> "Victoria Royal Roads University", "Vic...
#> $ date                   <date> 2001-03-28, 2001-03-29, 2001-03-30, 20...
#> $ n_readings             <int> 3, 23, 23, 23, 23, 23, 23, 23, 23, 24, ...
#> $ max8hr                 <dbl> NA, 41.6, 32.3, 43.1, 46.2, 41.5, 36.5,...
#> $ exceed                 <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
#> $ valid_max8hr           <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TR...
#> $ flag_max8hr_incomplete <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...

# Compute the 4th highest daily maximum
ann_4th_highest <- o3_ann_4th_highest(daily_max_o3, by = c("ems_id", "site"))
glimpse(ann_4th_highest)
#> Observations: 684
#> Variables: 12
#> $ ems_id                             <chr> "0110031", "0110031", "0110...
#> $ site                               <chr> "Victoria Royal Roads Unive...
#> $ year                               <int> 2001, 2002, 2003, 2004, 200...
#> $ max8hr                             <dbl> 52.7, 48.9, 51.4, 51.6, 51....
#> $ valid_in_year                      <dbl> 73, 264, 336, 311, 359, 226...
#> $ quarter_1                          <dbl> 3, 0, 88, 40, 89, 58, 89, 8...
#> $ quarter_2                          <dbl> 70, 85, 90, 89, 89, 0, 87, ...
#> $ quarter_3                          <dbl> 0, 89, 69, 91, 92, 79, 52, ...
#> $ quarter_4                          <dbl> 0, 90, 89, 91, 89, 89, 92, ...
#> $ valid_year                         <lgl> FALSE, TRUE, TRUE, TRUE, TR...
#> $ exceed                             <lgl> FALSE, FALSE, FALSE, FALSE,...
#> $ flag_year_based_on_incomplete_data <lgl> FALSE, FALSE, FALSE, FALSE,...

# Compute the rolling three year average.
three_yr_avg <- o3_three_yr_avg(ann_4th_highest, by = c("ems_id", "site"))
glimpse(three_yr_avg)
#> Observations: 598
#> Variables: 15
#> $ ems_id                             <chr> "0110031", "0110031", "0110...
#> $ site                               <chr> "Victoria Royal Roads Unive...
#> $ year                               <int> 2002, 2003, 2004, 2005, 200...
#> $ max8hr                             <dbl> 48.9, 51.4, 51.6, 51.6, 47....
#> $ valid_in_year                      <dbl> 264, 336, 311, 359, 320, 34...
#> $ quarter_1                          <dbl> 0, 88, 40, 89, 89, 88, 73, ...
#> $ quarter_2                          <dbl> 85, 90, 89, 89, 87, 83, 90,...
#> $ quarter_3                          <dbl> 89, 69, 91, 92, 52, 85, 89,...
#> $ quarter_4                          <dbl> 90, 89, 91, 89, 92, 90, 89,...
#> $ valid_year                         <lgl> TRUE, TRUE, TRUE, TRUE, TRU...
#> $ exceed                             <lgl> FALSE, FALSE, FALSE, FALSE,...
#> $ flag_year_based_on_incomplete_data <lgl> FALSE, FALSE, FALSE, FALSE,...
#> $ ozone_metric                       <dbl> NA, 50.1, 50.6, 51.5, 49.7,...
#> $ valid                              <lgl> FALSE, TRUE, TRUE, TRUE, TR...
#> $ flag_two_of_three_years            <lgl> FALSE, TRUE, FALSE, FALSE, ...
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
#> 1           ppb           &leq; 63ppb            ≤ 63ppb #377EB8
#> 2           ppb            &gt; 63ppb            > 63ppb #E41A1C
#> 3         μg/m³ &leq; 10&mu;g/m&sup3;          ≤ 10μg/m³ #377EB8
#> 4         μg/m³  &gt; 10&mu;g/m&sup3;          > 10μg/m³ #E41A1C
#> 5         μg/m³ &leq; 28&mu;g/m&sup3;          ≤ 28μg/m³ #377EB8
#> 6         μg/m³  &gt; 28&mu;g/m&sup3;          > 28μg/m³ #E41A1C

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
#> 5           0.0          4.0 &mu;g/m&sup3;         μg/m³
#> 6           4.0          6.4 &mu;g/m&sup3;         μg/m³
#> 7           6.4         10.0 &mu;g/m&sup3;         μg/m³
#> 8          10.0          Inf &mu;g/m&sup3;         μg/m³
#> 9           0.0         10.0 &mu;g/m&sup3;         μg/m³
#> 10         10.0         19.0 &mu;g/m&sup3;         μg/m³
#> 11         19.0         28.0 &mu;g/m&sup3;         μg/m³
#> 12         28.0          Inf &mu;g/m&sup3;         μg/m³
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
#> 1                 ≤ 50ppb #A6D96A
#> 2       > 50ppb & ≤ 56ppb #FEE08B
#> 3       > 56ppb & ≤ 63ppb #F46D43
#> 4                 > 63ppb #A50026
#> 5                ≤ 4μg/m³ #A6D96A
#> 6   > 4μg/m³ & ≤ 6.4μg/m³ #FEE08B
#> 7  > 6.4μg/m³ & ≤ 10μg/m³ #F46D43
#> 8               > 10μg/m³ #A50026
#> 9               ≤ 10μg/m³ #A6D96A
#> 10  > 10μg/m³ & ≤ 19μg/m³ #FEE08B
#> 11  > 19μg/m³ & ≤ 28μg/m³ #F46D43
#> 12              > 28μg/m³ #A50026
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
