
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcaaqs

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![Travis-CI
Build
Status](https://travis-ci.org/bcgov/rcaaqs.svg?branch=master)](https://travis-ci.org/bcgov/rcaaqs)[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

An [R](https://www.r-project.org/) package to facilitate the calculation
of air quality metrics according to the Canadian Ambient Air Quality
Standards
([CAAQS](http://www.ccme.ca/en/current_priorities/air/caaqs.html))

## Features

  - General functions for doing things like formatting dates, filling in
    sequences, etc.
  - Functions for stepwise calculation of CAAQS metrics—including
    implementing data completeness criteria—for different air
    pollutants. Currently these are complete for PM<sub>2.5</sub>
    (annual and 24hr), O<sub>3</sub>, NO<sub>2</sub>, and SO<sub>2</sub>
    metrics.
  - Functions for assigning metrics for different pollutants into
    achievement and management categories.

## Installation

The package is not available on CRAN, but can be installed using the
[devtools](https://github.com/hadley/devtools) package:

``` r
install.packages("devtools") # if not already installed

library(devtools)
install_github("bcgov/rcaaqs")
```

## Usage

This is a simple example using the included sample data set for
PM<sub>2.5</sub>.

``` r
library(rcaaqs)
library(dplyr, warn.conflicts = FALSE)

# Look at the sample data:
glimpse(pm25_sample_data)
#> Observations: 229,496
#> Variables: 6
#> $ ems_id    <chr> "0220205", "0220205", "0220205", "0220205", "0220205...
#> $ date_time <dttm> 2011-03-01 00:59:59, 2011-03-01 01:59:59, 2011-03-0...
#> $ site      <chr> "Powell River Wildwood_60", "Powell River Wildwood_6...
#> $ year      <int> 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011...
#> $ parameter <chr> "PM25", "PM25", "PM25", "PM25", "PM25", "PM25", "PM2...
#> $ value     <dbl> 2, 2, 0, 3, 1, 1, 2, 2, 2, 2, 1, 3, 1, 1, 0, 0, 0, 0...

# Compute the PM2.5 Annual CAAQS
pm_annual <- pm_annual_caaqs(pm25_sample_data, by = c("ems_id", "site"))
#> Calculating PM 2.5 daily average
#> Calculating PM 2.5 annual average
#> Calculating PM 2.5 annual CAAQS metric
glimpse(pm_annual)
#> Observations: 30
#> Variables: 14
#> $ ems_id                  <chr> "0220205", "0220205", "0220205", "0310...
#> $ site                    <chr> "Powell River Wildwood_60", "Powell Ri...
#> $ caaqs_year              <dbl> 2011, 2012, 2013, 2011, 2012, 2013, 20...
#> $ min_year                <dbl> 2011, 2012, 2012, 2011, 2011, 2011, 20...
#> $ max_year                <dbl> 2011, 2012, 2013, 2011, 2012, 2013, 20...
#> $ n_years                 <dbl> 0, 1, 1, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1,...
#> $ metric                  <chr> "pm2.5_annual", "pm2.5_annual", "pm2.5...
#> $ metric_value            <dbl> NA, NA, NA, NA, 4.6, 5.1, NA, 6.6, 6.5...
#> $ caaqs                   <ord> Insufficient Data, Insufficient Data, ...
#> $ mgmt                    <ord> Insufficient Data, Insufficient Data, ...
#> $ excluded                <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_daily_incomplete   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
#> $ flag_yearly_incomplete  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
#> $ flag_two_of_three_years <lgl> FALSE, FALSE, FALSE, FALSE, TRUE, FALS...

# Compute the PM2.5 24hr CAAQS
pm_24h <- pm_24h_caaqs(pm25_sample_data, by = c("ems_id", "site"))
#> Calculating PM 2.5 daily average
#> Calculating PM 2.5 annual 98th percentile
#> Calculating PM 2.5 24h CAAQS metric
glimpse(pm_24h)
#> Observations: 30
#> Variables: 14
#> $ ems_id                  <chr> "0220205", "0220205", "0220205", "0310...
#> $ site                    <chr> "Powell River Wildwood_60", "Powell Ri...
#> $ caaqs_year              <dbl> 2011, 2012, 2013, 2011, 2012, 2013, 20...
#> $ min_year                <dbl> 2011, 2012, 2012, 2011, 2011, 2011, 20...
#> $ max_year                <dbl> 2011, 2012, 2013, 2011, 2012, 2013, 20...
#> $ n_years                 <dbl> 0, 1, 1, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1,...
#> $ metric                  <chr> "pm2.5_24h", "pm2.5_24h", "pm2.5_24h",...
#> $ metric_value            <dbl> NA, NA, NA, NA, 13, 13, NA, 14, 15, NA...
#> $ caaqs                   <ord> Insufficient Data, Insufficient Data, ...
#> $ mgmt                    <ord> Insufficient Data, Insufficient Data, ...
#> $ excluded                <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_daily_incomplete   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
#> $ flag_yearly_incomplete  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_two_of_three_years <lgl> FALSE, FALSE, FALSE, FALSE, TRUE, FALS...
```

This is a simple example using the included sample data set for
O<sub>3</sub>.

``` r
# Look at the sample data:
glimpse(o3_sample_data)
#> Observations: 99,123
#> Variables: 8
#> $ ems_id    <chr> "E231866", "E231866", "E231866", "E231866", "E231866...
#> $ site      <chr> "Victoria Topaz", "Victoria Topaz", "Victoria Topaz"...
#> $ parameter <chr> "O3", "O3", "O3", "O3", "O3", "O3", "O3", "O3", "O3"...
#> $ date_time <dttm> 2013-01-01 00:00:00, 2013-01-01 01:00:00, 2013-01-0...
#> $ year      <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013...
#> $ month     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
#> $ value     <dbl> 13, 12, 12, 14, 19, 16, 14, 15, 12, 8, 11, 18, 19, 2...
#> $ units     <chr> "ppb", "ppb", "ppb", "ppb", "ppb", "ppb", "ppb", "pp...

# Compute the Ozone CAAQS
o3 <- o3_caaqs(o3_sample_data, by = c("ems_id", "site"))
#> Calculating O3 daily maximum of 8h average
#> Calculating O3 annual 4th highest
#> Calculating O3 CAAQS metric
glimpse(o3)
#> Observations: 12
#> Variables: 14
#> $ ems_id                  <chr> "0500886", "0500886", "0500886", "E229...
#> $ site                    <chr> "Kelowna College", "Kelowna College", ...
#> $ caaqs_year              <dbl> 2013, 2014, 2015, 2013, 2014, 2015, 20...
#> $ min_year                <dbl> 2013, 2013, 2013, 2013, 2013, 2013, 20...
#> $ max_year                <dbl> 2013, 2014, 2015, 2013, 2014, 2015, 20...
#> $ n_years                 <dbl> 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3
#> $ metric                  <chr> "o3", "o3", "o3", "o3", "o3", "o3", "o...
#> $ metric_value            <dbl> NA, 53, 54, NA, 44, 45, NA, 46, 46, NA...
#> $ caaqs                   <ord> Insufficient Data, Achieved, Achieved,...
#> $ mgmt                    <ord> Insufficient Data, Actions for Prevent...
#> $ excluded                <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_daily_incomplete   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_yearly_incomplete  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_two_of_three_years <lgl> FALSE, TRUE, FALSE, FALSE, TRUE, FALSE...
```

This is a simple example using the included sample data set for
SO<sub>2</sub>.

``` r
# Look at the sample data:
glimpse(so2_sample_data)
#> Observations: 56,533
#> Variables: 8
#> $ ems_id    <chr> "E231866", "E231866", "E231866", "E231866", "E231866...
#> $ site      <chr> "Victoria Topaz", "Victoria Topaz", "Victoria Topaz"...
#> $ parameter <chr> "SO2", "SO2", "SO2", "SO2", "SO2", "SO2", "SO2", "SO...
#> $ date_time <dttm> 2013-01-01 00:00:00, 2013-01-01 01:00:00, 2013-01-0...
#> $ year      <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013...
#> $ month     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
#> $ value     <dbl> 0.681388, 0.407499, 0.441388, 0.435833, 0.301943, 0....
#> $ units     <chr> "ppb", "ppb", "ppb", "ppb", "ppb", "ppb", "ppb", "pp...

# Compute the SO2 1-hr CAAQS (3-yr rolling average of 1hr measures)
so2_3yr <- so2_3yr_caaqs(so2_sample_data, by = c("ems_id", "site"))
#> Calculating SO2 daily maximum
#> Calculating SO2 annual 99th percentile
#> Calculating SO2 1h CAAQS metric
glimpse(so2_3yr)
#> Observations: 8
#> Variables: 14
#> $ ems_id                  <chr> "0500886", "0500886", "0500886", "E229...
#> $ site                    <chr> "Kelowna College", "Kelowna College", ...
#> $ caaqs_year              <dbl> 2013, 2014, 2015, 2013, 2013, 2014, 20...
#> $ min_year                <dbl> 2013, 2013, 2013, 2013, 2013, 2013, 20...
#> $ max_year                <dbl> 2013, 2014, 2015, 2013, 2013, 2014, 20...
#> $ n_years                 <dbl> 1, 2, 3, 0, 1, 2, 3, 0
#> $ metric                  <chr> "so2_3yr", "so2_3yr", "so2_3yr", "so2_...
#> $ metric_value            <dbl> NA, 1.8, 1.8, NA, NA, 17.1, 17.1, NA
#> $ caaqs                   <ord> Insufficient Data, Achieved, Achieved,...
#> $ mgmt                    <ord> Insufficient Data, Actions for Keeping...
#> $ excluded                <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_daily_incomplete   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_yearly_incomplete  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_two_of_three_years <lgl> FALSE, TRUE, FALSE, FALSE, FALSE, TRUE...

# Compute the SO2 annual CAAQS (1-yr average across all hourly measures)
so2_1yr <- so2_1yr_caaqs(so2_sample_data, by = c("ems_id", "site"))
#> Calculating SO2 annual average CAAQS metric
glimpse(so2_1yr)
#> Observations: 8
#> Variables: 10
#> $ ems_id                 <chr> "0500886", "0500886", "0500886", "E2297...
#> $ site                   <chr> "Kelowna College", "Kelowna College", "...
#> $ caaqs_year             <dbl> 2013, 2014, 2015, 2013, 2013, 2014, 201...
#> $ metric                 <chr> "so2_1yr", "so2_1yr", "so2_1yr", "so2_1...
#> $ metric_value           <dbl> 0.4, 0.3, 0.4, NA, 0.9, 1.0, 1.0, NA
#> $ caaqs                  <ord> Achieved, Achieved, Achieved, Insuffici...
#> $ mgmt                   <ord> Actions for Keeping Clean Areas Clean, ...
#> $ excluded               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
#> $ flag_daily_incomplete  <lgl> NA, NA, NA, NA, NA, NA, NA, NA
#> $ flag_yearly_incomplete <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
```

This is a simple example using the included sample data set for
NO<sub>2</sub>.

``` r
# Look at the sample data:
glimpse(no2_sample_data)
#> Observations: 99,083
#> Variables: 8
#> $ ems_id    <chr> "E231866", "E231866", "E231866", "E231866", "E231866...
#> $ site      <chr> "Victoria Topaz", "Victoria Topaz", "Victoria Topaz"...
#> $ parameter <chr> "NO2", "NO2", "NO2", "NO2", "NO2", "NO2", "NO2", "NO...
#> $ date_time <dttm> 2013-01-01 00:00:00, 2013-01-01 01:00:00, 2013-01-0...
#> $ year      <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013...
#> $ month     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
#> $ value     <dbl> 5.458333, 5.127500, 4.747499, 3.583333, 2.670833, 2....
#> $ units     <chr> "ppb", "ppb", "ppb", "ppb", "ppb", "ppb", "ppb", "pp...

# Compute the NO2 1-hr CAAQS (3-yr rolling average of 1hr measures)
no2_3yr <- no2_3yr_caaqs(no2_sample_data, by = c("ems_id", "site"))
#> Calculating NO2 daily maximum
#> Calculating NO2 annual 98th percentile
#> Calculating NO2 1h CAAQS metric
glimpse(no2_3yr)
#> Observations: 12
#> Variables: 14
#> $ ems_id                  <chr> "0500886", "0500886", "0500886", "E229...
#> $ site                    <chr> "Kelowna College", "Kelowna College", ...
#> $ caaqs_year              <dbl> 2013, 2014, 2015, 2013, 2014, 2015, 20...
#> $ min_year                <dbl> 2013, 2013, 2013, 2013, 2013, 2013, 20...
#> $ max_year                <dbl> 2013, 2014, 2015, 2013, 2014, 2015, 20...
#> $ n_years                 <dbl> 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3
#> $ metric                  <chr> "no2_3yr", "no2_3yr", "no2_3yr", "no2_...
#> $ metric_value            <dbl> NA, 30.3, 30.3, NA, 26.4, 27.2, NA, 36...
#> $ caaqs                   <ord> Insufficient Data, Achieved, Achieved,...
#> $ mgmt                    <ord> Insufficient Data, Actions for Prevent...
#> $ excluded                <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_daily_incomplete   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
#> $ flag_yearly_incomplete  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
#> $ flag_two_of_three_years <lgl> FALSE, TRUE, FALSE, FALSE, TRUE, FALSE...

# Compute the NO2 annual CAAQS (1-yr average across all hourly measures)
no2_1yr <- no2_1yr_caaqs(no2_sample_data, by = c("ems_id", "site"))
#> Calculating NO2 annual average CAAQS metric
glimpse(no2_1yr)
#> Observations: 12
#> Variables: 10
#> $ ems_id                 <chr> "0500886", "0500886", "0500886", "E2297...
#> $ site                   <chr> "Kelowna College", "Kelowna College", "...
#> $ caaqs_year             <dbl> 2013, 2014, 2015, 2013, 2014, 2015, 201...
#> $ metric                 <chr> "no2_1yr", "no2_1yr", "no2_1yr", "no2_1...
#> $ metric_value           <dbl> 5.9, 6.7, 7.1, 6.4, 5.8, 5.9, 9.0, 8.4,...
#> $ caaqs                  <ord> Achieved, Achieved, Achieved, Achieved,...
#> $ mgmt                   <ord> Actions for Preventing Air Quality Dete...
#> $ excluded               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
#> $ flag_daily_incomplete  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
#> $ flag_yearly_incomplete <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
```

You can view the CAAQS Achievement Levels as well as Management Levels
in the included data frames. These are used internally in
`pm_annual_caaq()` and `pm_24hr_caaq()` to assign metric values at each
station to the appropriate CAAQS Achievement Level and Management Level:

``` r
achievement_levels
#> # A tibble: 6 x 11
#>   parameter    labels       lower_breaks upper_breaks units  units_unicode
#>   <chr>        <chr>               <dbl>        <dbl> <chr>  <chr>        
#> 1 o3           Achieved                0           63 ppb    ppb          
#> 2 o3           Not Achieved           63          Inf ppb    ppb          
#> 3 pm2.5_annual Achieved                0           10 ug/m^3 μg/m³        
#> 4 pm2.5_annual Not Achieved           10          Inf ug/m^3 μg/m³        
#> 5 pm2.5_24h    Achieved                0           28 ug/m^3 μg/m³        
#> 6 pm2.5_24h    Not Achieved           28          Inf ug/m^3 μg/m³        
#> # ... with 5 more variables: units_html <chr>, val_labels <chr>,
#> #   val_labels_html <chr>, val_labels_unicode <chr>, colour <chr>

management_levels
#> # A tibble: 12 x 11
#>    parameter  labels         lower_breaks upper_breaks units units_unicode
#>    <chr>      <chr>                 <dbl>        <dbl> <chr> <chr>        
#>  1 o3         Actions for K…          0           50   ppb   ppb          
#>  2 o3         Actions for P…         50           56   ppb   ppb          
#>  3 o3         Actions for P…         56           63   ppb   ppb          
#>  4 o3         Actions for A…         63          Inf   ppb   ppb          
#>  5 pm2.5_ann… Actions for K…          0            4   ug/m… μg/m³        
#>  6 pm2.5_ann… Actions for P…          4            6.4 ug/m… μg/m³        
#>  7 pm2.5_ann… Actions for P…          6.4         10   ug/m… μg/m³        
#>  8 pm2.5_ann… Actions for A…         10          Inf   ug/m… μg/m³        
#>  9 pm2.5_24h  Actions for K…          0           10   ug/m… μg/m³        
#> 10 pm2.5_24h  Actions for P…         10           19   ug/m… μg/m³        
#> 11 pm2.5_24h  Actions for P…         19           28   ug/m… μg/m³        
#> 12 pm2.5_24h  Actions for A…         28          Inf   ug/m… μg/m³        
#> # ... with 5 more variables: units_html <chr>, val_labels <chr>,
#> #   val_labels_html <chr>, val_labels_unicode <chr>, colour <chr>
```

## Project Status

The package is under active development.

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/rcaaqs/issues/).

## How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

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

This repository is maintained by [Environmental Reporting
BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B).
Click [here](https://github.com/bcgov/EnvReportBC) for a complete list
of our repositories on GitHub.
