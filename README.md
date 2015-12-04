<!-- README.md is generated from README.Rmd. Please edit that file -->
<a rel="Exploration" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="http://bcdevexchange.org/badge/2.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a>

[![Travis-CI Build Status](https://travis-ci.org/bcgov/rcaaqs.svg?branch=master)](https://travis-ci.org/bcgov/rcaaqs)

------------------------------------------------------------------------

rcaaqs
======

An [R](www.r-project.org) package to faciliate the calculation of air quality metrics according to the Canadian Ambient Air Quality Standards ([CAAQS](http://www.ccme.ca/en/current_priorities/air/caaqs.html))

### Features

-   General functions for doing things like formatting dates, filling in sequences, etc.
-   Functions for stepwise calculation of CAAQS metrics for different pollutants. Currently these are only complete for PM2.5 (annual and 24hr) metrics, and are in development for ozone.
-   Functions for assigning metrics for different pollutants into achievement and management categories.

### Installation

The package is not available on CRAN, but can be installed using the [devtools](https://github.com/hadley/devtools) package:

``` r
install.packages("devtools") # if not already installed

library(devtools)
install_github("bcgov/rcaaqs")
```

### Project Status

The package is under active development. It is working well for the calculation of PM<sub>2.5</sub> metrics, and the assignemnt of metrics into categories. We are working on the ozone metric calculations.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/rcaaqs/issues/).

### How to Contribute

If you would like to contribute to the package, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

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
