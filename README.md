<a rel="Exploration" 
href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img
alt="Being designed and built, but in the lab. May change, disappear, or be 
buggy." style="border-width:0" src="http://bcdevexchange.org/badge/2.svg" 
title="Being designed and built, but in the lab. May change, disappear, or be 
buggy." /></a>

---

# rcaaqs

An [R](www.r-project.org) package to faciliate the calculation of air quality 
metrics according to the Canadian Ambient Air Quality Standards
([CAAQS](http://www.ccme.ca/en/current_priorities/air/caaqs.html))

### Features

- General functions for doing things like formatting dates, filling in
  sequences, etc. 
- Functions for stepwise calculation of CAAQS metrics for
  different pollutants. Currently these are only complete for PM2.5 (annual and
  24hr) metrics, and are in development for ozone. 
- Functions for assigning
  metrics for different pollutants into achievement and management categories

### Installation

The package is not available on CRAN, but can be installed using
`devtools::install_github`:

``` r 
install.packages("devtools") # if not already installed

library(devtools)
install_github("bcgov/rcaaqs")

```

### Usage

### Project Status

The package is under active development. It is working well for the calculation
of PM2.5 metrics, and the assignemnt of metrics into categories. We are working
on the ozone metric calculations.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an 
[issue](https://github.com/bcgov/<pkg-name>/issues/).

### How to Contribute

If you would like to contribute to the package, please see our 
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

### License

Apache 2.0. See our [license](LICENSE) for more details.
