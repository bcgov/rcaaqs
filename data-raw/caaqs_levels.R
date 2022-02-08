# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

library(tibble)
library(dplyr)
library(stringr)

units_tbl <- tibble(
  units = c("ppb", "ug/m^3"),
  units_unicode = c("ppb", "\u03BCg/m\u00B3"), 
  units_html = c("ppb", "&mu;g/m&sup3;")
)

label_colours_hex <- c("Achieved" = "#377eb8", 
                   "Not Achieved" = "#e41a1c", 
                   "Actions for Keeping Clean Areas Clean" = "#A6D96A", 
                   "Actions for Preventing Air Quality Deterioration" = "#FEE08B", 
                   "Actions for Preventing CAAQS Exceedance" = "#F46D43", 
                   "Actions for Achieving Air Zone CAAQS" = "#A50026")

label_colours <- c("Achieved" = "blue", 
                   "Not Achieved" = "red", 
                   "Actions for Keeping Clean Areas Clean" = "green", 
                   "Actions for Preventing Air Quality Deterioration" = "yellow", 
                   "Actions for Preventing CAAQS Exceedance" = "orange", 
                   "Actions for Achieving Air Zone CAAQS" = "red")

## A function factory that creates a function that takes one argument (an operator)
## and converts it to the unicode or html version 
replace_chars <- function(replace_with = c("html", "unicode")) {
  replace_with <- match.arg(replace_with)
  
  comparison_tbl <- tibble(
    operator =  c("<=", ">=", "<", ">", "&", "ug/m^3"),
    unicode = c("\u2264", "\u2265", "<", ">", "&", "\u03BCg/m\u00B3"), 
    html = c("&lteq;", "&gteq;", "&lt;", "&gt;", "&amp;", "&mu;g/m&sup3;")
  )
  
  function(operator) {
    vapply(operator, function(x) {
      comparison_tbl[[replace_with]][comparison_tbl$operator == x]
    }, 
    FUN.VALUE = character(1), USE.NAMES = FALSE)
  }
}

## Strings to search for to replace with html/unicode tags - order matters here (specific to general)!
operators_regex <- paste0(c("<=", ">=", "<", ">", "&", "ug/m\\^3"), collapse = "|")


## Achievement levels table
# Filled in SO2 and NO2 from: http://airquality-qualitedelair.ccme.ca/en/
achievement_levels <- tribble(
  ~parameter,     ~labels,        ~lower_breaks, ~upper_breaks, ~units,
  "o3",           "Achieved",        0,             62,           "ppb",
  "o3",           "Not Achieved",   62,            Inf,           "ppb",
  
  "pm2.5_annual", "Achieved",        0,            8.8,        "ug/m^3",
  "pm2.5_annual", "Not Achieved",  8.8,            Inf,        "ug/m^3",

  "pm2.5_24h",    "Achieved",        0,             27,        "ug/m^3",
  "pm2.5_24h",    "Not Achieved",   27,            Inf,        "ug/m^3",
  
  "no2_1yr",      "Achieved",        0,             17,           "ppb",
  "no2_1yr",      "Not Achieved",    17,           Inf,           "ppb",
  
  "no2_3yr",      "Achieved",        0,             60,           "ppb",
  "no2_3yr",      "Not Achieved",   60,            Inf,           "ppb",
  
  "so2_1yr",      "Achieved",        0,            5.0,           "ppb",
  "so2_1yr",      "Not Achieved",  5.0,            Inf,            "ppb",
  
  "so2_3yr",      "Achieved",        0,             70,            "ppb",
  "so2_3yr",      "Not Achieved",   70,            Inf,            "ppb"
) %>% 
  left_join(units_tbl, by = "units") %>% 
  mutate(parameter = factor(parameter),
         val_labels = ifelse(is.infinite(upper_breaks), 
                             paste0("> ", lower_breaks, units), 
                             paste0("<= ", upper_breaks, units)), 
         val_labels_html = str_replace_all(val_labels, operators_regex, replace_chars("html")), 
         val_labels_unicode = str_replace_all(val_labels, operators_regex, replace_chars("unicode")), 
         colour = label_colours_hex[labels], 
         colour_text = label_colours[labels])


## Management levels table
# Filled in SO2 and NO2 from: http://airquality-qualitedelair.ccme.ca/en/
management_levels <- tribble(
  ~parameter,     ~labels,                                            ~lower_breaks, ~upper_breaks, ~units,
  "o3",           "Actions for Keeping Clean Areas Clean",            0,             50,            "ppb",
  "o3",           "Actions for Preventing Air Quality Deterioration", 50,            56,            "ppb",
  "o3",           "Actions for Preventing CAAQS Exceedance",          56,            63,            "ppb",
  "o3",           "Actions for Achieving Air Zone CAAQS",             63,            Inf,           "ppb",
  
  "pm2.5_annual", "Actions for Keeping Clean Areas Clean",            0,             4.0,           "ug/m^3",
  "pm2.5_annual", "Actions for Preventing Air Quality Deterioration", 4.0,           6.4,           "ug/m^3",
  "pm2.5_annual", "Actions for Preventing CAAQS Exceedance",          6.4,           10.0,          "ug/m^3",
  "pm2.5_annual", "Actions for Achieving Air Zone CAAQS",             10.0,          Inf,           "ug/m^3",
  
  "pm2.5_24h",    "Actions for Keeping Clean Areas Clean",            0,             10,            "ug/m^3",
  "pm2.5_24h",    "Actions for Preventing Air Quality Deterioration", 10,            19,            "ug/m^3",
  "pm2.5_24h",    "Actions for Preventing CAAQS Exceedance",          19,            28,            "ug/m^3",
  "pm2.5_24h",    "Actions for Achieving Air Zone CAAQS",             28,            Inf,           "ug/m^3",
  
  "no2_1yr",      "Actions for Keeping Clean Areas Clean",             0,               2,           "ppb",
  "no2_1yr",      "Actions for Preventing Air Quality Deterioration",  2,               7,           "ppb",
  "no2_1yr",      "Actions for Preventing CAAQS Exceedance",           7,              17,           "ppb",
  "no2_1yr",      "Actions for Achieving Air Zone CAAQS",              17,            Inf,           "ppb",

  "no2_3yr",      "Actions for Keeping Clean Areas Clean",             0,              20,           "ppb",
  "no2_3yr",      "Actions for Preventing Air Quality Deterioration",  20,             31,           "ppb",
  "no2_3yr",      "Actions for Preventing CAAQS Exceedance",           31,             60,           "ppb",
  "no2_3yr",      "Actions for Achieving Air Zone CAAQS",              60,            Inf,           "ppb",
  
  "so2_1yr",      "Actions for Keeping Clean Areas Clean",             0,               2,           "ppb",
  "so2_1yr",      "Actions for Preventing Air Quality Deterioration",  2,               3,           "ppb",
  "so2_1yr",      "Actions for Preventing CAAQS Exceedance",           3,               5,           "ppb",
  "so2_1yr",      "Actions for Achieving Air Zone CAAQS",              5,             Inf,           "ppb",
  
  "so2_3yr",      "Actions for Keeping Clean Areas Clean",            0,               30,           "ppb",
  "so2_3yr",      "Actions for Preventing Air Quality Deterioration", 30,              50,           "ppb",
  "so2_3yr",      "Actions for Preventing CAAQS Exceedance",          50,              70,           "ppb",
  "so2_3yr",      "Actions for Achieving Air Zone CAAQS",             70,             Inf,           "ppb"
) %>% 
  left_join(units_tbl, by = "units") %>% 
  mutate(
    parameter = factor(parameter),
    val_labels = ifelse(
      is.infinite(upper_breaks), paste0("> ", lower_breaks, units), 
      ifelse(lower_breaks == 0, paste0("<= ", upper_breaks, units),
             paste0("> ", lower_breaks, units, 
                    " & <= ", upper_breaks, units))), 
    val_labels_html = str_replace_all(val_labels, operators_regex, replace_chars("html")), 
    val_labels_unicode = str_replace_all(val_labels, operators_regex, replace_chars("unicode")), 
    colour = label_colours_hex[labels], 
    colour_text = label_colours[labels])

## Save data
usethis::use_data(achievement_levels, management_levels, 
                  internal = FALSE, overwrite = TRUE)
