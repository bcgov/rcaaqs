library(tibble)
library(dplyr)
library(stringr)

units_tbl <- tibble(
  units = c("ppb", "ug/m^3"),
  units_unicode = c("ppb", "\u03BCg/m\u00B3"), 
  units_html = c("ppb", "&mu;g/m&sup3;")
)

label_colours <- c("Achieved" = "#377EB8", 
                   "Not Achieved" = "#E41A1C", 
                   "Actions for Keeping Clean Areas Clean" = "#A6D96A", 
                   "Actions for Preventing Air Quality Deterioration" = "#FEE08B", 
                   "Actions for Preventing CAAQS Exceedance" = "#F46D43", 
                   "Actions for Achieving Air Zone CAAQS" = "#A50026")

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
achievement_levels <- tribble(
  ~parameter,     ~labels,        ~lower_breaks, ~upper_breaks, ~units,
  "o3",           "Achieved",     0,             63,            "ppb",
  "o3",           "Not Achieved", 63,            Inf,           "ppb",
  "pm2.5_annual", "Achieved",     0,             10.0,          "ug/m^3",
  "pm2.5_annual", "Not Achieved", 10.0,          Inf,           "ug/m^3",
  "pm2.5_24h",    "Achieved",     0,             28,            "ug/m^3",
  "pm2.5_24h",    "Not Achieved", 28,            Inf,           "ug/m^3",
  "no2_1yr",      "Unknown",      0,             Inf,           "ppb",
  "no2_3yr",      "Unknown",      0,             Inf,           "ppb",
  "so2_1yr",      "Unknown",      0,             Inf,           "ppb",
  "so2_3yr",      "Unknown",      0,             Inf,           "ppb"
) %>% 
  left_join(units_tbl, by = "units") %>% 
  mutate(val_labels = ifelse(is.infinite(upper_breaks), 
                             paste0("> ", lower_breaks, units), 
                             paste0("<= ", upper_breaks, units)), 
         val_labels_html = str_replace_all(val_labels, operators_regex, replace_chars("html")), 
         val_labels_unicode = str_replace_all(val_labels, operators_regex, replace_chars("unicode")), 
         colour = label_colours[labels])


## Management levels table
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
  "no2_1yr",      "Unknown",                                          0,             Inf,           "ppb",
  "no2_3yr",      "Unknown",                                          0,             Inf,           "ppb",
  "so2_1yr",      "Unknown",                                          0,             Inf,           "ppb",
  "so2_3yr",      "Unknown",                                          0,             Inf,           "ppb"
) %>% 
  left_join(units_tbl, by = "units") %>% 
  mutate(
    val_labels = ifelse(
      is.infinite(upper_breaks), paste0("> ", lower_breaks, units), 
      ifelse(lower_breaks == 0, paste0("<= ", upper_breaks, units),
             paste0("> ", lower_breaks, units, 
                    " & <= ", upper_breaks, units))), 
    val_labels_html = str_replace_all(val_labels, operators_regex, replace_chars("html")), 
    val_labels_unicode = str_replace_all(val_labels, operators_regex, replace_chars("unicode")), 
    colour = label_colours[labels])

## Save data
devtools::use_data(achievement_levels, management_levels, pkg = ".", 
                   internal = FALSE, overwrite = TRUE)
