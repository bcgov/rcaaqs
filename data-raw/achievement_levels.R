library(tibble)
library(dplyr)

label_colours <- c("Achieved" = "#377EB8", "Not Achieved" = "#E41A1C")

units_tbl <- tibble(
  units = c("ppb", "ug/m^3"),
  units_unicode = c("ppb", "\u03BCg/m\u00B3"), 
  units_html = c("ppb", "&mu;g/m&sup3;")
)

achievement_levels <- tribble(
  ~parameter,     ~labels,        ~lower_breaks, ~upper_breaks, ~units,
  "o3",           "Achieved",     0,             63,            "ppb",
  "o3",           "Not Achieved", 63,            Inf,           "ppb",
  "pm2.5_annual", "Achieved",     0,             10.0,          "ug/m^3",
  "pm2.5_annual", "Not Achieved", 10.0,          Inf,           "ug/m^3",
  "pm2.5_24h",    "Achieved",     0,             28,            "ug/m^3",
  "pm2.5_24h",    "Not Achieved", 28,            Inf,           "ug/m^3"
) %>% 
  left_join(units_tbl, by = "units") %>% 
  mutate(val_labels_unicode = ifelse(is.infinite(upper_breaks), 
                                     paste0("\u003E ", lower_breaks, units_unicode), 
                                     paste0("\u2264 ", upper_breaks, units_unicode)), 
         val_labels_html = ifelse(is.infinite(upper_breaks), 
                                  paste0("&gt; ", lower_breaks, units_html), 
                                  paste0("&leq; ", upper_breaks, units_html)), 
         colour = label_colours[labels])
