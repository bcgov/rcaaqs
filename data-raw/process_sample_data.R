library(readr)

pm25_sample_data <- read_csv("data-raw/pm25_sample_data.csv", 
                             col_types = cols(
                               ems_id = col_character(),
                               date_time = col_datetime(format = ""),
                               site = col_character(),
                               year = col_integer(),
                               month = col_skip(),
                               parameter = col_character(),
                               value = col_double()
                             ),
                             locale = locale(tz = "Etc/GMT+8"))

use_data(pm25_sample_data, overwrite = TRUE, compress = "xz")
