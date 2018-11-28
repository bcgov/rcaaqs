library(dplyr)

pm24_caaqs <- pm_24h_caaqs(pm25_sample_data, by = c("ems_id", "site"))
pm24_caaqs
class(pm24_caaqs)
get_caaqs(pm24_caaqs)
get_daily(pm24_caaqs)
get_yearly(pm24_caaqs)
get_three_yr_rolling(pm24_caaqs)
get_by_vals(pm24_caaqs)
get_by(pm24_caaqs)
get_val(pm24_caaqs)
get_dt(pm24_caaqs)
get_param(pm24_caaqs)
plot_ts(pm24_caaqs, "0310172", "ems_id", 2013)

high_dates <- get_daily(pm24_caaqs) %>%
  filter(date >= as.Date("2012-05-01"),
         date < as.Date("2012-10-01"),
         avg_24h > 10) %>%
  select(ems_id, site, date)

pm_24h_caaqs_mgmt <- caaqs_management(pm24_caaqs, exclude_df = high_dates, exclude_df_dt = "date")
pm_24h_caaqs_mgmt
class(pm_24h_caaqs_mgmt)
get_caaqs(pm_24h_caaqs_mgmt)
get_daily(pm_24h_caaqs_mgmt)
get_yearly(pm_24h_caaqs_mgmt)
get_three_yr_rolling(pm_24h_caaqs_mgmt)

# PM2.5 annual
pm_annual_caaqs <- pm_annual_caaqs(pm25_sample_data, by = c("ems_id", "site"))
pm_annual_caaqs
class(pm_annual_caaqs)
get_caaqs(pm_annual_caaqs)
get_daily(pm_annual_caaqs)
get_yearly(pm_annual_caaqs)
get_three_yr_rolling(pm_annual_caaqs)
get_by_vals(pm_annual_caaqs)
get_by(pm_annual_caaqs)
get_val(pm_annual_caaqs)
get_dt(pm_annual_caaqs)
get_param(pm_annual_caaqs)
plot_ts(pm_annual_caaqs, "0310172", "ems_id", 2013)

high_dates <- get_daily(pm_annual_caaqs) %>%
  filter(date >= as.Date("2012-05-01"),
         date < as.Date("2012-10-01"),
         avg_24h > 10) %>%
  select(ems_id, site, date)

pm_annual_caaqs_mgmt <- caaqs_management(pm_annual_caaqs, exclude_df = high_dates, 
                                         exclude_df_dt = "date")
pm_annual_caaqs_mgmt
class(pm_annual_caaqs_mgmt)
get_caaqs(pm_annual_caaqs_mgmt)
get_daily(pm_annual_caaqs_mgmt)
get_yearly(pm_annual_caaqs_mgmt)
get_three_yr_rolling(pm_annual_caaqs_mgmt)

# o3
o3_caaqs <- o3_caaqs(o3_sample_data, by = c("ems_id", "site"))
o3_caaqs
class(o3_caaqs)
get_caaqs(o3_caaqs)
get_daily(o3_caaqs)
get_yearly(o3_caaqs)
get_three_yr_rolling(o3_caaqs)
get_by_vals(o3_caaqs)
get_by(o3_caaqs)
get_val(o3_caaqs)
get_dt(o3_caaqs)
get_param(o3_caaqs)
plot_ts(o3_caaqs, "E229797", "ems_id", 2015)

o3_high_dates <- get_daily(o3_caaqs) %>%
  filter(date >= as.Date("2015-05-01"),
         date < as.Date("2015-10-01"),
         max8hr > 50) %>%
  select(ems_id, site, date)

o3_caaqs_mgmt <- caaqs_management(o3_caaqs, exclude_df = o3_high_dates, 
                                  exclude_df_dt = "date")
o3_caaqs_mgmt
get_caaqs(o3_caaqs_mgmt)
get_daily(o3_caaqs_mgmt)
get_yearly(o3_caaqs_mgmt)
get_three_yr_rolling(o3_caaqs_mgmt)


## SO2 1yr
so2_1yr_caaqs <- so2_1yr_caaqs(so2_sample_data, by = c("ems_id", "site"))
get_hourly(so2_1yr_caaqs)
get_yearly(so2_1yr_caaqs)
get_caaqs(so2_1yr_caaqs)

so2_high_dates <- get_hourly(so2_1yr_caaqs) %>%
  filter(date_time >= as.POSIXct("2014-05-01"),
         date_time < as.POSIXct("2014-10-01"),
         value > 5) %>%
  select(ems_id, site, date_time)

so2_1yr_caaqs_mgmt <- caaqs_management(so2_1yr_caaqs, exclude_df = so2_high_dates, 
                                       exclude_df_dt = "date_time")
so2_1yr_caaqs_mgmt
get_hourly(so2_1yr_caaqs_mgmt)
get_yearly(so2_1yr_caaqs_mgmt)
get_caaqs(so2_1yr_caaqs_mgmt)

## SO2 3yr
so2_3yr_caaqs <- so2_3yr_caaqs(so2_sample_data, by = c("ems_id", "site"))
get_daily(so2_3yr_caaqs)
get_yearly(so2_3yr_caaqs)
get_caaqs(so2_3yr_caaqs)

so2_high_dates <- get_daily(so2_3yr_caaqs) %>%
  filter(date >= as.Date("2014-05-01"),
         date < as.Date("2014-10-01"),s
         max_24h > 30) %>%
  select(ems_id, site, date)

caaqs_management(so2_3yr_caaqs, exclude_df = so2_high_dates, exclude_df_dt = "date")

## NO2 1yr
no2_1yr_caaqs <- no2_1yr_caaqs(no2_sample_data, by = c("ems_id", "site"))
get_hourly(no2_1yr_caaqs)
get_yearly(no2_1yr_caaqs)
get_caaqs(no2_1yr_caaqs)

no2_1yr_caaqs_mgmt <- caaqs_management(no2_1yr_caaqs)
no2_1yr_caaqs_mgmt
get_hourly(no2_1yr_caaqs_mgmt)
get_yearly(no2_1yr_caaqs_mgmt)
get_caaqs(no2_1yr_caaqs_mgmt)

## NO2 3yr
no2_3yr_caaqs <- no2_3yr_caaqs(no2_sample_data, by = c("ems_id", "site"))
get_yearly(no2_3yr_caaqs)

no2_high_dates <- get_daily(no2_3yr_caaqs) %>%
  filter(date >= as.Date("2014-05-01"),
         date < as.Date("2014-10-01"),
         max_24h > 30) %>%
  select(ems_id, site, date)

caaqs_management(no2_3yr_caaqs)
caaqs_management(no2_3yr_caaqs, exclude_df = no2_high_dates, exclude_df_dt = "date")

