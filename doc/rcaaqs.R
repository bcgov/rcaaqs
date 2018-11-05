## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  dpi = 150,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.width = 8,
  warning = FALSE,
  message = FALSE
)
library(ggplot2)

## ------------------------------------------------------------------------
# For plotting
library(ggplot2)  

# For data manipulation
library(dplyr)    
library(tidyr)

# For calculations
library(rcaaqs)

## ------------------------------------------------------------------------
head(pm25_sample_data)

## ------------------------------------------------------------------------
pm25_sample_data <- pm25_sample_data %>% 
  mutate(value = clean_neg(value, type = "pm25"))

pm25_sample_data <- do(pm25_sample_data,
                       date_fill(., date_col = "date_time",
                                 fill_cols = c("ems_id", "station_name"),
                                 interval = "1 hour"))


## ------------------------------------------------------------------------
pm <- pm_annual_caaqs(pm25_sample_data, 
                     by = c("ems_id", "site"))
pm

## ------------------------------------------------------------------------
pm <- pm_annual_caaqs(pm25_sample_data, 
                     by = c("ems_id", "site"),
                     return_all = TRUE)

pm

## ------------------------------------------------------------------------
pm_daily_avg <- unnest(pm, daily_avg)
pm_daily_avg

## ---- fig.asp = 0.8------------------------------------------------------
ggplot(data = pm_daily_avg, aes(x = date, y = avg_24h)) +
  geom_line() +
  facet_wrap(~ site)

## ------------------------------------------------------------------------
high_dates <- pm_daily_avg %>%
  filter(date >= as.Date("2012-05-01"),
         date < as.Date("2012-10-01"),
         avg_24h > 10) %>%
  select(ems_id, site, date)
high_dates

## ------------------------------------------------------------------------
pm_excl <- pm_annual_caaqs(pm25_sample_data, 
                          by = c("ems_id", "site"),
                          return_all = TRUE,
                          exclude_df = high_dates,
                          exclude_df_dt = "date")

pm_excl_caaqs <- unnest(pm_excl, caaqs)
pm_excl_caaqs

## ------------------------------------------------------------------------
high_dates <- data.frame(ems_id = "0310162",
                         site = "Port Moody Rocky Point Park",
                         start = as.Date(c("2012-06-11", "2012-07-01")),
                         end = as.Date(c("2012-06-25", "2012-08-14")))
high_dates

pm_excl <- pm_annual_caaqs(pm25_sample_data, 
                          by = c("ems_id", "site"),
                          return_all = TRUE,
                          exclude_df = high_dates,
                          exclude_df_dt = c("start", "end"))

pm_excl_caaqs <- unnest(pm_excl, caaqs)
pm_excl_caaqs

## ---- echo = FALSE, fig.width = 12---------------------------------------
knitr::include_graphics(system.file('function_overview.png', package = 'rcaaqs'))

## ------------------------------------------------------------------------
pm_caaqs <- unnest(pm, caaqs) %>%
  filter(caaqs_year == 2013)

## ------------------------------------------------------------------------
library(bcmaps)
az <- bcmaps::airzones(class = "sp")
class(az)
az@data

## ------------------------------------------------------------------------
url <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Air_Monitoring_Stations/bc_air_monitoring_stations.csv"

stations <- readr::read_csv(url, na = "N/A") %>%  # Download csv file
  rename_all(tolower) %>%                         # Rename columns to lower case
  filter(ems_id %in% unique(pm$ems_id)) %>%       # Filter to ids in our pm data
  select(ems_id, latitude, longitude) %>%         # Select only the important columns
  mutate(latitude = as.numeric(latitude),         # Transform lat and lon to numeric
         longitude = as.numeric(longitude)) %>%
  distinct()                                      # Remove any duplicates

stations

## ------------------------------------------------------------------------
pm_caaqs <- left_join(pm_caaqs, stations, by = "ems_id")
pm_caaqs

## ------------------------------------------------------------------------
pm_az <- assign_airzone(pm_caaqs, airzones = az, coords = c("latitude", "longitude"))
pm_az

## ------------------------------------------------------------------------
pm_az2 <- airzone_metric(pm_az)
pm_az2

## ---- fig.width = 8, fig.asp = 1.5, eval = require("maptools")-----------
library(sp)
library(gridExtra)

az_plot <- spTransform(az, CRS("+proj=longlat"))
az_plot <- fortify(az_plot, region = "Airzone") %>%
  left_join(pm_az2, by = c("id" = "airzone"))

g1 <- ggplot(data = az_plot, aes(x = long, y = lat, group = group)) +
  theme(legend.position = c(0.85, 0.85)) +
  coord_map() +
  geom_polygon(aes(fill = caaqs)) +
  geom_path(colour = "black") +
  geom_point(data = pm_az, shape = 21, colour = "black",
             aes(x = lon, y = lat, fill = caaqs), 
             inherit.aes = FALSE) +
  scale_fill_manual(values = get_colours(type = "achievement"), drop = FALSE) +
  labs(title = "Achievement Status: PM2.5 Annual CAAQS")

g2 <- ggplot(data = az_plot, aes(x = long, y = lat, group = group)) +
  theme(legend.position = c(0.85, 0.85)) +
  coord_map() +
  geom_polygon(aes(fill = mgmt)) +
  geom_path(colour = "black") +
  geom_point(data = pm_az, shape = 21, colour = "black",
             aes(x = lon, y = lat, fill = mgmt), 
             inherit.aes = FALSE) +
  scale_fill_manual(values = get_colours(type = "management"), drop = FALSE) +
  labs(title = "Management Status: PM2.5 Annual CAAQS")

grid.arrange(g1, g2, ncol = 1)

