library(tidyverse)

stations <- readr::read_csv("ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Air_Monitoring_Stations/bc_air_monitoring_stations.csv",
                            na = "N/A") %>%
  dplyr::rename_all(tolower) %>%
  dplyr::select(ems_id, latitude, longitude) %>%
  dplyr::mutate(latitude = as.numeric(latitude),
                longitude = as.numeric(longitude)) %>%
  dplyr::group_by(ems_id) %>%
  dplyr::slice(1)

p0 <- pm_daily_avg(pm25_sample_data, by = c("ems_id", "site"))

p1.1 <- pm_yearly_98(p0, by = c("ems_id", "site"))
p1.2 <- pm_three_yr_avg(p1.1, by = c("ems_id", "site"))
p1.3 <- pm_24h_caaq(p1.2, by = c("ems_id", "site")) %>%
  left_join(stations, by = "ems_id")

p2.1 <- pm_yearly_avg(p0, by = c("ems_id", "site"))
p2.2 <- pm_three_yr_avg(p2.1, by = c("ems_id", "site"), val = "ann_avg")
p2.3 <- pm_annual_caaq(p2.2, by = c("ems_id", "site")) %>%
  left_join(stations, by = "ems_id")

saveRDS(p0, "./tests/testthat/pm_daily.rds")

saveRDS(p1.1, "./tests/testthat/pm_24h_y98.rds")
saveRDS(p1.2, "./tests/testthat/pm_24h_3y.rds")
saveRDS(p1.3, "./tests/testthat/pm_24h_caaq.rds")

saveRDS(p2.1, "./tests/testthat/pm_ann_avg.rds")
saveRDS(p2.2, "./tests/testthat/pm_ann_3y.rds")
saveRDS(p2.3, "./tests/testthat/pm_ann_caaq.rds")
