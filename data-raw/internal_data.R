source("data-raw/achievement_levels.R")
source("data-raw/management_levels.R")

devtools::use_data(achievement_levels, management_levels, pkg = ".", 
                   internal = FALSE, overwrite = TRUE)
