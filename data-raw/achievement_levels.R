parameter <- c(rep("o3", 2), rep("pm2.5_annual", 2), rep("pm2.5_daily", 2))

labels <- rep(c("Achieved", 
                "Not Achieved"), 3)

lower_breaks <- c(0, 63, 
                  0, 10.0, 
                  0, 28)

upper_breaks <- c(63, Inf,
                  10.0, Inf,
                  28, Inf)

units_unicode <- c(rep("ppb", 2), 
                   rep("\u03BCg/m\u00B3", 2), 
                   rep("\u03BCg/m\u00B3", 2))

units_html <- c(rep("ppb", 2), 
                rep("&mu;g/m&sup3;", 2), 
                rep("&mu;g/m&sup3;", 2))

val_labels_unicode <- c(paste0("\u2264 ", lower_breaks[2], units_unicode[1]), 
                        paste0("\u003E ", lower_breaks[2], units_unicode[1]), 
                        paste0("\u2264 ", lower_breaks[4], units_unicode[3]), 
                        paste0("\u003E ", lower_breaks[4], units_unicode[3]), 
                        paste0("\u2264 ", lower_breaks[6], units_unicode[5]), 
                        paste0("\u003E ", lower_breaks[6], units_unicode[5]))

val_labels_html <- c(paste0("&leq; ", lower_breaks[2], units_html[1]), 
                     paste0("&gt; ", lower_breaks[2], units_html[1]), 
                     paste0("&leq; ", lower_breaks[4], units_html[3]), 
                     paste0("&gt; ", lower_breaks[4], units_html[3]), 
                     paste0("&leq; ", lower_breaks[6], units_html[5]), 
                     paste0("&gt; ", lower_breaks[6], units_html[5]))

colour <- rep(c("#377EB8", "#E41A1C"), 3)

achievement_levels <- data.frame(parameter, labels, lower_breaks, upper_breaks, 
                           units_html, units_unicode, val_labels_html, 
                           val_labels_unicode, colour)

devtools::use_data(achievement_levels, pkg = ".", overwrite = TRUE)
