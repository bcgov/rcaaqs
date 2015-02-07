parameter <- c(rep("o3", 4), rep("pm2.5_annual", 4), rep("pm2.5_daily", 4))

labels <- rep(c("Actions for Keeping Clean Areas Clean", 
                "Actions for Preventing Air Quality Deterioration", 
                "Actions for Preventing CAAQS Exceedance", 
                "Actions for Achieving Air Zone CAAQS"), 3)

lower_breaks <- c(0, 50, 56, 63, 
                  0, 4.0, 6.4, 10.0, 
                  0, 10, 19, 28)

upper_breaks <- c(50, 56, 63, Inf,
                  4.0, 6.4, 10.0, Inf,
                  10, 19, 28, Inf)

units_unicode <- c(rep("ppb", 4), 
                   rep("\u03BCg/m\u00B3", 4), 
                   rep("\u03BCg/m\u00B3", 4))

units_html <- c(rep("ppb", 4), 
                rep("&mu;g/m&sup3;", 4), 
                rep("&mu;g/m&sup3;", 4))

val_labels_unicode <- c(paste0("\u2264 ", lower_breaks[2], units_unicode[1]), 
                        paste0("\u003E ", lower_breaks[2], units_unicode[1], 
                               " \u0026 \u2264 ", lower_breaks[3], units_unicode[1]), 
                        paste0("\u003E ", lower_breaks[3], units_unicode[1], 
                               " \u0026 \u2264 ", lower_breaks[4], units_unicode[1]), 
                        paste0("\u003E ", lower_breaks[4], units_unicode[1]), 
                        paste0("\u2264 ", lower_breaks[6], units_unicode[5]), 
                        paste0("\u003E ", lower_breaks[6], units_unicode[5], 
                               " \u0026 \u2264 ", lower_breaks[7], units_unicode[5]), 
                        paste0("\u003E ", lower_breaks[7], units_unicode[5], 
                               " \u0026 \u2264 ", lower_breaks[8], units_unicode[5]), 
                        paste0("\u003E ", lower_breaks[8], units_unicode[5]), 
                        paste0("\u2264 ", lower_breaks[10], units_unicode[9]), 
                        paste0("\u003E ", lower_breaks[10], units_unicode[9], 
                               " \u0026 \u2264 ", lower_breaks[11], units_unicode[9]), 
                        paste0("\u003E ", lower_breaks[11], units_unicode[9], 
                               " \u0026 \u2264 ", lower_breaks[12], units_unicode[9]), 
                        paste0("\u003E ", lower_breaks[12], units_unicode[9]))

val_labels_html <- c(paste0("&leq; ", lower_breaks[2], units_html[1]), 
                     paste0("&gt; ", lower_breaks[2], units_html[1], 
                            " &amp; &leq; ", lower_breaks[3], units_html[1]), 
                     paste0("&gt; ", lower_breaks[3], units_html[1], 
                            " &amp; &leq; ", lower_breaks[4], units_html[1]), 
                     paste0("&gt; ", lower_breaks[4], units_html[1]), 
                     paste0("&leq; ", lower_breaks[6], units_html[5]), 
                     paste0("&gt; ", lower_breaks[6], units_html[5], 
                            " &amp; &leq; ", lower_breaks[7], units_html[5]), 
                     paste0("&gt; ", lower_breaks[7], units_html[5], 
                            " &amp; &leq; ", lower_breaks[8], units_html[5]), 
                     paste0("&gt; ", lower_breaks[8], units_html[5]), 
                     paste0("&leq; ", lower_breaks[10], units_html[9]), 
                     paste0("&gt; ", lower_breaks[10], units_html[9], 
                            " &amp; &leq; ", lower_breaks[11], units_html[9]), 
                     paste0("&gt; ", lower_breaks[11], units_html[9], 
                            " &amp; &leq; ", lower_breaks[12], units_html[9]), 
                     paste0("&gt; ", lower_breaks[12], units_html[9]))

colour <- rep(c("#A6D96A", "#FEE08B", "#F46D43", "#A50026"), 3)

management_levels <- data.frame(parameter, labels, lower_breaks, upper_breaks, 
                           units_html, units_unicode, val_labels_html, 
                           val_labels_unicode, colour)

devtools::use_data(management_levels, pkg = ".", overwrite = TRUE)
