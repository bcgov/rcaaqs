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

val_labels_unicode <- c(paste0("\u2264 ", breaks[2], units_unicode[1]), 
                        paste0("\u003E ", breaks[2], units_unicode[1], 
                               " \u0026 \u2264 ", breaks[3], units_unicode[1]), 
                        paste0("\u003E ", breaks[3], units_unicode[1], 
                               " \u0026 \u2264 ", breaks[4], units_unicode[1]), 
                        paste0("\u003E ", breaks[4], units_unicode[1]), 
                        paste0("\u2264 ", breaks[6], units_unicode[5]), 
                        paste0("\u003E ", breaks[6], units_unicode[5], 
                               " \u0026 \u2264 ", breaks[7], units_unicode[5]), 
                        paste0("\u003E ", breaks[7], units_unicode[5], 
                               " \u0026 \u2264 ", breaks[8], units_unicode[5]), 
                        paste0("\u003E ", breaks[8], units_unicode[5]), 
                        paste0("\u2264 ", breaks[10], units_unicode[9]), 
                        paste0("\u003E ", breaks[10], units_unicode[9], 
                               " \u0026 \u2264 ", breaks[11], units_unicode[9]), 
                        paste0("\u003E ", breaks[11], units_unicode[9], 
                               " \u0026 \u2264 ", breaks[12], units_unicode[9]), 
                        paste0("\u003E ", breaks[12], units_unicode[9]))

val_labels_html <- c(paste0("&leq; ", breaks[2], units_html[1]), 
                     paste0("&gt; ", breaks[2], units_html[1], 
                            " &amp; &leq; ", breaks[3], units_html[1]), 
                     paste0("&gt; ", breaks[3], units_html[1], 
                            " &amp; &leq; ", breaks[4], units_html[1]), 
                     paste0("&gt; ", breaks[4], units_html[1]), 
                     paste0("&leq; ", breaks[6], units_html[5]), 
                     paste0("&gt; ", breaks[6], units_html[5], 
                            " &amp; &leq; ", breaks[7], units_html[5]), 
                     paste0("&gt; ", breaks[7], units_html[5], 
                            " &amp; &leq; ", breaks[8], units_html[5]), 
                     paste0("&gt; ", breaks[8], units_html[5]), 
                     paste0("&leq; ", breaks[10], units_html[9]), 
                     paste0("&gt; ", breaks[10], units_html[9], 
                            " &amp; &leq; ", breaks[11], units_html[9]), 
                     paste0("&gt; ", breaks[11], units_html[9], 
                            " &amp; &leq; ", breaks[12], units_html[9]), 
                     paste0("&gt; ", breaks[12], units_html[9]))

colour <- rep(c("#A6D96A", "#FEE08B", "#F46D43", "#A50026"), 3)

caaqs_levels <- data.frame(parameter, labels, lower_breaks, upper_breaks, 
                           units_html, units_unicode, val_labels_html, 
                           val_labels_unicode, colour)

devtools::use_data(caaqs_levels, pkg = ".", overwrite = TRUE)
