#' Plot daily time series data with exceedances
#' 
#' @import ggplot2 scales
#'
#' @param daily_data a dataframe of daily aggregated air quality readings with
#'   columns: date, avg_24h (if pm25), max8hr (if o3)
#' @param parameter air pollutant ("o3", "pm2.5_annual", "pm2.5_daily")
#' @param caaqs_data (optional) a one-row dataframe
#' @param rep_yr The reporting year
#' @param plot_exceedances logical. Should exceedances be plotted?
#'
#' @return a ggplot2 object
#' @export
  
  if (!"date" %in% names(daily_data)) stop("There is no 'date' column in daily_data")
  if (!inherits(daily_data[["date"]], c("POSIXt", "Date"))) stop("'date' column is not a valid date type")

  if (grepl("pm2.5", parameter)) {
    val <- "avg24h"
    ylab <- "Daily Average PM2.5\n(micrograms per cubic meter)"
    if (parameter == "pm2.5_annual") {
      if (plot_exceedances) stop("Plotting daily exceedances not meaningful for this metric")
      param_name <- "Annual PM2.5"
      caaq_metric <- "pm_annual_metric"
      caaq_status <- "caaqs_annual"
    } else if (parameter == "pm2.5_24h") {
      param_name <- "24h PM2.5"
      caaq_metric <- "pm_24h_metric"
      caaq_status <- "caaqs_24h"
    }
  } else if (parameter == "o3") {
    val <-  "max8hr"
    param_name <- "Ozone"
    ylab <- "Daily Maximum Ozone\n(parts per billion)"
  } else {
    stop(parameter, " is not a valid parameter name")
  }
  
  if (!val %in% names(daily_data)) stop(val, " column is not present in daily_data")
  if (!inherits(daily_data[[val]], "numeric")) stop(val, " is not numeric")
  
  param_levels <- rcaaqs::get_levels("achievement", parameter)
  std <- param_levels$lower_breaks[param_levels$labels == "Not Achieved"]
  par_units <- as.character(param_levels$units_unicode[1])
  
  daily_data <- daily_data[!is.na(daily_data[[val]]), , drop = FALSE]
  
  min_year <- rep_yr - 2
  maxdate <- as.Date(paste0(rep_yr, "-12-31"))
  mindate <- as.Date(paste0(min_year, "-01-01"))
  
  p <- ggplot(daily_data, size = 1) + 
    scale_x_date(expand = c(0, 50), limits = c(mindate - 1, maxdate), 
                 breaks = date_breaks(width = "1 year"), labels = date_format("%Y")) + 
    geom_line(aes_string(x = "date", y = val), colour = "#9ecae1", size = 0.5) + 
    geom_hline(aes_string(yintercept = std), linetype = 2, colour = "#e41a1c") + 
    annotate("text", label = paste0(param_name, " Standard (", std, " ", par_units, ")  \n"), 
             x = maxdate, y = std, vjust = 0.3, hjust = 1, 
             size = 3.5, colour = "#e41a1c") + 
    theme_minimal(base_size = 10) + 
    theme(axis.line =  element_line(colour = "black"), axis.title.y = element_text(vjust = 1)) + 
    labs(x = NULL, y = ylab)
  
  if (plot_exceedances) {
    exceedance_data <- daily_data[daily_data[[val]] > std, , drop = FALSE]
    
    if (nrow(exceedance_data) > 0) {
      p <- p + 
        geom_point(data = exceedance_data, aes_string(x = "date", y = val), 
                   colour = "#e41a1c", size = 2) + 
        annotate("text", x = exceedance_data[["date"]][1] + 20, y = exceedance_data[[val]][1], 
                 label = "Exceedances", hjust = 0, vjust = 0, colour = "#e41a1c", size = 3)
    }
  }
  
  if (!is.null(caaqs_data)) {
    stopifnot(nrow(caaqs_data) == 1)
    min_year <- caaqs_data[["min_year"]]
    max_year <- caaqs_data[["max_year"]]
    caaqs_data$b_date <- as.Date(paste0(caaqs_data$min_year, "-01-01"))
    caaqs_data$e_date <- as.Date(paste0(caaqs_data$max_year, "-12-31"))
    
    label_pos_x <- as.Date(paste0(min_year, "-09-15"))
    label_pos_y <- ggplot_build(p)$panel$ranges[[1]]$y.range[2]
    seg_x <- label_pos_x + 5
    seg_xend <- seg_x + 50
    
    p <- p + 
      geom_segment(data = caaqs_data, 
                   mapping = aes_string(x = "b_date", xend = "e_date", 
                                 y = caaq_metric, yend = caaq_metric, 
                                 colour = caaq_status),  
                   size = 1.5) + 
      annotate("text", x = label_pos_x, y = label_pos_y, 
               label = paste(min_year, "-", max_year, param_name, "Metric"), 
               size = 3.5, hjust = 1, colour = "grey50") + 
      geom_segment(colour = "grey60", x = as.numeric(seg_x), y = label_pos_y, 
               xend = as.numeric(seg_xend), yend = caaqs_data[[caaq_metric]]) +
      scale_colour_manual(values = c("#377eb8", "#e41a1c"), 
                          labels = paste(min_year, "-", max_year, param_name, "Metric"), 
                          name = element_blank(), guide = "none")
  }
  
  p
}
