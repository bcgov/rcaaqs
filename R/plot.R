#' Plot daily time series data with exceedances
#' 
#' @import ggplot2
#' @importFrom scales date_format
#'   
#' @param daily_data a dataframe of daily aggregated air quality readings with 
#'   columns: date, avg_24h (if pm25), max8hr (if o3)
#' @param caaqs_data (optional) a one-row dataframe with columns "min_year",
#'   "max_year" and columns for caaqs achievement status and management status
#' @param annual_data (optional) a dataframe of annual average pm2.5
#'   concentrations with columns "year" and "ann_avg". Used only when
#'   \code{parameter = "pm2.5_annual"}
#' @param parameter air pollutant ("o3", "pm2.5_annual", "pm2.5_daily")
#' @param rep_yr The reporting year
#' @param plot_exceedances logical. Should exceedances be plotted?
#' @param base_size base font size for the plot
#'   
#' @return a ggplot2 object
#' @export
plot_ts <- function(daily_data, caaqs_data = NULL, annual_data = NULL, parameter, 
                    rep_yr, plot_exceedances = FALSE, base_size = 10) {
  
  if (!"date" %in% names(daily_data)) stop("There is no 'date' column in daily_data")
  if (!inherits(daily_data[["date"]], c("POSIXt", "Date"))) stop("'date' column is not a valid date type")
  
  line_col <- "#9ecae1"
  plot_std <- TRUE
  annot_size <- 0.32 * base_size
  
  if (parameter == "pm2.5_annual") {
    val <- "avg_24h"
    ylab <- "Daily Average PM2.5\n(micrograms per cubic meter)"
    plot_std <- FALSE
    if (plot_exceedances) stop("Plotting daily exceedances not meaningful for this metric")
    if (!is.null(caaqs_data)) {
      if (is.null(annual_data) || !inherits(annual_data, "data.frame"))
        stop("annual_data is required for pm2.5_annual if caaqs_data is supplied")
      param_name <- "Annual PM2.5"
      caaq_metric <- "pm_annual_metric"
      caaq_status <- "caaqs_annual"
      line_col = "grey85"
      plot_std <- TRUE
    }
  } else if (parameter == "pm2.5_24h") {
    val <- "avg_24h"
    ylab <- "Daily Average PM2.5\n(micrograms per cubic meter)"
    param_name <- "24h PM2.5"
    caaq_metric <- "pm_24h_metric"
    caaq_status <- "caaqs_24h"
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
  
  # daily_data <- daily_data[!is.na(daily_data[[val]]), , drop = FALSE]
  
  min_year <- rep_yr - 2
  maxdate <- as.Date(paste0(rep_yr, "-12-31"))
  mindate <- as.Date(paste0(min_year, "-01-01"))
  
  p <- ggplot(daily_data, size = 1)
  p <- p + scale_x_date(expand = c(0, 50), limits = c(mindate - 1, maxdate), 
                 breaks = mid_breaks(), labels = scales::date_format("%Y"))
  p <- p + geom_line(aes_string(x = "date", y = val), colour = line_col, size = 0.5)
  p <- p + theme_minimal(base_size = base_size)
  p <- p + theme(axis.line =  element_line(colour = "black"), 
                 axis.title.y = element_text(vjust = 1), 
                 axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), 
                 panel.grid.minor.x = element_line(colour = "grey85"))
  p <- p + labs(x = NULL, y = ylab)
  
  if (plot_exceedances) {
    exceedance_data <- daily_data[!is.na(daily_data[[val]]), , drop = FALSE]
    exceedance_data <- exceedance_data[exceedance_data[[val]] > std, , drop = FALSE]
    
    if (nrow(exceedance_data) > 0) {
      p <- p + geom_point(data = exceedance_data, aes_string(x = "date", y = val), 
                   colour = "#e41a1c", size = 2)
      p <- p + annotate("text", x = exceedance_data[["date"]][1] + 20, y = exceedance_data[[val]][1], 
                 label = "Exceedances", hjust = 0, vjust = 0, colour = "#e41a1c", size = annot_size)
    }
  }
  
  if (!is.null(caaqs_data) && !nrow(caaqs_data) == 0) {
    stopifnot(nrow(caaqs_data) == 1)
    if (is.na(caaqs_data[[caaq_metric]])) {
      warning("caaqs not added to plot: Insufficient Data")
    } else {
      min_year <- caaqs_data[["min_year"]]
      max_year <- caaqs_data[["max_year"]]
      caaqs_data$b_date <- as.Date(paste0(caaqs_data$min_year, "-01-01"))
      caaqs_data$e_date <- as.Date(paste0(caaqs_data$max_year, "-12-31"))
      
      label_pos_x <- as.Date(paste0(min_year, "-09-15"))
      max_val_in_min_year <- max(daily_data[[val]][daily_data$date < label_pos_x], na.rm = TRUE)
      label_pos_y <- max(max_val_in_min_year + 2, caaqs_data[[caaq_metric]] + 5)
      seg_x <- label_pos_x + 5
      seg_xend <- seg_x + 50
      
      p <- p + geom_segment(data = caaqs_data, 
                            mapping = aes_string(x = "b_date", xend = "e_date", 
                                                 y = caaq_metric, yend = caaq_metric, 
                                                 colour = caaq_status), size = 1.5)
      p <- p + annotate("text", x = label_pos_x, y = label_pos_y, 
                        label = paste(min_year, "-", max_year, param_name, "Metric"), 
                        size = annot_size, hjust = 1, colour = "grey50")
      p <- p + geom_segment(colour = "grey60", x = as.numeric(seg_x), y = label_pos_y, 
                            xend = as.numeric(seg_xend), yend = caaqs_data[[caaq_metric]])
      p <- p + scale_colour_manual(values = c("Achieved" = "#377eb8", "Not Achieved" = "#e41a1c"), 
                                   labels = paste(min_year, "-", max_year, param_name, "Metric"), 
                                   name = element_blank(), guide = "none")
    }
  }
  
  if (!is.null(annual_data) && parameter == "pm2.5_annual") {
    if (nrow(annual_data) > 3) stop("annual data should only be for three years (or less)")
    annual_data$date <- as.Date(paste0(annual_data$year, "-06-30"))
    
    p <- p + geom_point(data = annual_data, 
                        aes_string(x = "date", y = "ann_avg"), size = 5)
    
  }
  
  if (plot_std) {
    p <- p + geom_hline(aes_string(yintercept = std), linetype = 2, colour = "#e41a1c")
    p <- p + annotate("text", label = paste0(param_name, " Standard (", std, " ", par_units, ")  \n"), 
                      x = maxdate, y = std - 0.5, vjust = 1, hjust = 1, 
                      size = annot_size, colour = "#e41a1c")
  }
  
  p
}


#' Move annual breaks to the midpoint of the year
#' 
#' @importFrom scales fullseq
#' @param width The desired interval of the breaks
#'
#' @return a function
mid_breaks <- function(width = "1 year") {
  function(x) {
    if (length(x) > 2) stop("x should be a range of length 2")
    sq <- scales::fullseq(x, width)
    diff <- diff(sq)
    sq[-length(sq)] + diff / 2
  }
}
