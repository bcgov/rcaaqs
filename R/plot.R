# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

#' Plot yearly CAAQS
#' 
#'  Plot yearly CAAQS with exclusions for a particular station
#'   
#' @param x an object of class `caaqs_mgmt` (i.e. output of
#'   `caaqs_management()`)
#' @param id Character. Id of the station you would like to plot
#' @param id_col Character. Column in which to look for `id`
#' @param base_size Numeric. Base font size for the plot
#' @param annot_size Numierc. size of annotations. Scaling of this size is not
#'   the same as the rest of the font sizes, so you will have to experiment.
#'   Defaults to 0.32*base_size
#' @param plot_std Logical. Should the CAAQs standard be plotted?
#' @param plot_mgmt Logical. Should the CAAQs management standards be plotted?
#' @param year_min Numeric. Minimum year to include. Data will be filtered or
#'   filled to match.
#' @param year_max Numeric. Maximum year to include. Data will be filtered or
#'   filled to match.
#'   
#' @return a ggplot2 object
#' 
#' @export
plot_caaqs <- function(x, id = NULL, id_col = NULL, 
                       year_min = NULL, year_max = NULL,
                       plot_std = TRUE, plot_mgmt = TRUE,
                       base_size = 10, annot_size = NULL) {
  
  if (!inherits(x, "caaqs_mgmt")) {
    stop("x must be an object of class 'caaqs_mgmt.", call. = FALSE)
  }
  
  if (is.null(id) && !is.null(get_by(x))) {
    stop("'id' and 'id_col' required with more than one station", call. = FALSE)
  }
  
  if (!is.null(id_col) && !id_col %in% get_by(x)) {
    stop(id_col, " is not a column in the data", call. = FALSE)
  }
  
  if (!is.null(id) && !id %in% unique(get_by_vals(x)[[id_col]])) {
    stop(id, " is not a value in ", id_col, call. = FALSE)
  }
  
  if (is.null(annot_size)) {
    annot_size <- 0.32*base_size
  } else if (!is.numeric(annot_size)) {
    stop("annot_size must be numeric")
  }
  parameter <- get_param(x)
  par_units <- setNames(plot_units(parameter), NULL)
  
  if (parameter == "pm2.5_annual") {
    ylab <- bquote(paste(PM[2.5], "Annual Metric (", 
                         ..(parse(text = par_units)), ")"), splice = TRUE)
  } else if (parameter == "pm2.5_24h") {
    ylab <- bquote(paste(PM[2.5], "24-Hour Metric (", 
                         ..(parse(text = par_units)), ")"), splice = TRUE)
  } else if (parameter == "o3") {
    ylab <- "Daily Maximum Ozone\n(parts per billion)"
  } else if (parameter == "so2_3yr") {
    ylab <- bquote(paste(SO[2], "1-Hour Metric (", 
                         ..(parse(text = par_units)), ")"), splice = TRUE)
  } else if (parameter == "so2_1yr") {
    ylab <- bquote(paste(SO[2], "Annual Metric (", 
                         ..(parse(text = par_units)), ")"), splice = TRUE)
  } else if (parameter == "no2_3yr") {
    ylab <- bquote(paste(NO[2], "1-Hour Metric (", 
                         ..(parse(text = par_units)), ")"), splice = TRUE)
  } else if (parameter == "no2_1yr") {
    ylab <- bquote(paste(NO[2], "Annual Metric (", 
                         ..(parse(text = par_units)), ")"), splice = TRUE)
  } else {
    stop(parameter, " is currently not supported in 'plot_caaqs()'")
  }
  
  # Get daily data from caaqs object and subset to the station of interest
  # Must be  
  #  - valid
  #  - in date range 

  caaqs_data <- get_caaqs(x)

  # Treat 1-year metrics separate from multi (3-yr) metrics
  if(parameter %in% c("so2_1yr", "no2_1yr")) {
    caaqs_data <- dplyr::mutate(caaqs_data, year_lab = as.character(.data$caaqs_year))
  } else {
    caaqs_data <- caaqs_data %>%
      dplyr::mutate(
        year_lab = paste0(.data$caaqs_year - 2, "-", .data$caaqs_year),
        year_lab = dplyr::if_else(.data$flag_two_of_three_years, 
                                  paste0(.data$year_lab, "*"), .data$year_lab))
  }
  
  caaqs_data <- caaqs_data %>%
    dplyr::mutate(
      raw = .data$metric_value_ambient,
      value = .data$raw - .data$metric_value_mgmt) %>%
    dplyr::select(dplyr::all_of(id_col), "caaqs_year", "year_lab", 
                  "value_adj" = .data$metric_value_mgmt, "value", "raw") %>%
    tidyr::pivot_longer(cols = tidyr::contains("value"), 
                        names_to = "type", values_to = "value") %>%
    dplyr::mutate(type = factor(.data$type, 
                                levels = c("value", "value_adj"), 
                                labels = c("No Adjustment", "TF/EE Adjusted")))

  # Filter to id
  if(!is.null(id_col) && length(unique(caaqs_data[[id_col]])) > 1) {
    caaqs_data <- dplyr::filter(caaqs_data, .data[[id_col]] == .env$id)
  }
  
  if(nrow(caaqs_data) == 0) {
    message("No valid data for that id")
    return(invisible(NULL))
  }
  
  # Pad years to year_min / year_max
  if(is.null(year_min)) year_min <- min(caaqs_data$caaqs_year)
  if(is.null(year_max)) year_max <- max(caaqs_data$caaqs_year)
  
  caaqs_data <- caaqs_data %>%
    dplyr::filter(.data$caaqs_year >= .env$year_min, 
                  .data$caaqs_year <= .env$year_max, 
                  !is.na(.data$raw))
  
  if(nrow(caaqs_data) == 0 || all(is.na(caaqs_data$value))) {
    message("No valid data for that id in that year range")
    return(invisible(NULL))
  }
  
  caaqs_data <- caaqs_data %>%
    tidyr::complete(caaqs_year = year_min:year_max, 
                    !!id_col := .env$id,  #Note id_col is from environment
                    type = c("TF/EE Adjusted", "No Adjustment")) %>%
    dplyr::mutate(year_lab = dplyr::if_else(
      is.na(.data$year_lab) | is.na(.data$value), 
      paste0(.data$caaqs_year - 2, "-", .data$caaqs_year), 
      .data$year_lab))
      
  
  # Plotting details
  mgmt <- management_levels %>%
    dplyr::filter(.data$parameter == .env$parameter)
  
  # Add padding to upper category
  ylim <- max(caaqs_data$raw, na.rm = TRUE) * 1.1
  if(plot_mgmt & ylim < (max(mgmt$lower_breaks) * 1.1)) {
    ylim <- max(mgmt$lower_breaks) * 1.1
  }
  
  std <- get_std(parameter)
  # Plot - setup
  g <- ggplot2::ggplot(data = caaqs_data, 
                       ggplot2::aes(x = .data[["year_lab"]], 
                                    y = .data[["value"]], 
                                    fill = .data[["type"]])) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"), 
      axis.line.y = element_blank(),
      axis.title.y = ggplot2::element_text(vjust = 1), 
      axis.ticks.x = ggplot2::element_blank())
  
  if(plot_mgmt) {
    g <- g + 
      ggplot2::geom_rect(
        data = mgmt, 
        aes(xmin = -Inf, xmax = Inf, 
            ymin = .data[["lower_breaks"]], 
            ymax = .data[["upper_breaks"]], 
            fill = .data[["labels"]]),
        inherit.aes = FALSE, alpha = 0.55) 
      
  } 
  # Add bars
  g <- g +
    ggplot2::geom_bar(stat = "identity", na.rm = TRUE, 
                      alpha = 1, colour = "black", width = 0.5) +
    ggplot2::scale_fill_manual(
      values = c("No Adjustment" = "#b4acb3", "TF/EE Adjusted" = "#8f94a6",
                 rev(mgmt$colour))) +
    ggplot2::scale_y_continuous(expand = c(0,0), limits = c(NA, ylim),
                                breaks = scales::breaks_extended(n = 7)) +
    ggplot2::labs(x = "Reporting Period", y = ylab)
  
  if(plot_std) {
    g <- g + 
      ggplot2::geom_hline(yintercept = std, linetype = 2, size = 1, 
                          colour = "#e41a1c") + 
      ggplot2::annotate(geom = "text", y = std, x = +Inf, 
                        label = "CAAQS", hjust = 1, vjust = -0.2)
  }
  
  if(any(grepl("\\*", caaqs_data$year_lab))) {
    g <- g + ggplot2::labs(caption = "* indicates only two of three years")
  }
    
  g
}


#' Plot daily time series data with exceedances and optionally caaqs for a 
#' particular station
#'   
#' @param x an object of class `caaqs`
#' @param id the id of the station you would like to plot
#' @param id_col the column in which to look for `id`
#' @param rep_yr The reporting year
#' @param plot_exceedances logical. Should exceedances be plotted?
#' @param base_size base font size for the plot
#' @param annot_size size of annotations. Scaling of this size is not the same 
#'   as the rest of the font sizes, so you will have to experiment. Defaults to 
#'   0.32*base_size
#' @param plot_caaqs Should the caaqs be plotted?
#'   
#' @return a ggplot2 object
#' @export
plot_ts <- function(x, id = NULL, id_col = NULL, rep_yr, plot_caaqs = TRUE,
                    plot_exceedances = FALSE, base_size = 10, 
                    annot_size = NULL) {
  
  if (!inherits(x, "caaqs")) stop("x must be an object of class 'caaqs.", 
                                  call. = FALSE)
  
  if (is.null(id) && !is.null(get_by(x))) {
    stop("id and id_col required when more than one monitoring station is present")
  }
  
  if (!is.null(id_col) && !id_col %in% get_by(x)) {
    stop(id_col, " is not a column in the data", call. = FALSE)
  }
  
  if (!is.null(id) && !id %in% unique(get_by_vals(x)[[id_col]])) {
    stop(id, " is not a value in ", id_col, call. = FALSE)
  }
  
  if (is.null(annot_size)) {
    annot_size <- 0.32*base_size
  } else if (!is.numeric(annot_size)) {
    stop("annot_size must be numeric")
  }
  
  line_col <- "#9ecae1"
  plot_std <- TRUE
  draw_caaqs <- FALSE
  caaqs_metric <- "metric_value"
  caaqs_status <- "caaqs"
  
  parameter <- get_param(x)
  
  if (parameter == "pm2.5_annual") {
    val <- "avg_24h"
    ylab <- bquote(atop('Daily Average ' ~PM[2.5],~ '(micrograms per cubic metre)'))
    param_name <- "Annual~PM[2.5]"
    if (plot_exceedances) stop("Plotting daily exceedances not meaningful for this metric")
  } else if (parameter == "pm2.5_24h") {
    val <- "avg_24h"
    ylab <- bquote(atop('Daily Average ' ~PM[2.5],~ '(micrograms per cubic metre)'))
    param_name <- "24*h~PM[2.5]"
  } else if (parameter == "o3") {
    val <-  "max8hr"
    param_name <- "Ozone"
    ylab <- "Daily Maximum Ozone\n(parts per billion)"
  } else {
    stop(parameter, " is currently not supported in plot_ts")
  }
  
  # Get daily data from caaqs object and subset to the station of interest
  daily_data <- get_daily(x)
  
  if (!is.null(id)) daily_data <- daily_data[daily_data[[id_col]] == id, ]

  std <- get_std(parameter)
  par_units <- plot_units(parameter)
  
  # daily_data <- daily_data[!is.na(daily_data[[val]]), , drop = FALSE]
  
  ## Fill in missing dates so that gaps in data are not connected
  # daily_data <- tidyr::complete(daily_data,
  #                               date = tidyr::full_seq(.data$date, 1))
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

  if (plot_caaqs) {
    caaqs_data <- get_caaqs(x)
    
    if (!is.null(id)) caaqs_data <- caaqs_data[caaqs_data[[id_col]] == id, ]
    
    caaqs_data <- caaqs_data[caaqs_data[["caaqs_year"]] == rep_yr, ]

    stopifnot(nrow(caaqs_data) == 1)
    if (is.na(caaqs_data[[caaqs_metric]])) {
      warning("caaqs not added to plot: Insufficient Data")
    } else {
      draw_caaqs <- TRUE
      min_year <- caaqs_data[["min_year"]]
      max_year <- caaqs_data[["max_year"]]
      caaqs_data$b_date <- as.Date(paste0(caaqs_data$min_year, "-01-01"))
      caaqs_data$e_date <- as.Date(paste0(caaqs_data$max_year, "-12-31"))
      
      label_pos_x <- mean(c(mindate, maxdate))
      # Put y label at higher of 98th percentile of data, or caaqs_data + 5
      high_val_in_min_year <- stats::quantile(daily_data[[val]][daily_data$date < label_pos_x], 0.98, na.rm = TRUE)
      label_pos_y <- max(high_val_in_min_year + 2, caaqs_data[[caaqs_metric]] + 5)
      seg_x <- label_pos_x + 5
      seg_xend <- seg_x + 50
      
      p <- p + geom_segment(data = caaqs_data, 
                            mapping = aes_string(x = "b_date", xend = "e_date", 
                                                 y = caaqs_metric, yend = caaqs_metric, 
                                                 colour = caaqs_status), size = 1.5)
      p <- p + annotate("text", x = label_pos_x, y = label_pos_y, 
                        label = paste0(min_year, "-", max_year, "~", param_name, "~Metric"), 
                        parse = TRUE,
                        size = annot_size, hjust = 1, colour = "grey45")
      p <- p + geom_segment(data = caaqs_data, colour = "grey45", x = as.numeric(seg_x), y = label_pos_y, 
                            xend = as.numeric(seg_xend), yend = caaqs_data[[caaqs_metric]])
      p <- p + scale_colour_manual(values = c("Achieved" = "#377eb8", "Not Achieved" = "#e41a1c"), 
                                   labels = expression(paste0(min_year, "-", max_year, "~", param_name, "~Metric")), 
                                   name = element_blank(), guide = "none")
    }
  }
  
  if (plot_std) {
    p <- p + geom_hline(aes_string(yintercept = std), linetype = 2, colour = "#e41a1c")
    # Set y label position dependent on if and where caaqs line is drawn
    if (draw_caaqs) {
      if (caaqs_data[[caaqs_metric]][1] < std || caaqs_data[[caaqs_metric]][1] - std > 5) {
        label_pos_y <- std + (base_size / 2)
      } else {
        label_pos_y <- std - (base_size / 10)
      }
    } else {
      label_pos_y <- std + (base_size / 2)
    }
    
    p <- p + annotate("text", 
                      x = maxdate, y = label_pos_y, vjust = 1, hjust = 1, 
                      size = annot_size, colour = "#e41a1c",
                      label = paste0(param_name, " ~ Standard ~ (", std, " ~ ", par_units, ")", collapse = ""), 
                      parse = TRUE)
  }
  
  p
}


#' Move annual breaks to the midpoint of the year
#' 
#' @param width The desired interval of the breaks
#'
#' @return a function
#' 
#' @noRd

mid_breaks <- function(width = "1 year") {
  function(x) {
    if (length(x) > 2) stop("x should be a range of length 2")
    sq <- scales::fullseq(x, width)
    diff <- diff(sq)
    sq[-length(sq)] + diff / 2
  }
}

#' Generate a summary plot of individual station CAAQS values, grouped by Airzone
#' 
#' @param data a data frame with columns for the metric value, the station name,
#'   and the air zone
#' @param metric_val name of column in data containing the CAAQS metric value
#'   for each station
#' @param station name of column in data containing monitoring station names
#' @param airzone name of column in data containing arizones
#' @param parameter the name of the column containing the parameter(s) of interest
#' @param base_size base font size (default 12)
#' @param pt_size size of points (default 4)
#' @param az_labeller ggplot2 labeller function to customize the airzone labels.
#'   Default `label_value` (i.e., no formatting of Airzone names). 
#'   See [ggplot2::label_value()]
#' @param ... options passed on to theme()
#'   
#' @import ggplot2
#'   
#' @return ggplot2 object
#' @export
#' 
summary_plot <- function(data, metric_val, station, airzone, parameter, 
                         base_size = 12, pt_size = 4, az_labeller = label_value, 
                         ...) {
  if (!inherits(data, "data.frame")) stop("data should be a data frame")
  if (!all(c(metric_val, station, airzone) %in% names(data))) stop("not all of specified columns are in data")
  if (!is.numeric(data[[metric_val]])) stop("specified metric column is not numeric")

  metrics <- unique(data[[parameter]])
  
  if (length(metrics) > 1) {
    facet_string <- paste0(airzone, " ~ ", parameter)
    rcaaqs_labeller <- labeller(.rows = az_labeller, .cols = param_labeller)
  } else {
    facet_string <- paste0(airzone, " ~ .")
    rcaaqs_labeller <- labeller(.rows = az_labeller)
  }
  
  lines_df <- data.frame(std = get_std(metrics), foo = "bar")
  names(lines_df)[2] <- parameter
  lines_df[[parameter]] <- rownames(lines_df)
  
  units <- unname(plot_units(metrics[1]))
  ## Conver to a call object for use in bquote
  units <- parse(text = units)[[1]]

  data[[airzone]] <- stats::reorder(data[[airzone]], data[[metric_val]], max, 
                                    order = TRUE)
  data[[airzone]] <- factor(data[[airzone]], 
                            levels = rev(levels(data[[airzone]])))
  
  # Get stations order for plotting
  data <- data %>%
    
    # Arrange in order of metric vals high to low
    dplyr::arrange(dplyr::desc(.data[[metric_val]])) %>%
    
    # Use this metric order to get Airzone order AND which metric should come first
    dplyr::mutate(ref_metric = factor(.data$metric, 
                                      levels = unique(.data$metric))) %>%
    
    # Arrange in order of metric type, then metric vals low to high
    dplyr::arrange(.data$ref_metric, .data[[metric_val]]) %>%
    
    # Use this order to get stations order
    # (note that stations not in the first metric, will go last)
    dplyr::mutate(!!station := factor(.data[[station]], 
                                      levels = unique(.data[[station]])))
  
  
  p <- ggplot(data, aes_string(x = metric_val, y = station))
  p <- p + facet_grid(facet_string, scales = "free", space = "free_y", 
                      drop = TRUE, labeller = rcaaqs_labeller)
  p <- p + geom_vline(data = lines_df, aes_string(xintercept = "std"), linetype = 2, 
                      colour = "#e41a1c")
  p <- p + xlab(bquote(CAAQS ~ Metric ~ Value ~ (.(units))))
  p <- p + ylab("Monitoring Station")
  p <- p + theme_bw(base_size = base_size)
  p <- p + guides(colour = "none")
  
  if (length(metrics) > 1) {
    p <- p + geom_point(size = pt_size)
    p <- p + aes_string(colour = parameter)
  } else {
    p <- p + geom_point(size = pt_size, colour = "#377eb8")
  }
  
  p <- p + theme(...)

  p
}

label_metrics <- function(x) {
  x[x == "pm2.5_annual"] <- "PM[2.5]~Annual~Metric"
  x[x == "pm2.5_24h"]   <- "PM[2.5]~24*'-'*hour~Metric"
  x[x == "so2_3yr"] <- "SO[2]~1*'-'*hour~Metric"
  x[x == "so2_1yr"]   <- "SO[2]~Annual~Metric"
  x[x == "no2_3yr"] <- "NO[2]~1*'-'*hour~Metric"
  x[x == "no2_1yr"]   <- "NO[2]~Annual~Metric"
  gsub("\\s", "~", x)
}

plot_units <- function(parameters) {
  c("o3" = "ppb",
    "pm2.5_annual" = "mu * g/ m^{3}",
    "pm2.5_24h" = "mu * g/ m^{3}",
    "so2_3yr" = "ppb",
    "so2_1yr" = "ppb",
    "no2_3yr" = "ppb",
    "no2_1yr" = "ppb")[parameters]
} 

param_labeller <- ggplot2::as_labeller(label_metrics, default = label_parsed)

management_map <- function(data, parameter = NULL) {
  # do stuff
}

achievement_map <- function(data, parameter = NULL) {
  # do stuff
}

achievement_plot <- function(data, parameter = NULL) {
  # do stuff
}

#' Plot date ranges at which different instruments were deployed at each station
#'
#' @param data data frame
#' @param dt a date or date/time column in data
#' @param station column in data containing station names or ids
#' @param instrument column in data containing instrument type
#'
#' @return a ggplot2 opbject
#' @export
plot_station_instruments <- function(data, dt = "date_time", station = "station_name", instrument = "instrument") {
  
  check_vars(vars = list(dt, station, instrument), data)
  check_one(dt, station, instrument)
  
  data$date <- lubridate::as_date(data[[dt]])
  
  ## conversation here discussing quosing strings: https://github.com/tidyverse/rlang/issues/116
  data <- dplyr::group_by(data, date, !!rlang::sym(station), !!rlang::sym(instrument))
  data <- dplyr::summarize(data)
  data <- dplyr::add_count(data, !!rlang::sym(station))

  ggplot(data, aes_string(x = "date", y = instrument, colour = instrument)) + 
    facet_wrap(station, scales = "free_y", ncol = 1, strip.position = "left") +
    geom_point(data = dplyr::filter(data, .data$n > 1), 
               aes(fill = "Overlap"), colour = "grey", alpha = 0.75, size = 2) +
    geom_line(size = 1) + 
    labs(y = station, fill = "") +
    theme(axis.text.y = element_blank(), 
          strip.text.y.left = element_text(angle = 0))
}
