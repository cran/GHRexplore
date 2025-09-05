#' Time series plot of two variables in two different axes
#'
#' @description Plots dual-axis time series of two covariates, case counts, or incidence 
#' rates.
#' 
#' @param data Data frame containing equally spaced (daily, weekly, monthly) 
#' covariate or disease case observations for one or multiple locations.
#' @param var A character vector of length 2 (left axis, right axis) identifying 
#' the variables to be plotted.
#' @param time Name of the variable that identifies the temporal dimension
#' of the data frame. Its values must be in date format ("yyyy-mm-dd")
#' representing the day of observation for daily data, the first day of the 
#' week for weekly, or the first day of the month for monthly observations.
#' @param type A character vector of length 2 (left axis, right axis) that 
#' specifies the types of variable in `var`. Possible values include 
#' 'cov' (covariate, default), 'counts' (case counts), and 'inc' (case incidence).
#'  If `type='inc'`, `pop` is required.
#' @param pop Character identifying the variable name for population. Only needed 
#' if `type='inc'`.
#' @param pt Numerical only used for `type='inc'`. It represents the scale of the
#' person-time (default 100,000) for incidence rates.
#' @param area Name of variable that identifies the different locations
#' (e.g., areal units) for which a time series is available.
#' @param aggregate_space Name of variable used to define spatial aggregation groups.
#' @param aggregate_time Temporal scale used to perform
#' temporal aggregation. Options are: "week" (ISO 8601), "month", "year".
#' @param aggregate_space_fun Character indicating the function to be used
#' in the aggregation over space for `type="cov"`. Options are "mean" (default),
#' "median", "sum". For case counts and incidence, "sum" is always applied.
#' @param aggregate_time_fun Character indicating the function to be used
#' in the aggregation over time for `type="cov"`. Options are "mean" (default),
#' "median", "sum". For case counts and incidence, "sum" is always applied.
#' @param align Options to align the two plots. Defaults to "min", which forces
#' the minimum of the two variables to be aligned. Other options include "mean" and 
#' "median". 
#' @param title Optional title of the plot. 
#' @param var_label A character vector of length 2 (left axis, right axis) with 
#' custom names for the case or covariate variable.
#' @param legend Character with a custom name for the legend.
#' @param ylab A character vector of length 2  (left, right) for the y-axes.
#' @param xlab Label for the x-axis.
#' @param free_y_scale Logical, default FALSE. Allows different scales in the 
#' y_axis when facets are used.
#' @param palette A character vector of length 2 (left axis, right axis) indicating
#' the colours (R or hex codes) to use for each of the two variables).
#' @param alpha Numerical between 0 and 1 determining the transparency of the 
#' lines. 
#' @return A dual-axis ggplot2 time series plot.
#' @seealso [plot_timeseries] for single-axis time series plots.
#' @export
#'
#' @examples
#' # Load data
#' data("dengue_MS")
#' data("dengue_SP")
#' 
#' # Plotting two covariates with temporal aggregation, align using the mean
#' plot_timeseries2(dengue_SP,
#'                  var = c("temp_med", "precip_tot"),
#'                  time = "date",
#'                  align = "mean",
#'                  aggregate_time = "month")
#' 
#' # Plotting case incidence and a covariate with temporal aggregation 
#' # and customized colours and labels
#' plot_timeseries2(dengue_SP,
#'                  var = c("cases", "precip_tot"),
#'                  type = c("inc", "cov"),
#'                  var_label = c("Incidence", "Precipitation"),
#'                  title = "Precipitation and dengue incidence in Sao Paulo",
#'                  time = "date",
#'                  pop = "pop",
#'                  aggregate_time = "month",
#'                  palette = c("darkgreen", "royalblue"),
#'                  alpha = 0.8)
#'                  
#' # Plotting case incidence and a covariate with spatial aggregation
#' plot_timeseries2(dengue_MS,
#'                  var = c("dengue_cases", "pdsi"),
#'                  type = c("inc", "cov"),
#'                  pop = "population",
#'                  time = "date",
#'                  area = "micro_code",
#'                  aggregate_space = "meso_code")
#' 

plot_timeseries2 <- function(data,
                             var,
                             time,
                             type = c("cov", "cov"),
                             pop = NULL,
                             pt = 100000,
                             area = NULL,
                             aggregate_space = NULL,
                             aggregate_time = NULL,
                             aggregate_space_fun = "mean",
                             aggregate_time_fun = "mean",
                             align = "min",
                             title = NULL,
                             var_label = NULL,
                             legend = "Variable", 
                             ylab = NULL,
                             xlab = NULL,
                             free_y_scale = FALSE,
                             palette = c("#168c81", "#B98AFB"),
                             alpha = 0.9) {
  
  # Input checks ----
  
  # Check data exists and is a data.frame
  if (missing(data)) {
    stop("Error: Missing required argument 'data'")
  } else if (!is.data.frame(data)) {
    stop("'data' should be a data.frame")
  }
  
  # Check if numeric 'var' exists in data
  if (missing(var)) {
    stop("Error: Missing required argument 'var'")
  }else if(length(var) != 2){
    stop("Error: 'var' must have length 2")
  }else if (!all(var %in% names(data))) {
    stop("The columns of the data do not include the 'var' argument")
  }else if (!is.numeric(data[[var[1]]]) | !is.numeric(data[[var[2]]])) {
    stop("'var' should be numeric")
  }else{
    .check_na(var[1], data)
    .check_na(var[2], data)
  }
  
  # Check time exists, is in the data.frame and is in date format
  if (missing(time)) {
    stop("Error: Missing required argument 'time'")
  } else if (is.null(data[[time]])) {
    stop("'time' not found in the data")
  } else if(any(is.na(.ymd_strict(data[[time]])))){
    stop("'Date' should be in 'yyyy-mm-dd' format")
  }
  
  # Check that 'type' is valid
  if(length(type) != 2){
    stop("Error: 'var' must have length 2")
  }else if (!all(type %in% c("cov","counts", "inc"))) {
    stop("type must be either 'cov', 'counts' or 'inc'")
  }
  
  # Check if 'area' exists in data
  if (!is.null(area) && is.null(data[[area]])) {
    stop("No column of the data matches the 'area' argument")
  }
  
  # Check that 'aggregate_space' is valid if specified
  if (!is.null(aggregate_space) && is.null(data[[aggregate_space]])) {
    stop("No column of the data match the 'aggregate_space' argument")
  }
  
  # Check that if 'aggregate_space' is valid area is also specified
  if (!is.null(aggregate_space) && is.null(area)) {
    stop("No 'area' argument provided")
  }
  
  # Check that 'aggregate_space_fun is one of the following functions
  # (sum , mean , median ) if specified.
  if (!is.null(aggregate_space) && !aggregate_space_fun %in% c(
    "sum", "mean", "median"
  )) {
    stop("aggregate_space_fun can be 'sum', 'mean' 'median'")
  } else if(!missing(aggregate_space_fun) && all(type != "cov")){
    message(paste0("'aggregate_space_fun' for case counts and incidence rates ",
                   "is predefined and cannot be modified."))
  }
  
  # Check that 'aggregate_time' is valid if specified
  if (!is.null(aggregate_time) && !(aggregate_time %in% c(
    "week", "month", "year"
  ))) {
    stop("'aggregate_time' can be 'week','month', 'year'")
  }
  
  # Check that 'aggregate_time_fun is one of the following functions
  # (sum , mean , median) if specified.
  if (!is.null(aggregate_time) && !aggregate_time_fun %in% c(
    "sum", "mean", "median"
  )) {
    stop("aggregate_time_fun can be 'sum', 'mean', 'median'")
  }else if(!missing(aggregate_time_fun) && all(type != "cov")){
    warning(paste0("'aggregate_time_fun' for case counts and incidence rates ",
                   "is predefined and cannot be modified."))
  }
  
  # Check for missing values in 'time'
  if (!is.null(time)) {
    .check_na(time, data, error = TRUE)
  }
  
  # Check for missing values in 'area' if specified
  if (!is.null(area)) {
    .check_na(area, data, error = TRUE)
  }
  
  # Check requriements and missing for 'pop' if specified
  if(any(type=="inc")){      
    if (!is.null(pop)) {
      if (is.null(data[[pop]])) {
        stop("No column of the data matches the 'pop' argument")
      }else{
        .check_na(pop, data)
      }
    }else{
      stop("'pop' required if type = 'inc'")
    }
    # Just fill for cov and counts since it is not used
  }else if (all(type %in% c("cov", "counts")) & is.null(pop)) {  
    pop <- "pop"
    data$pop <- rep(NA, length(data[[var[1]]]))
  }
  
  # palette check
  if(length(palette) != 2){
    stop("'palette' must be of length 2")
  }
  
  # align check
  if(!align %in% c("min", "mean", "median")){
    stop("'align' must be equal to either 'min', 'mean', or 'median'")
  }
  
  # Length var_label and ylab 
  if(!is.null(var_label) & (length(var_label) != 2 | !inherits(var_label, "character"))){
    stop("'var_label' must be a character vector of length 2")
  }else if(!is.null(ylab) & (length(ylab) != 2 | !inherits(ylab, "character"))){
    stop("'ylab' must be a character vector of length 2")
  }
  
  # Option A: single area, no aggregation ----
  # 'area' is not specified and 'aggregation' is not required
  # In this option data should be a single time series 
  if (is.null(area) && is.null(aggregate_space) && is.null(aggregate_time)) {
    
    # Check for duplicated dates and consecutive time points
    .check_consecutive(data, time)
    
    # Prepare data for plotting - 1
    data1 <- data |>
      dplyr::select(
        dplyr::all_of(c(time, var[1], pop))) |>
      rlang::set_names(c("time", "cases1", "pop1"))  |>
      dplyr::mutate(time = as.Date(time),
                    inc1 = (.data$cases1 / .data$pop1) * pt)
    if(type[1] == "cov"){
      data1$var1 <- data1$cases1
    }
    
    # Prepare data for plotting - 2
    data2 <- data |>
      dplyr::select(
        dplyr::all_of(c(time, var[2], pop))) |>
      rlang::set_names(c("time", "cases2", "pop2"))  |>
      dplyr::mutate(time = as.Date(time),
                    inc2 = (.data$cases2 / .data$pop2) * pt)
    if(type[2] == "cov"){
      data2$var2 <- data2$cases2
    }
    
    # Merge
    data <- dplyr::full_join(data1, data2, by = "time")
    
  }
  
  # Option B: multiple areas, no aggregation ----
  # 'area' is specified and 'aggregation' is not required (Multiple time series)
  else if (!is.null(area) && is.null(aggregate_space) && is.null(aggregate_time)) {
    
    # Check for duplicated dates and consecutive time points
    .check_consecutive(data, time, area)
    
    # Warning too many areas
    if (length(unique(data[[area]])) > 15) {
      warning(paste("More than 15 time series detected.",
                    "Try 'aggregate_space'?"
      ))
    }
    
    # Prepare data for plotting - 1
    data1 <- data |>
      dplyr::select(
        dplyr::all_of(c(time, var[1], pop, area))) |>
      rlang::set_names(c("time", "cases1", "pop1", "area"))  |>
      dplyr::mutate(time = as.Date(time),
                    area = as.character(area),
                    inc1 = (.data$cases1 / .data$pop1) * pt)
    if(type[1] == "cov"){
      data1$var1 <- data1$cases1
    }
    
    # Prepare data for plotting - 2
    data2 <- data |>
      dplyr::select(
        dplyr::all_of(c(time, var[2], pop, area))) |>
      rlang::set_names(c("time", "cases2", "pop2", "area"))  |>
      dplyr::mutate(time = as.Date(time),
                    area = as.character(area),
                    inc2 = (.data$cases2 / .data$pop2) * pt)
    if(type[2] == "cov"){
      data2$var2 <- data2$cases2
    }    
    
    # Merge
    data <- dplyr::full_join(data1, data2, by = c("time", "area"))
    
  }
  
  # Option C: aggregation ----
  # aggregation over space or time is required 
  else if (!is.null(aggregate_space) || !is.null(aggregate_time)) {
    
    # Check for duplicated dates and consecutive time points
    .check_consecutive(data, time, area)
    
    ## Option C.1: covariate ----
    if (type[1]=="cov") {
      
      # Aggregate
      data1 <- aggregate_cov(data, # 1st var
                             var = var[1],
                             time = time,
                             area = area,
                             aggregate_time = aggregate_time,
                             aggregate_space = aggregate_space, 
                             aggregate_space_fun = aggregate_space_fun,
                             aggregate_time_fun = aggregate_time_fun)
      names(data1)[names(data1)=="var"] <- "var1"
    }
    
    if (type[2]=="cov") {
      
      # Aggregate
      data2 <- aggregate_cov(data, # 2nd var
                             var = var[2],
                             time = time,
                             area = area,
                             aggregate_time = aggregate_time,
                             aggregate_space = aggregate_space, 
                             aggregate_space_fun = aggregate_space_fun,
                             aggregate_time_fun = aggregate_time_fun)
      names(data2)[names(data2)=="var"] <- "var2"
    }
    
    # Option C.2: Counts or inc ----
    if (type[1] %in% c("counts","inc")) {
      
      # Aggregate
      data1 <- aggregate_cases(data, # 1st var
                               cases = var[1],
                               pop = pop,
                               time = time,
                               area = area,
                               aggregate_time = aggregate_time,
                               aggregate_space = aggregate_space,
                               pt = pt)
      names(data1)[names(data1)=="cases"] <- "cases1"
      names(data1)[names(data1)=="pop"] <- "pop1"
      names(data1)[names(data1)=="inc"] <- "inc1"
    }
    
    if (type[2] %in% c("counts","inc")) {
      
      # Aggregate
      data2 <- aggregate_cases(data, # 2nd var
                               cases = var[2],
                               pop = pop,
                               time = time,
                               area = area,
                               aggregate_time = aggregate_time,
                               aggregate_space = aggregate_space,
                               pt = pt)
      names(data2)[names(data2)=="cases"] <- "cases2"
      names(data2)[names(data2)=="pop"] <- "pop2"
      names(data2)[names(data2)=="inc"] <- "inc2"
    }
    
    # Merge
    data <- dplyr::full_join(data1, data2, by = c("time", "area"))
  }
  
  
  # Plotting ----
  
  # Prepare plotting variables 
  if (type[1] == "inc"){ # 1st var
    data$plot_var1 <- data$inc1
  }else if(type[1] == "counts"){
    data$plot_var1 <- data$cases1
  }else if (type[1] == "cov") {
    data$plot_var1 <- data$var1
  }
  if (type[2] == "inc"){ # 2nd var
    data$plot_var2 <- data$inc2
  }else if(type[2] == "counts"){
    data$plot_var2 <- data$cases2
  }else if (type[2] == "cov") {
    data$plot_var2 <- data$var2
  }
  
  # Temporal x axis
  if (!is.null(aggregate_time)) {
    if (aggregate_time == "week") {
      data$time <- as.Date(paste(data$time, "1", sep = "-"), format = "%Y-%W-%u")
    } else if (aggregate_time == "month") {
      data$time <- as.Date(paste(data$time, "01", sep = "-"), format = "%Y-%m-%d")
    } else if (aggregate_time == "year") {
      data$time <- as.Date(paste(data$time, "01", "01", sep = "-"), format = "%Y-%m-%d")
    }
  }
  
  # Default axis and legend labels
  if(!is.null(var_label)){
    legend_label <- var_label
  }else{
    legend_label <- c("", "")
    legend_label[1] <- dplyr::case_when(
      type[1] == "cov" ~ var[1],
      type[1] == "counts" ~ "Case counts",
      type[1] == "inc" ~ "Incidence"
    )
    legend_label[2] <- dplyr::case_when(
      type[2] == "cov" ~ var[2],
      type[2] == "counts" ~ "Case counts",
      type[2] == "inc" ~ "Incidence"
    )
  }
  
  if(is.null(xlab)){xlab <- "Time"}
  
  if(is.null(ylab)){ 
    if(!is.null(var_label)){
      ylab <- c(var_label[1], var_label[2])
    }else{
      
      var_label <- var
      if(type[1] == "cov"){ # 1st axis
        ylab <- var[1]
      }else if(type[1] == "counts"){
        ylab <- "Case counts"
      } else if(type[1] == "inc"){
        if(is.null(aggregate_time) & !is.null(area)){
          time_interval <- .get_time_interval(data = data,
                                              time = "time",
                                              area = "area")
        } else if(is.null(aggregate_time) & is.null(area)){
          time_interval <- .get_time_interval(data = data,
                                              time = "time")
        }else {
          time_interval <- aggregate_time
        }
        ylab <- paste0("Incidence (", format(pt, big.mark = ",", scientific = FALSE),
                       " person-", time_interval, ")")
      }
      
      if(type[2] == "cov"){ # 2nd axis
        ylab <- c(ylab, var[2])
      }else if(type[2] == "counts"){
        ylab <- c(ylab, "Case counts")
      } else if(type[2] == "inc"){
        if(is.null(aggregate_time) & !is.null(area)){
          time_interval <- .get_time_interval(data = data,
                                              time = "time",
                                              area = "area")
        } else if(is.null(aggregate_time) & is.null(area)){
          time_interval <- .get_time_interval(data = data,
                                              time = "time")
        }else {
          time_interval <- aggregate_time
        }
        ylab <- c(ylab,
                  paste0("Incidence (", format(pt, big.mark = ",", scientific = FALSE),
                         " person-", time_interval, ")"))
      }
    }
  }
  
  # Base graph
  out <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = .data$plot_var1, 
                                    color = legend_label[1]),
                       alpha = alpha) +
    ggplot2::theme_bw() + 
    ggplot2::ylab(ylab[1]) + ggplot2::xlab(xlab) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust=0.5),
      legend.position = "bottom")
  
  # 2nd axis 
  scale <- diff(range(data$plot_var1, na.rm = TRUE)) / diff(range(data$plot_var2, na.rm = TRUE))
  if(align == "min"){
    shift <- min(data$plot_var1, na.rm = TRUE) - min(data$plot_var2, na.rm = TRUE) * scale
  }
  else if(align == "mean"){
    shift <- mean(data$plot_var1, na.rm = TRUE) - mean(data$plot_var2, na.rm = TRUE) * scale
  }
  else if(align == "median"){
    shift <- stats::median(data$plot_var1, na.rm = TRUE) - stats::median(data$plot_var2, na.rm = TRUE) * scale
  }
  out <- out +
    ggplot2::geom_line(aes(x = time,
                           y = .data$plot_var2 * scale + shift, 
                           color = legend_label[2]),
                       alpha = alpha) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(
      ~ (. - shift) / scale, name=ylab[2]))

  
  # Colors and legend
  col_vect <- stats::setNames(c(palette[1], palette[2]), c(legend_label[1], legend_label[2])) 
  out <- out +
    ggplot2::scale_color_manual(values = col_vect,
                                breaks = names(col_vect)) +
    ggplot2::labs(col = legend)
  
  # Facet
  if(!is.null(area) | !is.null(aggregate_space)){
    if(isTRUE(free_y_scale)){
      out <- out +
        ggplot2::facet_wrap(~ area, scales = "free_y")
    }else{
      out <- out +
        ggplot2::facet_wrap(~ area)
    }
  }
  
  # Add title if not NULL
  if(!is.null(title)){
    out <- out + ggplot2::ggtitle(title)
  }
  
  # Customize time x axis labels for long time series 
  nyears <- length(unique(format(as.Date(data$time), "%Y"))) # number of years
  
  # Adjust breaks dynamically: fewer labels for larger datasets
  if (nyears > 2){
    if (!is.null(area)){
      break_interval <- ifelse(nyears > 50, "5 years",
                               ifelse(nyears > 10, "2 years",
                                      "1 year"))
      
    }else{
      break_interval <- ifelse(nyears > 100, "5 years",
                               ifelse(nyears > 50, "2 years",
                                      "1 year"))
    }
    out<- out +
      ggplot2::scale_x_date(date_breaks = break_interval,
                            date_labels = "%Y",
                            expand = expansion(mult = 0.02))   
  }
  
  # return the final plot
  return(out)
}
