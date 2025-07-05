#' Seasonality plot
#'
#' @description Plots yearly time series of covariates, case counts, or incidence 
#' rates to explore seasonality patterns.
#' 
#' @param data Data frame containing equally spaced (daily, weekly, monthly) 
#' covariate or disease case observations for one or multiple locations.
#' @param var Name of the column identifying the variable to be plotted.
#' @param time Name of the variable that identifies the temporal dimension
#' of the data frame. Its values must be in date format ("yyyy-mm-dd")
#' representing the day of observation for daily data, the first day of the 
#' week for weekly, or the first day of the month for monthly observations.
#' @param type Character that specifies the type of variable in `var`. 
#' Possible values include 'cov' (covariate, default), 'counts' (case counts), 
#' and 'inc' (case incidence). If `type='inc'`, `pop` is required.
#' @param pop Character identifying the variable name for population. Only needed 
#' if `type='inc'`.
#' @param pt Scale of the person-time (default 100,000) for incidence rates.
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
#' @param transform Character, defaults to "identity" (i.e., no transformation). 
#' Transforms the y-axis for better visualization. Useful options include 
#' "log10p1" `log10(x+1)` for case counts and incidence with 0s, or 
#' any of the in-built ggplot2 options such as  "log10" `log10(x)`, "log1p" `log(x+1)`, 
#' and "sqrt" `sqrt(x)` (check all possible options using `?scale_y_continuous`).
#' @param title Optional title of the plot. 
#' @param var_label Character with a custom name for the case or covariate variable.
#' @param ylab Label for the y-axis.
#' @param xlab Label for the x-axis.
#' @param free_y_scale If TRUE, the y-axis scale is free in each facet.
#' @param palette GHR, RColorBrewer or colorspace palette. Use "-" before the 
#' palette name (e.g., "-Reds") to reverse it.
#' 
#' @return A ggplot2 seasonality plot.
#' @export
#'
#' @examples
#' # Load data
#' data("dengue_MS")
#' 
#' # Seasonality plot of a covariate with space aggregation
#' plot_seasonality(dengue_MS,
#'                  var = "tmax",
#'                  time = "date",
#'                  var_label = "Max temp.",
#'                  type = "cov",
#'                  area = "micro_code",
#'                  aggregate_space = "region_code") 
#' 
#' # Plot case counts (log scale) with space aggregation
#'  plot_seasonality(dengue_MS,
#'                   var = "dengue_cases",
#'                   time = "date",  
#'                   type = "counts",
#'                   area = "micro_code",
#'                   aggregate_space = "meso_code",
#'                   transform = "log10p1",
#'                   var_label = "Monthly Dengue Cases", 
#'                   xlab = "Month", 
#'                   ylab = "Number of cases",
#'                   free_y_scale = TRUE)
#'                   
#' # Seasonality plot of incidence
#' plot_seasonality(dengue_MS,
#'                  var = "dengue_cases",
#'                  time = "date",    
#'                  type = "inc",
#'                  pop = "population",
#'                  area = "micro_code",
#'                  pt = 1000, 
#'                  title = "Monthly Dengue Incidence",
#'                  palette = "Reds")        
#'                  
plot_seasonality <- function(data,
                             var,
                             time,
                             type = "cov",
                             pop = NULL,
                             pt = 100000,
                             area = NULL,
                             aggregate_space = NULL,
                             aggregate_time = "month",
                             aggregate_space_fun = "mean",
                             aggregate_time_fun = "mean",
                             transform = "identity",
                             title = NULL,
                             var_label = NULL,
                             ylab = NULL,
                             xlab = NULL,
                             free_y_scale = FALSE,
                             palette = "IDE1"){
  
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
  } else if (is.null(data[[var]])) {
    stop("No column of the data matches the 'var' argument")
  }else if (is.numeric(data[[var]]) == FALSE) {
    stop("'var' should be numeric")
  }else{
    .check_na(var, data)
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
  if (!type %in% c("cov","counts", "inc")) {
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
  }else if(!missing(aggregate_space_fun) && type != "cov"){
    warning(paste0("'aggregate_space_fun' for case counts and incidence rates ",
                   "is predefined and cannot be modified."))
  }
  
  # Check that 'aggregate_time' is valid if specified
  if (!is.null(aggregate_time) && !(aggregate_time %in% c(
    "week", "month"
  ))) {
    stop("'aggregate_time' can be 'week' or 'month'")
  }
  
  # Check that 'aggregate_time_fun is one of the following functions
  # (sum , mean , median) if specified.
  if (!is.null(aggregate_time) && !aggregate_time_fun %in% c(
    "sum", "mean", "median"
  )) {
    stop("aggregate_time_fun can be 'sum', 'mean', 'median'")
  }else if(!missing(aggregate_time_fun) && type != "cov"){
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
  
  # Check requirements and missings for 'pop' if specified
  if(type=="inc"){      
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
  }else if (type %in% c("cov", "counts") & is.null(pop)) {  
    pop <- "pop"
    data$pop <- rep(NA, length(data[[var]]))
  }
  
  # Check that the transform is valid
  val_trans <- c("log10p1",
                 "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", 
                 "log", "log10", "log1p", "log2", "logit", "modulus", 
                 "probability", "probit", "pseudo_log", "reciprocal", "reverse", 
                 "sqrt", "time")
  if(!transform %in% val_trans){
    stop("Invalid transform.") 
  }else if(transform == "log10p1" & type == "cov"){
    stop("Transform log10p1 is only available for case counts and incidence rates.") 
  }
  
  # Check for duplicated dates and consecutive time points
  if(is.null(area)){
    .check_consecutive(data, time)
  }else{
    .check_consecutive(data, time, area)
  }
  
  # Aggregate covariates ----
  if (type=="cov") {
    
    # Aggregate the data
    data <- aggregate_cov(data,
                          var = var,
                          time = time,
                          area = area,
                          aggregate_time = aggregate_time,
                          aggregate_space = aggregate_space,
                          aggregate_time_fun = aggregate_time_fun,
                          aggregate_space_fun = aggregate_space_fun)
    
    # Rename for plotting
    data$plot_var <- data$var
  }
  
  # Aggregate counts and incidence ----
  else if (type %in% c("counts","inc") && !is.null(var)) {
    
    # Aggregate the dataset
    data <- aggregate_cases(data,
                            cases = var,
                            pop = pop,
                            time = time,
                            area = area,
                            aggregate_time = aggregate_time,
                            aggregate_space = aggregate_space,
                            pt = pt)
    
    # Rename for plotting
    if (type == "inc") {
      data$plot_var <- (data$cases / data$pop) * pt
    } else if (type == "counts") {
      data$plot_var <- data$cases
    }
  }
  
  
  # Plotting ----
  
  # Customize dates for plot
  data <- data |> dplyr::mutate(
    year = as.integer(substr(time, 1, 4)),
    time = as.integer(substr(time, 6, 7))) |>
    dplyr::filter(time > 0)
  
  # Default axis labels
  if(is.null(xlab)){xlab <- .firstup(aggregate_time)}
  if(is.null(ylab)){
    if(!is.null(var_label)){
      ylab <- var_label
      
    }else{
      if(type == "cov"){
        ylab <- var
        
      }else if(type == "counts") {
        ylab = "Case counts"
        
      }else if(type == "inc"){
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
    }
  }
  
  # Define ggplot variables and common layout
  out <- ggplot2::ggplot(data, ggplot2::aes(
    x = as.factor(time),
    y = .data$plot_var,
    color = as.factor(.data$year),
    group = .data$year))
  
  # Apply transformation
  if(transform == "log10p1"){
    out <- out + ggplot2::scale_y_continuous(transform = .log10p1_trans,
                                             breaks = .log10_breaks_like(data$plot_var),
                                             labels = .log10_breaks_like(data$plot_var))
  }else{
    out <- out + ggplot2::scale_y_continuous(transform = transform)
  }
  
  # Customize color
  nyears <- length(unique(data$year))
  my_palette <- GHR_palette(palette, nyears)(nyears)
  
  # Adjust legend columns dynamically
  legend_cols <- ifelse(nyears > 15, 2, 1)
  
  out <- out + ggplot2::scale_color_manual(values = my_palette,
                                           name = "Year")
  
  # Define common layout
  out <- out +
    ggplot2::geom_line(alpha=0.9) +
    ggplot2::theme_bw() +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        hjust = 1, vjust=0.5), 
      legend.spacing.x = ggplot2::unit(0.2, 'cm')  
    )+
    ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_cols)) 
  
  # Add title if not NULL
  if(!is.null(title)){
    out <- out + ggplot2::ggtitle(title)
  }
  
  # Split the plot into multiple facets for each area
  if (length(unique(data[["area"]])) != 1 && free_y_scale == TRUE) {
    out <- out + ggplot2::facet_wrap(~ area, scales = "free_y") 
  } else if (length(unique(data[["area"]])) != 1) {
    out <- out + ggplot2::facet_wrap(~ area)
  }
  
  # X-axis label (Months or Weeks)
  if (aggregate_time == "month"){
    out <- out +
      ggplot2::scale_x_discrete(labels = function(x) month.abb[as.numeric(x)])
  } else if (aggregate_time == "week") {
    out <-  out + 
      ggplot2::scale_x_discrete(labels = function(x)
        ifelse(as.numeric(x) %% 4 == 0, paste0("W", as.numeric(x)), ""))
  }
  
  # return the final plot
  return(out)
}
