#' Time series plot
#'
#' @description Plots time series of covariates, case counts, or incidence rates.
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
#' @param facet If TRUE a separate time series for each space unit is plotted in 
#' different facets.
#' @param highlight ID of the `area` to be highlighted. Using this option will
#' only color the selected spatial unit and set all the rest to grey.
#' @param transform Character, defaults to "identity" (i.e., no transformation). 
#' Transforms the y-axis for better visualization. Useful options include
#' "log10p1" `log10(x+1)` useful for case counts and incidence with 0s, or 
#' any of the in-built ggplot2 options such as  "log10" `log10(x)`, "log1p" `log(x+1)`, 
#' and "sqrt" `sqrt(x)` (check all possible options using `?scale_y_continuous`).
#' @param title Optional title of the plot. 
#' @param var_label Character with a custom name for the case or covariate variable.
#' @param legend Character with a custom name for the legend.
#' @param ylab Label for the y-axis.
#' @param xlab Label for the x-axis.
#' @param free_y_scale Logical, default FALSE. Allows different scales in the 
#' y_axis when facets are used.
#' @param palette GHR, RColorBrewer or colorspace palette (e.g. "Purp").
#' Single R colors in `colors()` or hex codes can be used for single time series 
#' or facets. Use "-" before the palette name (e.g., "-Reds") to reverse it. 
#' Defaults to a dark green when `area` is NULL, when `facet` is TRUE or when
#' `highlight` is used (i.e. single time series), otherwise defaults to the 
#' "IDE2" palette. 
#' @return A ggplot2 time series plot.
#' @export
#'
#' @examples
#' # Load data
#' data("dengue_MS")
#' 
#' # Plotting a covariate, all areas in a single graph
#' plot_timeseries(dengue_MS,
#'                 var = "tmin",
#'                 time = "date",          
#'                 type = "cov",
#'                 area = "micro_code",   
#'                 title = "Minimun Temperature") 
#'                 
#' # Plotting a covariate with space aggregation and different facets
#' plot_timeseries(dengue_MS,
#'                 var = "tmin",
#'                 time = "date",
#'                 type = "cov",
#'                 area = "micro_code",
#'                 aggregate_space = "meso_code",
#'                 aggregate_space_fun = "mean",
#'                 facet = TRUE,           
#'                 var_label= "Minimum Temperature",
#'                 palette = "violetred")
#'                 
#' # Plotting counts, highlight a single area 
#' plot_timeseries(dengue_MS,
#'                 var = "dengue_cases", 
#'                 time = "date",         
#'                 type = "counts",
#'                 pop = "population",
#'                 area = "micro_code",  
#'                 title= "Dengue cases",
#'                 highlight = "50001")
#' 
#' # Plot disease counts (log scale) with temporal and spatial aggregation             
#' plot_timeseries(dengue_MS,
#'                 var = "dengue_cases", 
#'                 time = "date",      
#'                 type = "counts",
#'                 area = "micro_code",   
#'                 aggregate_space = "meso_code",
#'                 aggregate_time = "year",
#'                 title = "Yearly Cases",
#'                 transform = "log10") 
#'                
#' # Plot incidence for 1,000 people with a Brewer palette and log y axis
#' plot_timeseries(dengue_MS,
#'                 var = "dengue_cases",
#'                 time = "date",          
#'                 type = "inc",
#'                 pop = "population",
#'                 area = "micro_code",  
#'                 pt = 1000,
#'                 transform = "log10p1")                

plot_timeseries<- function(data,
                           var,
                           time,
                           type = "cov",
                           pop = NULL,
                           pt = 100000,
                           area = NULL,
                           aggregate_space = NULL,
                           aggregate_time = NULL,
                           aggregate_space_fun = "mean",
                           aggregate_time_fun = "mean",
                           facet = FALSE,
                           highlight = NULL,
                           transform = "identity", 
                           title = NULL,
                           var_label = NULL,
                           legend = NULL, 
                           ylab = NULL,
                           xlab = NULL,
                           free_y_scale = FALSE,
                           palette = NULL) {
  
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
  } else if(!missing(aggregate_space_fun) && type != "cov"){
    warning(paste0("'aggregate_space_fun' for case counts and incidence rates ",
                   "is predefined and cannot be modified."))
  }
  
  # Check that 'aggregate_time' is valid if specified
  if (!is.null(aggregate_time) && !(aggregate_time %in% c(
    "week", "month", "year"
  ))) {
    stop("'aggregate_time' can be 'week','month',year'")
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
  
  # Check that only one between facet and highlight is specified
  if (facet == TRUE && !is.null(highlight)) {
    stop("'facet' and 'highlight' cannot be specified together")
  }
  
  # Create legend label 
  if (is.null (legend)){
    legend<- "Area"} else {
      legend<- legend
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
  
  # Default palette
  if(is.null(palette)){
    palette <- ifelse(is.null(area)|isTRUE(facet)|!is.null(highlight),
                      "#168c81", "IDE2")
  }
  
  # Option A: single area, no aggregation ----
  # 'area' is not specified and 'aggregation' is not required
  # In this option data should be a single time series 
  if (is.null(area) && is.null(aggregate_space) && is.null(aggregate_time)) {
    
    # Check for duplicated dates and consecutive time points
    .check_consecutive(data, time)
    
    # Prepare data for plotting
    data <- data |>
      dplyr::select(
        time = {{ time }},
        cases = {{ var }},
        pop = {{ pop }}
      ) |>
      dplyr::mutate(time = as.Date(time),
                    inc = (.data$cases / pop) * pt)
    if(type == "cov"){
      data$var <- data$cases
    }
    if(isTRUE(facet)){
      warning("Multiple facets not possible for a single time series")
    }
  }
  
  # Option B multiple areas, no aggregation ----
  # 'area' is specified and 'aggregation' is not required (Multiple time series)
  else if (!is.null(area) && is.null(aggregate_space) && is.null(aggregate_time)) {
    
    # Check for duplicated dates and consecutive time points
    .check_consecutive(data, time, area)
    
    # Warning too many areas
    if (length(unique(data[[area]])) > 15) {
      warning(paste("More than 15 time series detected.",
                    "Try 'highlight' or 'aggregate_space'?"
      ))
    }
    
    # Prepare data for plotting
    data <- data |>
      dplyr::select(
        time = {{ time }},
        cases = {{ var }},
        pop = {{ pop }},
        area = {{ area }}
      ) |>
      dplyr::mutate(
        time = as.Date(time),
        area = as.character(area),
        inc = ((.data$cases / pop) * pt))
    if(type == "cov"){
      data$var <- data$cases
    }
  }
  
  # Option C multiple areas, aggregation ----
  # aggregation over space or time is required 
  else if (!is.null(aggregate_space) || !is.null(aggregate_time)) {
    
    # Check for duplicated dates and consecutive time points
    .check_consecutive(data, time, area)
    
    ## Option C.1: covariate ----
    if (type=="cov") {
      
      # Aggregate
      data <- aggregate_cov(data,
                            var = var,
                            time = time,
                            area = area,
                            aggregate_time = aggregate_time,
                            aggregate_space = aggregate_space, 
                            aggregate_space_fun = aggregate_space_fun,
                            aggregate_time_fun = aggregate_time_fun)
    }
    
    # Option C.2: Counts or inc ----
    else if (type %in% c("counts","inc")) {
      
      # Aggregate
      data <- aggregate_cases(data,
                              cases = var,
                              pop = pop,
                              time = time,
                              area = area,
                              aggregate_time = aggregate_time,
                              aggregate_space = aggregate_space,
                              pt = pt)
    }
  }
  
  # Prepare plotting variable ----
  if (type == "inc"){
    data$plot_var <- data$inc
  }else if(type == "counts"){
    data$plot_var <- data$cases
  }else if (type == "cov") {
    data$plot_var <- data$var
  }
  
  # Plotting ----
  
  # Create plotting variables
  if (!is.null(aggregate_time)) {
    if (aggregate_time == "week") {
      data$time <- as.Date(paste(data$time, "1", sep = "-"), format = "%Y-%W-%u")
    } else if (aggregate_time == "month") {
      data$time <- as.Date(paste(data$time, "01", sep = "-"), format = "%Y-%m-%d")
    } else if (aggregate_time == "year") {
      data$time <- as.Date(paste(data$time, "01", "01", sep = "-"), format = "%Y-%m-%d")
    }
  }
  
  # Default axis labels
  if(is.null(xlab)){xlab <- "Time"}
  if(is.null(ylab)){
    if(!is.null(var_label)){
      ylab <- var_label
      
    }else{
      if(type == "cov"){
        ylab <- var
        
      }else if(type == "counts"){
        ylab <- "Case counts"
        
      } else if(type == "inc"){
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
  
  # Base graph
  out <- ggplot2::ggplot(data, ggplot2::aes(x = time,
                                            y= .data$plot_var,
                                            color = area)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::ylab(ylab) + ggplot2::xlab(xlab) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust=0.5))
  
  # Add title if not NULL
  if(!is.null(title)){
    out <- out + ggplot2::ggtitle(title)
  }
  
  # Apply transformation
  if(transform == "log10p1"){
    out <- out + ggplot2::scale_y_continuous(transform = .log10p1_trans,
                                             breaks = .log10_breaks_like(data$plot_var),
                                             labels = .log10_breaks_like(data$plot_var))
  }else{
    out <- out + ggplot2::scale_y_continuous(transform = transform)
  }
  
  # Customize color for single time series plot (inc facet==TRUE and highlight)
  if (!is.null(palette)){
    my_palette <- GHR_palette(palette, 3)(3)
    single_color <- my_palette[3] 
  }
  
  # Customize plot for single time series and facet==FALSE
  if (is.null(data$area) | length(unique(data$area)) == 1) {
    
    # Message for a single time series and color ramp
    if(length(unique(my_palette))>1){
      message(paste0("A color ramp was selected for a single time series. ",
                     "Please select a single color to have more control."))
    }
    
    out$layers[[1]] <- NULL # substitute geom_line
    
    out <- out + 
      ggplot2::geom_line(color = single_color) +
      ggplot2::theme(legend.position = "none")
    
  }else if(length(unique(data$area)) >= 1 & !isTRUE(facet)) {
    
    # Customize plot for many areas - Adjust legend columns dynamically
    legend_cols <- ifelse(length(unique(data[["area"]])) > 15, 2, 1)
    out <- out +
      ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_cols)) 
  }
  
  # Customize time x axis labels for long time series 
  nyears <- length(unique(format(as.Date(data$time), "%Y"))) # number of years
  
  # Adjust breaks dynamically: fewer labels for larger datasets
  if (nyears > 2){
    if (facet == TRUE){
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
  
  # Customize plot if facet == TRUE
  if (isTRUE(facet) && (!is.null(area) || !is.null(aggregate_space) || (free_y_scale ==TRUE)))  {
    
    # Message for a single time series and color ramp
    if(length(unique(my_palette))>1){
      message(paste0("A color ramp was selected for a single time series. ",
                     "Please select a single color to have more control."))
    }
    
    out$layers[[1]] <- NULL # substitute geom_line
    out <- out +
      ggplot2::geom_line(color = single_color) +
      ggplot2::facet_wrap(~ area) +
      ggplot2::guides(color = "none")
  } 
  
  # Customize plot if highlight is specified
  if (!is.null(highlight) && (!is.null(area) || !is.null(aggregate_space))) {
    
    # Message for a single time series and color ramp
    if(length(unique(my_palette))>1){
      message(paste0("A color ramp was selected for highlight. ",
                     "Please select a single color to have more control."))
    }
    
    colors <- c(rep("grey80", 2000))
    out <- out + ggplot2::scale_color_manual(values = colors) +
      ggplot2::geom_line(
        data = out$data[out$data$area == highlight, ],
        colour = single_color
      ) + ggplot2::theme(legend.position = "none")
    
  } 
  
  # Customize colors if Multiple time series are detected and facet = FALSE
  # and no highlights has been specified
  if (facet == FALSE && is.null(highlight)){
    
    if (length(unique(data[["time"]])) != nrow(data)) {
      narea <- length(unique(out$data$area))
      my_palette <- GHR_palette(palette, narea)(narea)
      
      out <- out + ggplot2::scale_color_manual(values = my_palette,
                                               name = legend)
    }
  }
  
  # Apply free y-axis scaling if specified
  if (free_y_scale == TRUE && (!is.null(area)) && facet == TRUE) {
    out <- out + ggplot2::facet_wrap(~ area, scales = "free_y")
  }
  
  # return the final plot
  return(out)
}
