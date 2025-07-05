#' Heatmap plot
#'
#' @description Plots temporal heatmaps of covariates, case counts, or 
#' incidence rates.
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
#' (i.e., areal units) for which a time series is available.
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
#' Transforms the color ramp for better visualization. Useful options include 
#' "log10p1" `log10(x+1)` useful for case counts and incidence with 0s, or 
#' any of the in-built ggplot2 options such as  "log10" `log10(x)`, "log1p" `log(x+1)`, 
#' and "sqrt" `sqrt(x)` (check all possible options using `?scale_y_continuous`).
#' @param title Optional title of the plot. 
#' @param var_label Character with a custom name for the case or covariate variable.
#' @param ylab Label for the y-axis.
#' @param xlab Label for the x-axis.
#' @param palette GHR, RColorBrewer or colorspace palette. Use "-" before the palette 
#' name (e.g., "-Reds") to reverse it.
#' @param centering Numerical or "median", defaults to NULL. If set, 
#' it centers the palette on that value. 
#' @return A ggplot2 heatmap plot.
#' @export
#'
#' @examples
#' # Load data
#' data("dengue_MS")
#' 
#' # Covariate heatmap with space aggregation
#' plot_heatmap(dengue_MS,
#'              var = "tmin",
#'              time = "date",
#'              var_label = "Minimum\ntemp.",
#'              type = "cov",
#'              area = "micro_code",
#'              aggregate_space = "meso_code",  
#'              palette = "Blue-Red")
#'
#' # Case count heatmap with log scale
#' plot_heatmap(dengue_MS,
#'              var = "dengue_cases", 
#'              time = "date",  
#'              type = "counts",
#'              area = "micro_code",  
#'              palette = "Reds", 
#'              title = "Dengue counts", 
#'              var_label = "Dengue \ncounts",
#'              transform = "log10p1")  
#'              
#' # Case incidence (for 1,000 persons) heatmap with space aggregation
#' plot_heatmap(dengue_MS,
#'              var = "dengue_cases", 
#'              time = "date",          
#'              type = "inc",
#'              pop = "population",
#'              pt = 1000,
#'              area = "micro_code", 
#'              aggregate_space = "meso_code", 
#'              palette = "Purp")            

plot_heatmap <- function(data,
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
                         palette = NULL, 
                         centering = NULL) {
  
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
    
    # Aggregate the data if requested 
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
  if (type %in% c("counts","inc")) {
    
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
  
  # Default palette
  if(is.null(palette)){
    if(type == "cov"){
      palette <- "IDE1"
    }else{
      palette <- "Purp"
    }
  }
  
  # Customize dates for heatmap
  data <- data |> dplyr::mutate(
    year = as.integer(substr(time, 1, 4)),
    time = as.integer(substr(time, 6, 7))) |>
    dplyr::filter(time > 0)
  
  # Customize legend label 
  if (type == "cov"){
    if (is.null (var_label)){
      legend<- var
      } else {
        legend<- var_label
      }
  } else if (type == "inc") {
    if (is.null (var_label)){
      
      legend <- paste0("Incidence\n(per ",
                       format(pt, big.mark = ",", scientific = FALSE), ")")
      } else {
        legend<- var_label
      }
  } else if (type == "counts") {
    if (is.null (var_label)){
      legend<- "Case\ncounts"
      } else {
        legend<- var_label
      }
  }
  
  # Default axis labels
  if(is.null(xlab)){xlab <- .firstup(aggregate_time)}
  if(is.null(ylab)){ylab <- "Year"}
  
  # Define ggplot variables and common layout
  out <- ggplot2::ggplot(data, ggplot2::aes(
    x = as.factor(.data$time),
    y = .data$year,
    fill = .data$plot_var)) +
    ggplot2::geom_raster(alpha = 0.9) +
    ggplot2::theme_bw() +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab) +
    ggplot2::labs(fill = legend) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(color = "black"),
      axis.text.x = ggplot2::element_text(angle = 90,
                                          hjust = 1, vjust=0.5),
      axis.text.y = ggplot2::element_text(angle = 0,
                                          hjust = 1)) +
    ggplot2::theme(legend.key.height = ggplot2::unit(0.9, "cm"))
  
  # Add title if not NULL
  if(!is.null(title)){
    out <- out + ggplot2::ggtitle(title)
  }
  
  # Handle centering if not null
  if(!is.null(centering)) {  
    
    # Compute stats for centering
    limit <- data |>
      dplyr::summarise(
        min = min(plot_var, na.rm = TRUE),
        med = stats::median(plot_var, na.rm = TRUE),
        max = max(plot_var, na.rm = TRUE)
      ) |>
      unlist()
    
    if (centering == "median") {
      centering <- limit[["med"]]
    }else{
      if(centering < limit[["min"]] | centering > limit[["max"]])
        stop(paste0("The centering value must be within the range of the data ",
                    "after all transformations"))
    }
    
    # normalized the selected midpoint on a 0-1 scale 
    my_palette <- GHR_palette(palette)(n = 25) # Odd number, 13 is the centre
    rescaled_center <- (centering - limit[["min"]])/(limit[["max"]] - limit[["min"]])
    values_pos <- c(seq(0, rescaled_center, length.out = 13)[1:12],
                    rescaled_center,
                    seq(rescaled_center, 1, length.out = 13)[2:13])
    out <- out +
      ggplot2::scale_fill_gradientn(
        colors = my_palette,
        values = values_pos  
      ) 
    
  }else if(is.null(centering)){
    
    my_palette <- GHR_palette(palette)(n = 30)
    
    # Apply transformation and palette
    if(transform == "log10p1"){
      out <- out + ggplot2::scale_fill_gradientn(colors = my_palette,
                                                 transform = .log10p1_trans,
                                                 breaks = .log10_breaks_like(data$plot_var),
                                                 labels = .log10_breaks_like(data$plot_var))
    }else{
      out <- out + ggplot2::scale_fill_gradientn(colors = my_palette,
                                                 transform = transform)
    }
  }
  
  # NA label
  if(any(is.na(data$plot_var))){

    out <- out +
      ggplot2::geom_point(data = data.frame(lab = "NA"), x = NA, y = NA,
                          fill = NA, size = 7, shape = 15, na.rm = TRUE,
                          aes(color = .data$lab), show.legend = TRUE) +
      ggplot2::scale_color_manual(values = c("NA" = "grey50"),
                                  guide = ggplot2::guide_legend(
                                    override.aes = list(fill = "grey50",
                                                        color = "grey20"))) +
      ggplot2::labs(color = "") +
      ggplot2::guides(fill = guide_colorbar(order = 1),
                      color = ggplot2::guide_legend(order = 2)) +
      theme(legend.key = element_blank())
  }
  
  # X-axis label
  if (aggregate_time == "month"){
    out <- out +
      ggplot2::scale_x_discrete(labels = function(x) month.abb[as.numeric(x)])
  } else if (aggregate_time == "week") {
    out <-  out +
      ggplot2::scale_x_discrete(labels = function(x) 
        ifelse(as.numeric(x) %% 4 == 0,paste0("W", as.numeric(x)), ""))
  }
  
  # Y -axis label
  min_year <- min(data$year, na.rm = TRUE)
  max_year <- max(data$year, na.rm = TRUE)
  
  if (max_year - min_year > 10) {
    years <- seq(min_year, max_year, by = 2)
  } else {
    years <- seq(min_year, max_year, by = 1)
  }
  
  out <- out +
    ggplot2::scale_y_continuous(breaks = years)
  
  # If required split the plot into multiple facets for each area
  if ((!is.null(area) || !is.null(aggregate_space))) {
    out <- out + ggplot2::facet_wrap(~area)
  }
  
  # Return final plot
  return(out)
  
} 
