#' Choropleth map
#'
#' @import ggplot2
#' @import dplyr
#' @import grDevices
#'
#' @description Plots a choropleth map of covariates, case counts, or incidence 
#' rates.
#' 
#' @param data Data frame containing equally spaced (daily, weekly, monthly) 
#' covariate or case observations for one or multiple locations.
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
#' @param map Name of the sf object corresponding to the spatial unit 
#' specified in 'area'.
#' @param map_area Name of the variable that identifies the different locations
#' (e.g., areal units) in the map object. If not specified, it assumes the same
#' name as in `area`.
#' @param aggregate_time Temporal scale for visualization and aggregation. Options 
#' include "all" (across all time points) and "year" (default).
#' @param by_year Deprecated. Use 'aggregate_time' instead.
#' @param aggregate_time_fun Character indicating the function to be used
#' in the aggregation over time for `type="cov"`. Options are "mean" (default),
#' "median", "sum". For case counts and incidence, "sum" is always applied.
#' @param transform Character, defaults to "identity" (i.e., no transformation). 
#' Transforms the color ramp for better visualization. Useful options include 
#' "log10p1" `log10(x+1)` for case counts and incidence with 0s, or 
#' any of the in-built ggplot2 options such as  "log10" `log10(x)`, "log1p" `log(x+1)`, 
#' and "sqrt" `sqrt(x)` (check all possible options using `?scale_y_continuous`).
#' @param title Optional title of the plot. 
#' @param var_label Character with a custom name for the case or covariate variable.
#' @param palette GHR, RColorBrewer or colorspace palette. Use "-" before the palette 
#' name (e.g., "-Reds") to reverse it.
#' @param centering Numerical or "median", defaults to NULL. If set, 
#' it centers the palette on that value. 
#' @param bins Number of bins for categorization of numerical variables. 
#' Defaults to NULL (no binning).
#' @param bins_method Method to compute the bins, only used when `bins` is not NULL.
#' Possible values are "quantile" (default) and "equal".
#' @param bins_label Optional labels for the bins. They must have the same length 
#' as the number of bins. Defaults to NULL (default interval labels).
#' @param ... Additional aesthetics to be passed to geom_sf. Possible values 
#' include `colour` (e.g., `colour="black"`), linewidth (e.g., `linewidth=0.1`),
#' linetype (e.g., `linetype=2`), and alpha (e.g., `alpha=0.8`).
#' @return A ggplot2 choropleth map.
#' @export
#'
#'
#' @examples
#' # Load data
#' library("sf")
#' data("dengue_MS")
#' data("map_MS")
#' 
#' # Temporal average of a covariate
#' plot_map(data = dengue_MS, 
#'          var = "tmin",  
#'          time = "date",       
#'          type = "cov",
#'          area = "micro_code",  
#'          map = map_MS,         
#'          map_area = "code",   
#'          aggregate_time = "all",
#'          aggregate_time_fun = "mean",         
#'          palette ="Reds",
#'          var_label= "Min Temp.")
#' 
#' # Categorical covariate
#' plot_map(data = dengue_MS, 
#'          var = "biome_name",        
#'          time = "date",      
#'          area = "micro_code", 
#'          aggregate_time = "all",
#'          map = map_MS,       
#'          map_area = "code",  
#'          palette ="Viridis",
#'          var_label= "Biome")
#' 
#' # Case counts by year (log)
#' dengue_MS |>
#'   plot_map(var = "dengue_cases",    
#'            time = "date",    
#'            type = "counts",
#'            area = "micro_code", 
#'            pop = "population",
#'            map = map_MS,   
#'            map_area = "code",
#'            palette = "Reds",
#'            transform = "log10p1")
#' 
#' # Case incidence by year, binned
#' plot_map(dengue_MS,
#'          var = "dengue_cases", 
#'          type = "inc",
#'          time = "date",
#'          area = "micro_code",
#'          pop = "population",
#'          pt = 1000,
#'          map = map_MS, 
#'          map_area = "code",
#'          bins = 5,   
#'          palette = "Viridis")

plot_map <- function(data,
                     var,
                     time,
                     type = "cov",
                     pop = NULL,
                     pt = 100000,
                     area = NULL,
                     map = NULL,
                     map_area = NULL,
                     by_year = NULL, 
                     aggregate_time = "year",
                     aggregate_time_fun = "mean",
                     transform = "identity",
                     title = NULL,
                     var_label = NULL,
                     palette = NULL,
                     centering = NULL,
                     bins = NULL,
                     bins_method = "quantile",
                     bins_label = NULL,
                     ...){
  
  # Data checks ----
  
  # Deprecated arguments
  if (!missing(by_year)) {
   stop(paste0("The argument 'by_year' has been deprecated. Please use ",
               "'aggregate_time' instead.")) 
  }
  
  # Check data exists and is a data.frame
  if (missing(data)) {
    stop("Error: Missing required argument 'data'")
  } else if (!is.data.frame(data)) {
    stop("'data' should be a data.frame")
  }
  
  # Check if 'var' exists in data
  if (missing(var)) {
    stop("Error: Missing required argument 'var'")
  } else if (!is.null(var) && is.null(data[[var]])) {
    stop("No column of the data matches the 'var' argument")
  } else {
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
  
  # Check that 'aggregate_time is 'all' or 'year'
  if (!aggregate_time %in% c("all", "year")) {
    stop("aggregate_time can be 'all' or 'year'")
  }
  
  # Check that 'aggregate_time_fun is one of the following functions
  # (sum , mean , median) if specified.
  if (!aggregate_time_fun %in% c(
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
  
  # Check data exists and is a data.frame
  if (missing(map)) {
    stop("Missing required argument 'map'")
  } 
  if (is.null(map_area)) {map_area <- area}
  
  # Check bins 
  if(!is.null(bins)){
    if(abs(bins - round(bins)) > 1e-10){
      stop("bins must be an integer.")
    }else if(!bins_method %in% c("quantile", "equal")){
      stop("bins_method must be 'quantile' or 'equal'.")
    }else if(!is.null(bins_label) && length(bins_label) != bins){
      stop("The length of bins_label must be equal to the number of bins.")
    }
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
  
  # Connect data and sf ----
  # Check for mismatches in keys before merging
  # Ensure the keys are character vectors and trim whitespace
  map_keys <- trimws(as.character(unique(map[[map_area]])))
  data_keys <- trimws(as.character(unique(data[[area]])))
  
  # Check for mismatches after normalization
  missing_in_data <- setdiff(map_keys, data_keys)
  missing_in_map <- setdiff(data_keys, map_keys)
  
  if (length(missing_in_data) > 0 || length(missing_in_map) > 0) {
    if (length(missing_in_data) > 0) {
      warn1 <- paste0("Values in 'map' not found in 'data': ",
                      paste(missing_in_data, collapse = ", "), ". ")
    }else{warn1 <- ""}
    if (length(missing_in_map) > 0) {
      warn2 <- paste0("Values in 'data' not found in 'map': ",
                      paste(missing_in_map, collapse = ", "), ". ")
    }else{warn2 <- ""}
    warning("Some IDs do not match between map and data. ", warn1, warn2)
  }
  
  # Covariate ----
  if (type=="cov") {
    
    # Check that var is numeric or a factor
    if (!class(data[[var]]) %in% c("integer", "numeric",
                                   "factor", "character")) {
      stop("'var' should be numeric or a factor.")
    }
    
    ## Factor ----
    if (class(data[[var]]) %in% c("factor", "character")) {
      
      data[["plot_var"]] <- as.factor(data[[var]])
      data_unique <- data 
      data_unique$area <- data[[area]]
      
      # Check the covariate is time-invariant during all period
      if(aggregate_time == "all"){
        
        data_distinct <- data_unique |> 
          dplyr::group_by(area)  |>
          dplyr::summarise(n_unique = n_distinct(plot_var), .groups="keep")  |> 
          dplyr::ungroup()
        
        if(any(data_distinct$n_unique > 1)){
          stop(paste0("plot_map does not accept time-varying categorical covariates. ",
                      "You may want to try setting aggregate_time='year'"))
        }else{
          
          # Proceed if so
          data_unique <- data_unique |> 
            dplyr::group_by(area)  |>
            dplyr::filter(!duplicated(plot_var)) |> 
            dplyr::ungroup()
          
          # Add geometries
          map_data <- merge(map, data_unique, 
                            by.x = map_area, by.y = area,
                            all.x = TRUE)
        }
        
        # Check the covariate is time-invariant within years
      }else if(aggregate_time == "year"){
        
        data_unique$year <- as.numeric(format(data[[time]], "%Y"))
        data_distinct <- data_unique |> 
          dplyr::group_by(area, .data$year)  |>
          dplyr::summarise(n_unique = n_distinct(plot_var), .groups="keep")  |> 
          dplyr::ungroup()
        
        if(any(data_distinct$n_unique > 1)){
          stop(paste0("plot_map does not accept time-varying categorical covariates."))
          
        }else{
          
          # Proceed if so
          data_unique <- data_unique |> 
            dplyr::group_by(area, .data$year)  |>
            dplyr::filter(!duplicated(plot_var)) |> 
            dplyr::ungroup()
          
          # Add geometries with yearly expansion
          map_expanded <- map |>
            dplyr::rowwise() |>
            dplyr::mutate(year = list(unique(data_unique$year))) |> 
            tidyr::unnest(year)  
          map_data <- merge(
            map_expanded, data_unique,
            by.x = c(map_area, "year"), by.y = c("area", "year"), 
            all.x = TRUE)
          
        }
      }
    }
    
    # Numeric ----
    else if(is.numeric(data[[var]])){
      
      # If aggregate_time == "year", add a 'year' column to the data
      if (aggregate_time == "year") {
        data_agg <- data |>
          aggregate_cov(var = var,
                        time = time,
                        area = area,
                        aggregate_time = "year",
                        aggregate_time_fun = aggregate_time_fun)
      } else {
        data_agg <- data |>
          dplyr::group_by(!!rlang::sym(area)) |>
          dplyr::summarise(var = match.fun(aggregate_time_fun)(!!rlang::sym(var), na.rm = TRUE),
                           .groups = "drop")
      }
      
      # Rename for plotting
      data_agg$plot_var <- data_agg$var
      
      # Expand the map if aggregate_time == "year"
      if (aggregate_time == "year") {
        
        # Extract the unique years from the aggregated data
        all_years <- unique (data_agg$time)
        
        # Create all combinations of map rows and the years we have in data
        map_expanded <- map |>
          dplyr::rowwise() |>
          dplyr::mutate(year = list(all_years)) |>   # Attach the list of years to each row
          tidyr::unnest(year)  
        
        # Merge expanded map with aggregated data
        map_data <- merge(
          map_expanded,
          data_agg,
          by.x = c(map_area, "year"), by.y = c("area", "time"), 
          all.x = TRUE
        )
        
      } else {
        
        # If aggregate_time == "all", just merge by area
        map_data <- merge(
          map,
          data_agg,
          by.x = map_area,
          by.y = area,
          all.x = TRUE
        )
      }  
    }
  }
  
  # Counts or incidence ----
  else if (type %in% c("counts","inc")) {
    
    # Check that var is numeric
    if (!is.numeric(data[[var]])) {
      stop("'var' should be numeric")
    }
    
    # If by_year = TRUE, add a 'year' column to the data
    if (aggregate_time=="year") {
      
      data_agg <- data |>
        aggregate_cases(
          cases = var,
          pop = pop,
          time = time,
          area = area,
          pt = pt,
          aggregate_time = "year")
      
    } else {
      # Aggregate cases and pop to year within each area
      data_agg <- data |>
        dplyr::select( time = {{ time }},
                       area = {{area}},
                       cases = {{ var }},
                       pop = {{ pop }}) |>
        dplyr::group_by(area, time) |>
        dplyr::summarise(cases = sum(.data$cases, na.rm = TRUE),
                         pop = mean(pop, na.rm = TRUE),
                         .groups = "drop")
      
      # If aggregate_time=="all", just merge by area
      # Aggregate the cases and pop across all years within each area
      data_agg <- data_agg |>
        dplyr::group_by(area) |>
        dplyr::summarise(cases = sum(.data$cases, na.rm = TRUE),
                         pop = mean(pop, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::mutate (inc = (.data$cases/pop)*pt)
    }
    
    # Rename for plotting
    if (type == "inc") {
      data_agg$plot_var <- (data_agg$cases / data_agg$pop) * pt
    } else if (type == "counts") {
      data_agg$plot_var <- data_agg$cases
    }
    
    # Expand the map if aggregate_time=="year"
    if (aggregate_time=="year") {
      # Extract the unique years from the aggregated data
      all_years <- unique (data_agg$time)
      
      # Create all combinations of map rows and the years we have in data
      # NOTE: This effectively replicates each row of 'map' for every possible year
      map_expanded <- map |>
        dplyr::rowwise() |>
        dplyr::mutate(year = list(all_years)) |>   # Attach the list of years to each row
        tidyr::unnest(year)  
      
      # Merge expanded map with aggregated data
      map_data <- merge(
        map_expanded,
        data_agg,
        by.x = c(map_area, "year"), by.y = c("area", "time"), 
        all.x = TRUE
      )
      
    } else {
      
      # Merge aggregated data with map
      map_data <- merge(map, data_agg, by.x = map_area, by.y = "area", all.x = TRUE)
    }  
  }
  
  # Plotting ----
  
  # Default palettes
  if(is.null(palette)){
    if (type == "cov" & is.factor(map_data[["plot_var"]])){
      palette <- "IDE2"
    }else if(type == "cov"){
      palette <- "IDE1"
    }else{
      palette <- "Purp"
    }
  }
  
  # Bin config
  if (!is.null(bins) & !is.factor(map_data[["plot_var"]])) {
    if(bins_method == "quantile"){
      if(is.null(bins_label)){
        map_data <- map_data |>
          dplyr::mutate(plot_var = ggplot2::cut_number(plot_var, n = bins))
      }else{
        map_data <- map_data |>
          dplyr::mutate(plot_var = ggplot2::cut_number(plot_var, n = bins, 
                                                       labels = bins_label))
      }
    }else if(bins_method == "equal"){
      if(is.null(bins_label)){
        map_data <- map_data |>
          dplyr::mutate(plot_var = ggplot2::cut_interval(plot_var, n = bins))
      }else{
        map_data <- map_data |>
          dplyr::mutate(plot_var = ggplot2::cut_interval(plot_var, n = bins, 
                                                         labels = bins_label))
      }
    }
    
    # Centering config
  } else if (!is.null(centering) & !is.factor(map_data[["plot_var"]])) {
    
    # Compute stats for centering
    limit <- map_data |>
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
  }
  
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
  
  # Base plot
  out <- ggplot2::ggplot(map_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = plot_var), ...) + 
    ggplot2::labs(fill = legend) +
    ggplot2::theme_void() +
    theme(plot.margin = unit(rep(0.1, 4), "cm")) +
    ggplot2::theme(legend.position = "right")+ 
    ggplot2::theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(fill = legend) 
  
  # Add title if not NULL
  if(!is.null(title)){
    out <- out + ggplot2::ggtitle(title)
  }
  
  # Customize color gradient 
  if(is.factor(map_data[["plot_var"]])){ # Categorical variable and bins
    
    level_names <- unique(map_data$plot_var[!is.na(map_data$plot_var)])
    my_palette <- GHR_palette(palette, length(level_names))(n = length(level_names))
    out <- out + 
      ggplot2::scale_fill_manual(values = my_palette)
    
  } else if(!is.null(centering)) { # Centering
    
    # normalized the selected midpoint on a 0-1 scale 
    my_palette <- GHR_palette(palette)(n = 25) # Odd number, 13 is the centre
    rescaled_center <- (centering - limit[["min"]])/(limit[["max"]] - limit[["min"]])
    values_pos <- c(seq(0, rescaled_center, length.out = 13)[1:12],
                    rescaled_center,
                    seq(rescaled_center, 1, length.out = 13)[2:13])
    out <- out +
      ggplot2::scale_fill_gradientn(
        colors = my_palette,
        values = values_pos) +
      ggplot2::theme(legend.key.height = ggplot2::unit(0.9, "cm"))
    
  }else if(is.null(centering)){ # All other cases
    
    my_palette <- GHR_palette(palette)(n = 30)
    
    # Apply transformation and palette
    if(transform == "log10p1"){
      out <- out + ggplot2::scale_fill_gradientn(colors = my_palette,
                                                 transform = .log10p1_trans,
                                                 breaks = .log10_breaks_like(map_data$plot_var),
                                                 labels = .log10_breaks_like(map_data$plot_var))
    }else{
      out <- out + ggplot2::scale_fill_gradientn(colors = my_palette,
                                                 transform = transform)
    }
    out <- out + ggplot2::theme(legend.key.height = ggplot2::unit(0.9, "cm"))
    
    # NA label
    if(any(is.na(map_data$plot_var))){
      
      out <- out +
        ggplot2::geom_point(data = data.frame(lab = "NA"), x = NA, y = NA,
                            fill = NA, size = 7, shape = 15, na.rm = TRUE,
                            ggplot2::aes(color = .data$lab), show.legend = TRUE) +
        
        ggplot2::scale_color_manual(values = c("NA" = "grey50"),
                                    guide = ggplot2::guide_legend(
                                      override.aes = list(fill = "grey50",
                                                          color = "grey20"))) +
        ggplot2::labs(color = "") +
        ggplot2::guides(fill = ggplot2::guide_colorbar(order = 1),
                        color = ggplot2::guide_legend(order = 2)) +
        ggplot2::theme(legend.key = ggplot2::element_blank())
    }
  }
  
  # Facet by year if needed
  if (aggregate_time=="year") {
    out <- out + facet_wrap(~ .data$year)
  }
  
  # Return the final plot
  return(out)
}
