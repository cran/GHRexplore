#' .get_time_interval
#'
#' This function determines the time interval (e.g., daily, weekly, monthly, yearly)
#' of a dataset based on the differences between the first two time points.
#'
#' @param data an object of class 'data.frame' containing equally spaced
#'             time series data (daily, weekly, monthly data) for one or multiple spatial units.
#' @param time A string representing the name of the column in `data` that contains time information.
#'             The column should be in "yyyy-mm-dd" format.
#' @param area (Optional) A string representing the names of the columns in `data`
#'             that contains grouping information. Required if `data` contains 
#'             multiple spatial units.
#'             
#' @importFrom rlang := !! sym
#' 
#' @return A character string indicating the time interval of the dataset. Possible values:
#'         "day", "week", "month", "year", or "unknown".
#' @noRd

.get_time_interval <- function(data = NULL, time = NULL, area = NULL) {
  
  # Check if 'data' and 'time' are provided
  if (is.null(data) || is.null(time)) {
    stop("'data' and 'time' must be provided.")
  }
  
  # Input validation for 'area'
  if (!is.null(area) && !all(names(area) %in% names(data))) {
    stop("The variable specified in 'area' can't be found in 'data'.") 
  }
  
  # Check if there are no duplicated dates
  if (!is.null(area)) {
    if (nrow(unique(data[, c(area, time)])) != nrow(data[, c(area, time)])) {
      stop(paste0(
        "Duplicated time units detected. To analyze data with multiple areas",
        " please specify them using 'area' argument." 
      ))
    }
  } else { 
    if ((length(unique(data[[time]])) != nrow(data))) { 
      stop(paste0(
        "Duplicated time units detected. If the data contains multiple spatial ",
        "units then the 'area' argument is required"
      ))
    }
  }  
  
  # Sort data by grouping variables and time
  if(!is.null(area)) {
    data <- data |>
      dplyr::arrange(dplyr::across(all_of(c(area, time)))) 
    date_vector <- as.Date(data[[time]])
  } else {
    data <- data |>
      dplyr::arrange(dplyr::across(all_of(c(time)))) 
    if(is.vector(data)) {
      date_vector <- as.Date(data)
    } else {
      date_vector <- as.Date(data[[time]])
    }
  }
  
  # Check for failed date conversion
  if (any(is.na(date_vector))) {
    stop("NA detected. Ensure 'time' column is in 'yyyy-mm-dd' format.")
  }
  
  # Calculate difference between the first two dates
  date_diffs <- diff(date_vector[1:2])
  # Determine the time interval based on the difference
  if (date_diffs == 1) {
    t <- "day"
  } else if (date_diffs == 7) {
    t <- "week"
  } else if (date_diffs >= 28 && date_diffs <= 31) {
    t <- "month"
  } else if (date_diffs >= 350 && date_diffs <= 370) {
    t <- "year"
  } else {
    t <- "unknown"
  }
  # Return the identified time interval as a string
  return(t)
}

#' .check_consecutive 
#'  
#' @description
#' Verifies whether time values in a dataset are consecutive based on the 
#' detected time interval (e.g., daily, weekly, monthly, yearly). If a `area` 
#' column is specified, the check is performed within each area.
#'
#' @param data A data frame containing the dataset. Must include the time variable
#'             and optionally an area variable.
#' @param time A string representing the name of the column in `data` that contains 
#'             time values. The column should be in "yyyy-mm-dd" format or any 
#'             format recognizable by `as.Date()`.
#' @param area (Optional) A character vector representing the names of the columns
#'             in `data` that contain grouping information. If provided, the check will
#'             be performed separately within each area.
#'
#' @return A logical value: `TRUE` if all time values (within each area if 
#' `area` is specified) are consecutive, and `FALSE` otherwise.
#' @noRd

.check_consecutive <- function(data, time, area = NULL) {  
  
  # Check if 'data' and 'time' are provided
  if (missing(data) || missing(time)) {
    stop("'data' and 'time' must be provided.")
  }
  
  # Check if 'data' is a data.frame
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  
  # Check if 'time' column exists in 'data'
  if (!(time %in% names(data))) {
    stop(paste("Time column", time, "not found in 'data'."))
  }
  
  # Input validation for 'area'
  if (!is.null(area)) {
    if (!all(area %in% names(data))) {
      stop("The variable specified in 'area' can't be found in 'data'.") 
    }
  }
  
  # Check for duplicated time entries
  if (!is.null(area)) {
    duplicated_rows <- duplicated(data[, c(area, time), drop = FALSE])
    if (any(duplicated_rows)) {
      stop(paste0("Duplicated time units detected. Ensure each area has ",
                  "unique time points."))
    }
  } else { 
    if (any(duplicated(data[[time]]))) { 
      stop(paste0("Duplicated time units detected. If the data contains multiple ",
                  "spatial units, specify them using the 'area' argument."))
    }
  }  
  
  # Subset relevant columns (area and time) - ensure `time` is in Date format
  cols <- c(area, time)
  data <- data |> 
    dplyr::mutate(!!time := as.Date(.data[[time]])) |>
    dplyr::select(all_of(cols))
  
  # Determine the time interval (e.g., day, week, month, year)
  interval <- .get_time_interval(data, time = time, area = area)
  
  # Check consecutiveness within each 'area' if area is provided 
  if (!is.null(area)) {
    is_consecutive <- data |>
      dplyr::arrange(dplyr::across(all_of(c(area, time)))) |>
      dplyr::group_by(dplyr::across(all_of(area))) |>
      dplyr::mutate(date_diffs = as.numeric(difftime(.data[[time]], 
                                                     dplyr::lag(.data[[time]]), 
                                                     units = "days"))) |>
      dplyr::filter(!is.na(.data$date_diffs)) |>
      dplyr::summarize(all_consecutive = dplyr::case_when(
        interval == "day"   ~ all(.data$date_diffs == 1),
        interval == "week"  ~ all(.data$date_diffs == 7),
        interval == "month" ~ all(.data$date_diffs >= 28 & .data$date_diffs <= 31),
        interval == "year"  ~ all(.data$date_diffs >= 350 & .data$date_diffs <= 370),
        TRUE ~ FALSE
      ), .groups = "drop") |>
      dplyr::pull(.data$all_consecutive)
    
    # Combine results across all areas
    is_consecutive <- all(is_consecutive)
  } else {
    
    # Check consecutiveness when 'area' is not specified
    is_consecutive <- data |>
      dplyr::arrange(.data[[time]]) |>
      dplyr::mutate(date_diffs = as.numeric(difftime(.data[[time]], 
                                                     dplyr::lag(.data[[time]]), 
                                                     units = "days"))) |>
      dplyr::filter(!is.na(.data$date_diffs)) |>
      dplyr::summarize(all_consecutive = dplyr::case_when(
        interval == "day"   ~ all(.data$date_diffs == 1),
        interval == "week"  ~ all(.data$date_diffs == 7),
        interval == "month" ~ all(.data$date_diffs >= 28 & .data$date_diffs <= 31),
        interval == "year"  ~ all(.data$date_diffs >= 350 & .data$date_diffs <= 370),
        TRUE ~ FALSE
      )) |>
      dplyr::pull(.data$all_consecutive)
  }
  
  if(!isTRUE(is_consecutive)){
    warning(paste0("Some intervals between consecutive time points do not follow ",
                   "the detected time interval: ", interval))
  }
  
  # return the consecutivness as boolean 
  return(is_consecutive)
}

#' .check_na
#' @description This function checks for NAs in a specified column and 
#' returns a warning or error if there are any.
#' @param data A dataframe containing the dataset. 
#' @param col_name A string representing the name of the column in `data` .
#' @param error Boolean, if FALSE return warning, if TRUE an error.
#' @return A warning specifying the columns with missing values
#' 
#' @noRd
#'         

.check_na <- function(col_name, data, error = FALSE) {
  if (!is.null(col_name) && col_name %in% names(data)) {
    if (any(is.na(data[[col_name]]))) {
      if(isTRUE(error)){
        stop(paste("Missing values found in the '", col_name, "' column"))
      }else{
        warning(paste("Missing values found in the '", col_name, "' column"))
      }
    }
  }
}

#' Aggregate covariates
#' 
#' @description Aggregates a data frame containing a covariate of interest in 
#' space and/or time.
#'
#' @param data Data frame containing equally spaced
#' (daily, weekly, monthly) incident cases for one or multiple areas.
#' @param var Name of the variable that identifies the covariate.
#' @param time Name of the variable that identifies the temporal dimension. 
#' The values must be in date format ("yyyy-mm-dd")
#' representing the date of observation for daily data, the first day of the 
#' week for weekly, or the first day of the month for monthly observations.
#' @param area Name of variable that identifies the different locations
#' (i.e., areal units) for which a time series is available.
#' @param aggregate_space Name of variable used to define spatial aggregation groups.
#' @param aggregate_time Temporal scale used to perform
#' temporal aggregation. Options are: "week" (ISO 8601), "month", "year".
#' @param aggregate_space_fun Character indicating the function to be used
#' in the aggregation over space, default is "mean".
#' @param aggregate_time_fun Character indicating the function to be used
#' in the aggregation over time, default is "mean".

#' @return A data frame with the aggregated covariate.
#' @export
#'

aggregate_cov <- function(data = NULL,
                          var = NULL,
                          time = NULL,
                          area = NULL,
                          aggregate_space = NULL,
                          aggregate_time = NULL,
                          aggregate_space_fun = "mean",
                          aggregate_time_fun = "mean") {
  
  # Check data is a data.frame
  if (!is.data.frame(data)) stop("'data' should be a data.frame")
  
  # Check necessary columns and their type
  if (any(c(var, time) %in% colnames(data) == FALSE)) {
    stop("'time' and/or 'var' not found in the data")
  }
  
  if (!is.numeric(data[[var]])) stop("'var' should be numeric")
  
  # Validate aggregate_space and aggregate_time
  if (!is.null(aggregate_space) && !aggregate_space %in% colnames(data)) {
    stop("No column of the data matches the 'aggregate_space' argument")
  }
  
  if (!is.null(aggregate_time) && !aggregate_time %in% c("week", "month", "year")) {
    stop("'aggregate_time' can be 'week','month', or 'year'")
  }
  
  if (!is.null(aggregate_space) && !aggregate_space_fun %in% c(
    "sum", "mean", "median")) {
    stop("aggregate_space_fun can be 'sum', 'mean' 'median'")
  }
  
  # Check that 'aggregate_time_fun is one of the following functions
  # (sum , mean , median) if specified.
  if (!is.null(aggregate_time) && !aggregate_time_fun %in% c(
    "sum", "mean","median" )) {
    stop("aggregate_time_fun can be 'sum', 'mean', 'median'")
  }
  
  # Check that 'time' is in the correct date format and if correct
  # transform it to a Date
  if (any(is.na(as.Date(data[[time]], format = "%Y-%m-%d")))) {
    stop("'Date' should be in 'yyyy-mm-dd' format")
  } else {data[[time]] <- as.Date(data[[time]], format = "%Y-%m-%d")}
  
  
  t <- .get_time_interval(data = data,
                          time = time,
                          area = area)
  # Check time aggregation 
  scale_rank <- c("day" = 1, "week" = 2, "month" = 3, "year" = 4)
  if (!is.null(aggregate_time)) {
    # If data scale is recognized
    if (!t %in% names(scale_rank)) {
      stop(sprintf("Unrecognized current data scale '%s'.", t))
    }
    
    # If user tries to go from e.g. monthly (3) -> weekly (2), throw error
    if (scale_rank[aggregate_time] < scale_rank[t]) {
      stop(sprintf(
        "Cannot aggregate from %s data to %s data (finer resolution). 
         Please choose a coarser time scale.",
        t, aggregate_time
      ))
    }
  }
  
  # If area is missing and aggregate_space is missing, create a dummy area
  if (is.null(area) && is.null(aggregate_space)) {
    data$area <- 1
    area <- "area"
  }
  
  ## 1. Aggregate_space ----
  if(!is.null(aggregate_space)) {
    
    data <- data |>
      dplyr::select(
        time = {{ time }},
        area = {{ aggregate_space }},
        var = {{ var }}) |>
      dplyr::mutate(time = as.Date(time), area = as.character(area)) |>
      dplyr::group_by(time, area) |>
      dplyr::summarise(var = match.fun(aggregate_space_fun)(var),
                       .groups = "drop")
    
  } else { data <- data |>
    dplyr::select(
      time = {{ time }},
      area = {{ area }},
      var = {{ var }}) |>
    dplyr::mutate(time = as.Date(time), area = as.character(area))
  }
  
  
  # 2. Aggregate_time ----
  
  #  Week - special case, we need to use ISO weeks for correct aggregation
  if (!is.null(aggregate_time) && aggregate_time == "week") {
    
    data$time <- ISOweek::ISOweek(as.Date(data$time))
    data <- data |>
      dplyr::group_by(area, time) |>
      dplyr::summarise(var = match.fun(aggregate_time_fun)(var),
                       .groups = "drop")
    
    data$time <- format(ISOweek::ISOweek2date(paste0(data$time, "-1")),
                        "%Y-%W")
  }
  
  # Month
  else if (!is.null(aggregate_time) && aggregate_time == "month") {
    
    data$time <- format(as.Date(data$time), "%Y-%m")
    
    data <- data |>
      dplyr::group_by(area,time) |>
      dplyr::summarise(var = match.fun(aggregate_time_fun)(var),
                       .groups = "drop")
  }
  
  # Year
  else if (!is.null(aggregate_time) && aggregate_time == "year") {
    
    data$time <- format(as.Date(data$time), "%Y")
    
    data <- data |>
      dplyr::group_by(area,time) |>
      dplyr::summarise(var = match.fun(aggregate_time_fun)(var),
                       .groups = "drop")
  }
  
  return(data)
}

#' Aggregate cases
#' 
#' @description Aggregates a data frame containing disease cases in space and/or
#' time.
#'
#' @param data Data frame containing equally spaced
#' (daily, weekly, monthly) incident cases for one or multiple areas.
#' @param cases Name of the variable that identifies the cases.
#' @param time Name of the variable that identifies the temporal dimension. 
#' The values must be in date format ("yyyy-mm-dd")
#' representing the day of observation for daily data, the first day of the 
#' week for weekly, or the first day of the month for monthly observations.
#' @param area  Name of variable that identifies the different locations
#' (e.g., areal units) for which a time series is available.
#' @param pop Name of the variable that identifies the population.
#' @param pt Scale of the person-time (default 100,000) for incidence rates.
#' @param aggregate_space Name of variable used to define spatial aggregation groups.
#' @param aggregate_time Temporal scale used to perform
#' temporal aggregation. Options are: "week" (ISO 8601), "month", "year".
#'
#' @return A data frame with the aggregated cases.
#' @export
#'

aggregate_cases <- function(data = NULL,
                            cases = NULL,
                            pop = NULL,
                            time = NULL,
                            area = NULL,
                            pt = 100000,
                            aggregate_space = NULL,
                            aggregate_time = NULL) {
  
  # Check data is a data.frame
  if (!is.data.frame(data)) stop("'data' should be a data.frame")
  
  # Check necessary columsn and their type
  if (any(c(cases, time) %in% colnames(data) == FALSE)) {
    stop("'time' and/or 'cases' not found in the data")
  }
  if (!is.numeric(data[[cases]])) stop("'cases' should be numeric")
  
  # Validate aggregate_space and aggregate_time
  if (!is.null(aggregate_space) && !aggregate_space %in% colnames(data)) {
    stop("No column of the data matches the 'aggregate_space' argument")
  }
  if (!is.null(aggregate_time) && !aggregate_time %in% c("week", "month", "year")) {
    stop("'aggregate_time' can be 'week','month', or 'year'")
  }
  
  # Check that 'time' is in the correct date format and if correct
  # transform it to a Date
  if (any(is.na(as.Date(data[[time]], format = "%Y-%m-%d")))) {
    stop("'Date' should be in 'yyyy-mm-dd' format")
  } else (data[[time]] <- as.Date(data[[time]], format = "%Y-%m-%d"))
  
  t <- .get_time_interval(data = data,
                          time = time,
                          area = area)
  
  # Check time aggregation 
  scale_rank <- c("day" = 1, "week" = 2, "month" = 3, "year" = 4)
  if (!is.null(aggregate_time)) {
    # If data scale is recognized
    if (!t %in% names(scale_rank)) {
      stop(sprintf("Unrecognized current data scale '%s'.", t))
    }
    
    # If user tries to go from e.g. monthly (3) -> weekly (2), throw error
    if (scale_rank[aggregate_time] < scale_rank[t]) {
      stop(sprintf(
        "Cannot aggregate from %s data to %s data (finer resolution). 
         Please choose a coarser time scale.",
        t, aggregate_time
      ))
    }
  }
  
  # Add a column for area with same value for all observations if area is missing 
  # and area was not provided by the user 
  if (is.null(area) && is.null(aggregate_space)) {
    data$area <- 1
    area <- "area"
  }
  
  # 1. Aggregate_space ----
  if(!is.null(aggregate_space)) {
    
    data <- data |>
      dplyr::select(
        time = {{ time }},
        area = {{ aggregate_space }},
        cases = {{ cases }},
        pop = {{ pop }}) |>
      dplyr::mutate(time = as.Date(time), area = as.character(area)) |>
      dplyr::group_by(time, area) |>
      dplyr::summarise(cases = sum(cases), pop = sum(pop), .groups = "drop")
    
  } else { data <- data |>
    dplyr::select(
      time = {{ time }},
      area = {{ area }},
      cases = {{ cases }},
      pop = {{pop}}) |>
    dplyr::mutate(time = as.Date(time), area = as.character(area))
  }
  
  # 2. Aggregate_time ----
  
  #  Week
  if (!is.null(aggregate_time) && aggregate_time == "week") {
    
    data$time <- ISOweek::ISOweek(as.Date(data$time))
    data <- data |>
      dplyr::group_by(area,time) |>
      dplyr::summarise(cases = sum(cases), pop = mean(pop)) |>
      dplyr::ungroup()
    
    data$time <- format(ISOweek::ISOweek2date(paste0(data$time, "-1")),
                        "%Y-%W")
  }
  
  # Month
  else if (!is.null(aggregate_time) && aggregate_time == "month") {
    
    data$time <- format(as.Date(data$time), "%Y-%m")
    data <- data |>
      dplyr::group_by(area,time) |>
      dplyr::summarise(cases = sum(cases), pop = mean(pop)) |>
      dplyr::ungroup()
  }
  else if (!is.null(aggregate_time) && aggregate_time == "year") {
    
    data$time <- format(as.Date(data$time), "%Y")
    data <- data |>
      dplyr::group_by(area,time) |>
      dplyr::summarise(cases = sum(cases), pop = mean(pop)) |>
      dplyr::ungroup()
  }

  # Compute Incidence
  data$inc <- (data$cases/data$pop) * pt
  
  if (is.null(pop)) {
    data <- data |> dplyr::select(area,time,cases)
  }
  
  return(data)
}


#' .ymd_strict
#' 
#' Checks parsing of a date accoring to yyyy-mm-dd
#'
#' @param x a string to be parsed
#' @return an aggregated dataset
#' @noRd
.ymd_strict <- function(x) {
  parsed <- as.Date(x, format = "%Y-%m-%d")
  ifelse(!is.na(parsed) & format(parsed, "%Y-%m-%d") == x, parsed, as.Date(NA))
}

#' .log10p1_trans
#' 
#' Transform to execute a log10(x+1) scale transformation
#' @noRd
.log10p1_trans <- scales::trans_new(
  name = "log10p1",
  transform = function(x) log10(x + 1),
  inverse = function(x) 10^x - 1
)

#' .log10_breaks_like
#' 
#' Breaks and label generator for .log10p1_trans
#' @param x the numeric vector for which labels are computed
#' @noRd
.log10_breaks_like <- function(x) {
  rng <- range(x, na.rm = TRUE)
  upper <- ceiling(log10(rng[2] + 1))
  lower <- floor(log10(max(1, rng[1] + 1))) 
  breaks <- 10^(lower:upper)
  if(lower==0 & any(x<1, na.rm = TRUE)){
    breaks <- c(0, breaks )
  }
  return(breaks)
}


#' Make first element of string upper case
#'
#' @param x character
#'
#' @returns the modified string
#' @noRd
.firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}