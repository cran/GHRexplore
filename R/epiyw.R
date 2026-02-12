#' Convert a epiyearweek to date format
#'
#' @description The conversion is done following the ISO 8601 standard,
#' whereby the first week of the year is the one that contains at least
#' four days of the  new year. This coincides with the approach taken
#' by `lubridate::epiweek` and `lubridate::isoweek` but may not
#' coincide with other definitions of epidemiological weeks.
#'
#' @param epiyw An epiyear-epiweek character vector in 'YYYY-WW' format.
#' @param weekday Starting day of the epiweek. Defaults to Monday.
#'
#' @return The converted epiyearweeks to dates.
#' @export
#'
#' @examples
#' epiyw <- c("2025-01", "2025-10", "2025-20", "2025-30")
#' epiyw_to_date(epiyw)
epiyw_to_date <- function(epiyw, weekday = "Monday") {
  # Input checks ----
  # Epi yearweeks, no missings
  if (any(is.na(epiyw))) {
    stop("'epiyw' contains missing values.")
  } else if (
    !is.character(epiyw) ||
      any(nchar(epiyw) != 7) ||
      any(!grepl("^\\d{4}-\\d{2}$", epiyw))
  ) {
    stop("'epiyw' must be numeric in 'YYYY-WW' format.")
  }

  # Get epiyear and epiweek
  epiyear <- as.numeric(substr(epiyw, 1, 4))
  epiweek <- as.numeric(substr(epiyw, 6, 7))

  # Valid epiweek
  if (any(epiweek < 1) || any(epiweek > 53)) {
    stop("'epiyw' contains invalid epiweeks.")
  }

  # Week days
  wdays <- c(
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
  )
  if (!weekday %in% wdays) {
    stop("'weekday' no recognised.")
  }

  # Conversion ----

  # Get the date of the first epiweek of the epiyear
  jan4 <- as.Date(paste0(as.character(epiyear), "-01-04"))
  jan4wstart <- lubridate::floor_date(jan4, unit = "week", week_start = weekday)

  # Compute day
  date <- jan4wstart + 7 * (epiweek - 1)

  return(date)
}


#' Convert a date to epiyearweek format
#'
#' @description The conversion is done following the ISO 8601 standard,
#' whereby the first week of the year is the one that contains at
#' least four days of the new year. This coincides with the
#' approach taken by `lubridate::epiweek` and `lubridate::isoweek`
#' but may not coincide with other definitions of epidemiological weeks.
#'
#' @param date A vector of type 'Date'.
#' @param weekday The weekday that determines the start of the week. Defaults to
#' 'Monday'.
#'
#' @return The transformed dates to epiyearweek.
#' @export
#'
#' @examples
#' # Convert dates in 'dengue_SP' in epiyw
#' data(dengue_SP)
#' head(dengue_SP$date)
#' dengue_SP$epiyw <- date_to_epiyw(as.Date(dengue_SP$date), "Sunday")
#' head(dengue_SP$epiyw)
date_to_epiyw <- function(date, weekday = "Monday") {
  # Input checks ----

  # Dates
  if (!inherits(date, "Date")) {
    stop("'date' must be of type 'Date'.")
  } else if (any(is.na(date))) {
    stop("'date' contains missing values.")
  }

  # Week days
  wdays <- c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
  )
  if (!weekday %in% wdays) {
    stop("'weekday' not recognised.")
  }

  # Conversion ----

  # Rebased date - first and last day of the week according to the start weekday
  wstart <- lubridate::floor_date(date, unit = "week", week_start = weekday)
  wend <- lubridate::ceiling_date(date, unit = "week", week_start = weekday) - 1

  # The end of the first week must fall at least four days into the year
  epiyear <- lubridate::year(wend - 3)

  # We get the epiweek with respect to Jan 4th, always week 1 by ISO definition
  jan4 <- as.Date(paste0(as.character(epiyear), "-01-04"))
  jan4wstart <- lubridate::floor_date(jan4, unit = "week", week_start = weekday)
  epiweek <- 1L + (as.numeric(wstart) - as.numeric(jan4wstart)) %/% 7L

  # Output
  epiyw <- paste0(epiyear, "-", sprintf("%02d", epiweek))
  return(epiyw)
}
