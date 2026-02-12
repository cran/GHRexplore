# epiyw_to_date ----

test_that("epiyw_to_date converts known examples correctly", {
  expect_equal(
    epiyw_to_date("2025-01", weekday = "Monday"),
    as.Date("2024-12-30")
  )
  expect_equal(
    epiyw_to_date("2025-01", weekday = "Tuesday"),
    as.Date("2024-12-31")
  )
  expect_equal(
    epiyw_to_date("2025-01", weekday = "Wednesday"),
    as.Date("2025-01-01")
  )
  expect_equal(
    epiyw_to_date("2025-01", weekday = "Thursday"),
    as.Date("2025-01-02")
  )
  expect_equal(
    epiyw_to_date("2025-01", weekday = "Friday"),
    as.Date("2025-01-03")
  )
  expect_equal(
    epiyw_to_date("2025-01", weekday = "Saturday"),
    as.Date("2025-01-04")
  )
  expect_equal(
    epiyw_to_date("2025-01", weekday = "Sunday"),
    as.Date("2024-12-29")
  )
})

test_that("epiyw_to_date handles vector input", {
  x <- c("2025-01", "2025-02")
  out <- epiyw_to_date(x, "Monday")
  expect_length(out, 2)
  expect_true(all(inherits(out, "Date")))
})

test_that("epiyw_to_date handles week 53 correctly", {
  expect_equal(
    epiyw_to_date("2020-53", "Monday"),
    as.Date("2020-12-28")
  )
})

test_that("epiyw_to_date validates input format", {
  expect_error(
    epiyw_to_date(202501),
    "epiyw' must be numeric in 'YYYY-WW' format."
  )
  expect_error(
    epiyw_to_date("2025-W1"),
    "epiyw' must be numeric in 'YYYY-WW' format."
  )
  expect_error(
    epiyw_to_date(c("2025-01", NA_character_)),
    "'epiyw' contains missing values."
  )
  expect_error(epiyw_to_date("2025-00"), "'epiyw' contains invalid epiweeks.")
  expect_error(epiyw_to_date("2025-54"), "'epiyw' contains invalid epiweeks.")
})

test_that("epiyw_to_date validates weekday", {
  expect_error(
    epiyw_to_date("2025-01", weekday = "Funday"),
    "'weekday' no recognised."
  )
})


# date_to_epiyw ----
test_that("date_to_epiyw converts known dates correctly", {
  expect_equal(date_to_epiyw(as.Date("2024-12-30"), "Monday"), "2025-01")
  expect_equal(date_to_epiyw(as.Date("2024-12-30"), "Tuesday"), "2024-52")
  expect_equal(date_to_epiyw(as.Date("2024-12-30"), "Wednesday"), "2024-52")
  expect_equal(date_to_epiyw(as.Date("2024-12-30"), "Thursday"), "2024-52")
  expect_equal(date_to_epiyw(as.Date("2024-12-30"), "Friday"), "2024-53")
  expect_equal(date_to_epiyw(as.Date("2024-12-30"), "Saturday"), "2024-53")
  expect_equal(date_to_epiyw(as.Date("2024-12-30"), "Sunday"), "2025-01")
})


test_that("date_to_epiyw handles vector inputs", {
  d <- as.Date(c("2024-12-30", "2025-01-06"))
  out <- date_to_epiyw(d, "Monday")
  expect_length(out, 2)
  expect_true(is.character(out))
  expect_equal(out, c("2025-01", "2025-02"))
})


test_that("date_to_epiyw handles week 53 correctly", {
  expect_equal(date_to_epiyw(as.Date("2021-01-03"), "Monday"), "2020-53")
})

test_that("date_to_epiyw and epiyw_to_date are inverse functions", {
  x <- as.Date("2025-04-15")
  epiyw <- date_to_epiyw(x, "Monday")
  back <- epiyw_to_date(epiyw, "Monday")
  expect_equal(back, x - as.integer(lubridate::wday(x, week_start = 1) - 1))
})

test_that("date_to_epiyw validates inputs", {
  expect_error(date_to_epiyw("2024-12-30"))
  expect_error(date_to_epiyw(as.Date(c("2024-12-30", NA))))
  expect_error(date_to_epiyw(as.Date("2024-12-30"), weekday = "Funday"))
})


# Extensive check with lubridate ----
test_that("GHR epiweek functions match lubridate's", {
  epibind <- function(y, w) {
    paste0(y, "-", sprintf("%02d", w))
  }

  epibind2 <- function(y, w) {
    paste0(y, "-W", sprintf("%02d", w))
  }

  dateseq <- seq.Date(
    from = as.Date("2000-01-01"),
    to = as.Date("2025-12-31"),
    by = "day"
  )
  res <- data.frame(dates = dateseq)

  # Test date_to_epiyw
  res$lub_isoyear <- lubridate::isoyear(res$dates)
  res$lub_isoweek <- lubridate::isoweek(res$dates)
  res$lub_epiyear <- lubridate::epiyear(res$dates)
  res$lub_epiweek <- lubridate::epiweek(res$dates)
  res$lub_isoyw <- epibind(res$lub_isoyear, res$lub_isoweek)
  res$lub_epiyw <- epibind(res$lub_epiyear, res$lub_epiweek)
  res$ghr_isoyw <- date_to_epiyw(res$dates)
  res$ghr_epiyw <- date_to_epiyw(res$dates, weekday = "Sunday")

  # Conditions
  expect_equal(res$lub_isoyw, res$ghr_isoyw)
  expect_equal(res$lub_epiyw, res$ghr_epiyw)

  # Test epiyw_to_date
  res$date_monday <- lubridate::floor_date(
    res$dates,
    unit = "week",
    week_start = "Monday"
  )
  res$date_sunday <- lubridate::floor_date(
    res$dates,
    unit = "week",
    week_start = "Sunday"
  )
  res$ghr_isodate <- epiyw_to_date(res$lub_isoyw, "Monday")
  res$ghr_epidate <- epiyw_to_date(res$lub_epiyw, "Sunday")

  # Conditions
  expect_equal(res$date_monday, res$ghr_isodate)
  expect_equal(res$date_sunday, res$ghr_epidate)
})
