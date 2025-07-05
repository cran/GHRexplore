
# Tests for .get_time_interval ----

test_that(".get_time_interval detects daily data correctly", {
  # A minimal daily dataset
  df_daily <- data.frame(
    time = c("2021-01-01", "2021-01-02", "2021-01-03"))
  
  interval <- .get_time_interval(data = df_daily, time = "time")
  expect_equal(interval, "day")
})

test_that(".get_time_interval detects weekly data correctly", {
  df_weekly <- data.frame(
    time = c("2021-01-01", "2021-01-08", "2021-01-15")
  )
  interval <- .get_time_interval(df_weekly, time = "time")
  expect_equal(interval, "week")
})

test_that(".get_time_interval requires no duplicate time if area is NULL", {
  df_dup <- data.frame(
    time = c("2021-01-01", "2021-01-01")) # duplicated date
  
  expect_error(
    .get_time_interval(df_dup, time = "time"),
    "Duplicated time units detected"
  )
})

test_that(".get_time_interval can handle multiple areas (no error if duplicates across areas)", {
  df_groups <- data.frame(
    group = c("A","A","B","B"),
    time = as.Date(c("2021-01-01", "2021-01-02", "2021-01-01", "2021-01-02"))
  )
  # No error should occur, because we specified `group`
  interval <- .get_time_interval(df_groups, time = "time", area = "group")
  # Because the difference between first two time points is 1 day
  expect_equal(interval, "day")
})


# Tests for .check_consecutive ----
test_that(".check_consecutive identifies consecutive daily data (no area)", {
  df_daily <- data.frame(
    time = c("2021-01-01","2021-01-02","2021-01-03")
  )
  # All consecutive daily intervals
  expect_true(.check_consecutive(df_daily, time = "time"))
})

test_that(".check_consecutive returns FALSE if daily data is missing a date", {
  # Skipping 2021-01-02
  df_skip <- data.frame(
    time = c("2021-01-01","2021-01-03")
  )
  # Not consecutive
  expect_warning(.check_consecutive(df_skip, time = "time"),
                 "Some intervals between consecutive time points")
})

test_that(".check_consecutive works with multiple areas", {
  df_group <- data.frame(
    group = c("A","A","B","B"),
    time = as.Date(c("2021-01-01","2021-01-02","2021-01-01","2021-01-02"))
  )
  # Both A and B are consecutive daily
  expect_true(.check_consecutive(df_group, time = "time", area = "group"))
})

test_that(".check_consecutive throws error if duplicates exist and area not provided", {
  df_dup <- data.frame(
    time = as.Date(c("2021-01-01","2021-01-01"))
  )
  expect_error(
    .check_consecutive(df_dup, time = "time"),
    "Duplicated time units detected"
  )
})

# Tests for .check_na ----

# Test 1: Check if warning is raised when there are NAs
test_that("Warning is issued when NAs are present", {
  df <- data.frame(
    id = 1:5,
    value = c(10, NA, 30, 40, 50),
    name = c("A", "B", "C", "D", "E"),
    stringsAsFactors = FALSE
  )
  expect_warning(.check_na("value", df), "Missing values found in the ' value ' column")
  expect_error(.check_na("value", df, error=TRUE),
               "Missing values found in the ' value ' column")
  
})


# Test 2: Check no warning when there are no NAs
test_that("No warning when no NAs are present", {
  df <- data.frame(
    id = 1:5,
    value = c(10, NA, 30, 40, 50),
    name = c("A", "B", "C", "D", "E"),
    stringsAsFactors = FALSE
  )
  expect_silent(.check_na("name", df))
})

# Test 3: Check behavior when column does not exist
test_that("No warning for non-existent column", {
  df <- data.frame(
    id = 1:5,
    value = c(10, NA, 30, 40, 50),
    name = c("A", "B", "C", "D", "E"),
    stringsAsFactors = FALSE
  )
  expect_silent(.check_na("non_existent", df))
})

# Test 4: Check behavior when col_name is NULL
test_that("No warning when col_name is NULL", {
  df <- data.frame(
    id = 1:5,
    value = c(10, NA, 30, 40, 50),
    name = c("A", "B", "C", "D", "E"),
    stringsAsFactors = FALSE
  )
  expect_silent(.check_na(NULL, df))
})

# Tests for aggregate_cov ----

test_that("'data' should be a data.frame", {
  expect_error(aggregate_cov(data = NULL, var = "cases", time = "date"), "'data' should be a data.frame")
})

test_that("'time' and/or 'var' not found in the data", {
  data <- data.frame(region = c("A", "B"), cases = c(10, 20))
  expect_error(aggregate_cov(data = data, var = "cases", time = "date"), "'time' and/or 'var' not found in the data")
})

test_that("'var' should be numeric", {
  data <- data.frame(date = as.Date(c("2025-01-01", "2025-01-02")), cases = c("ten", "twenty"))
  expect_error(aggregate_cov(data = data, var = "cases", time = "date"), "'var' should be numeric")
})


test_that("'aggregate_time' should be valid", {
  data <- data.frame(date = as.Date("2025-01-01"), cases = 10)
  expect_error(aggregate_cov(data = data, var = "cases", time = "date", aggregate_time = "quarter"),
               "'aggregate_time' can be 'week','month', or 'year'")
})

test_that("'aggregate_space_fun' should be valid", {
  data <- data.frame(date = as.Date("2025-01-01"), cases = 10, region = "a")
  expect_error(aggregate_cov(data = data, var = "cases", time = "date",
                             aggregate_space = "region", aggregate_space_fun = "max"),
               "aggregate_space_fun can be 'sum', 'mean' 'median'")
})

test_that("'aggregate_time_fun' should be valid", {
  data <- data.frame(date = as.Date("2025-01-01"), cases = 10)
  expect_error(aggregate_cov(data = data, var = "cases", time = "date",
                             aggregate_time = "month", aggregate_time_fun = "mode"),
               "aggregate_time_fun can be 'sum', 'mean', 'median'")
})

test_that("'Date' should be in 'yyyy-mm-dd' format", {
  data <- data.frame(date = c("2025-18-02", "2025-19-02"), cases = c(10, 15))
  expect_error(aggregate_cov(data = data, var = "cases", time = "date"), "'Date' should be in 'yyyy-mm-dd' format")
})


test_that("aggregate_cov basic mean over time (e.g. month)", {
  df <- data.frame(
    date = as.Date(c("2021-01-01","2021-01-02","2021-02-01","2021-02-02")),
    covar = c(10, 30, 20, 40)
  )
  out <- aggregate_cov(
    data = df,
    var = "covar",
    time = "date",
    aggregate_time = "month",
    aggregate_time_fun = "mean"
  )
  # There are two distinct months: 2021-01 and 2021-02
  # Jan => mean(10,30) = 20
  # Feb => mean(20,40) = 30
  expect_equal(nrow(out), 2)
  expect_equal(sort(out$var), c(20, 30))
})

test_that("aggregate_cov sum over space, no time aggregation", {
  df <- data.frame(
    date = as.Date(c("2021-01-01","2021-01-01","2021-01-02","2021-01-02")),
    micro_area = c("A","B","A","B"),
    macro_area = c( "Z","Z", "Z", "Z"),
    covar= c(1,2,3,4)
  )
 #Sum across dept 
  out <- aggregate_cov(
    data = df,
    var = "covar",
    time = "date",
    area = "micro_area",           # original area col
    aggregate_space = "macro_area", 
    aggregate_space_fun = "sum",
    # no aggregate_time => results remain daily
    aggregate_time = NULL
  )
  # So for date=2021-01-01 => sum(1+2)=3
  # for date=2021-01-02 => sum(3+4)=7
  expect_equal(nrow(out), 2)
  expect_equal(out$var, c(3,7))
})

test_that("aggregate_cov fails if var is not numeric", {
  df <- data.frame(
    date = as.Date(c("2021-01-01","2021-01-02")),
    covar = c("x","y")
  )
  expect_error(
    aggregate_cov(df, var = "covar", time = "date"),
    "'var' should be numeric"
  )
})


# Tests for aggregate_cases ----

test_that("'data' should be a data.frame", {
  expect_error(aggregate_cases(data = NULL, cases = "cases", time = "time"), 
               "'data' should be a data.frame")
})

test_that("'time' and/or 'cases' should exist in data", {
  data <- data.frame(date = as.Date("2025-01-01"), counts = 10)
  expect_error(aggregate_cases(data = data, cases = "cases", time = "time"), 
               "'time' and/or 'cases' not found in the data")
})

test_that("'cases' should be numeric", {
  data <- data.frame(time = as.Date("2025-01-01"), cases = "ten")
  expect_error(aggregate_cases(data = data, cases = "cases", time = "time"), 
               "'cases' should be numeric")
})

test_that("'aggregate_space' should exist in data", {
  data <- data.frame(time = as.Date("2025-01-01"), cases = 10, region = "A")
  expect_error(aggregate_cases(data = data, cases = "cases", time = "time", 
                               aggregate_space = "nonexistent_column"), 
               "No column of the data matches the 'aggregate_space' argument")
})

test_that("'aggregate_time' should be valid", {
  data <- data.frame(time = as.Date("2025-01-01"), cases = 10)
  expect_error(aggregate_cases(data = data, cases = "cases", time = "time", 
                               aggregate_time = "daily"), 
               "'aggregate_time' can be 'week','month', or 'year'")
})

test_that("'Date' should be in 'yyyy-mm-dd' format", {
  data <- data.frame(date = c("2025-18-02", "2025-19-02"), cases = c(10, 15))
  expect_error(aggregate_cases(data = data, cases = "cases", time = "date"), "'Date' should be in 'yyyy-mm-dd' format")
})


test_that("Handles missing population column", {
  data <- data.frame(
    time = as.Date(c("2025-01-01", "2025-01-02")),
    cases = c(10, 20),
    region = c("A", "A")
  )
  
  result <- aggregate_cases(data, cases = "cases", time = "time", 
                            aggregate_space = "region")
  
  expect_false("pop" %in% colnames(result)) # Should not have 'pop' column
  expect_equal(ncol(result), 3) # Should only contain area, time, and cases
})



## old code
test_that("aggregate_cases sums  'cases' and  sum 'pop' across space", {
  df <- data.frame(
    date = as.Date(c("2021-01-01","2021-01-01","2021-01-02","2021-01-02")),
    microarea = c("A","B","A","B"),
    macroarea = c("Z","Z","Z","Z"),
    cases  = c(10,20,30,40),
    pop    = c(100,200,100,200), 
    pt = 100000
  )
  # sum(cases) per date, average(pop) per date if we do no time aggregation
  out <- aggregate_cases(
    data = df,
    cases = "cases",
    pop   = "pop",
    time  = "date",
    area  = "microarea",
    aggregate_space = "macroarea"
  )
  # For 2021-01-01 => sum(10+20)=30, pop= sum(100,200)=300
  # For 2021-01-02 => sum(30+40)=70, pop= sum(100,200)=300
  expect_equal(nrow(out), 2)
  expect_equal(out$cases, c(30,70))
  expect_equal(out$pop,   c(300,300))
  # inc = (cases / pop) * pt (default pt = 100000)
  # inc => c((30/300)*100000, (70/300)*100000) => c(0.2, 0.466..., which is 0.46666)
  expect_equal(round(out$inc, 2), c(10000, 23333.33))
})

test_that("aggregate_cases only aggregates over time (sums 'cases' and averages 'pop')", {
  df <- data.frame(
    date = as.Date(c("2021-02-01", "2021-02-08", "2021-02-15", "2021-02-22")),
    region = c("Z","Z","Z","Z"),
    cases  = c(1, 3, 2, 1),
    pop    = c(100,100,100,100)
  )
  # We do not specify 'aggregate_space', so no spatial aggregation
  # We'll aggregate by 'month' in time
  out <- aggregate_cases(
    data = df,
    cases = "cases",
    pop   = "pop",
    time  = "date",
    area  = "region",
    aggregate_time = "month"
  )
  # Expected:
  # For February 2021 => sum(cases) = 1 + 3 + 2 + 1 = 7, average(pop) = 100
  # inc => (cases / pop) * pt => c((7/100)*100000) => c(7000)
  
  # There should be 2 rows: one for 2021-01 and one for 2021-02
  expect_equal(nrow(out), 1)
  
  # Sort by time if needed, or match exactly
  expect_equal(out$cases, c(7))
  expect_equal(out$pop, c(100))
  expect_equal(out$inc, c(7000))
})


test_that("aggregate_cases aggregates spatio-temporally (sums 'cases' across space and time, sum or avg pop)", {
  df <- data.frame(
    date      = as.Date(c(
      "2021-02-01","2021-02-01","2021-02-08","2021-02-08",
      "2021-02-15","2021-02-15","2021-02-22","2021-02-22"
    )),
    microarea = c("A","B","A","B","A","B","A","B"),
    macroarea = c("Z","Z","Z","Z","Z","Z","Z","Z"),
    cases     = c(1,4,3,2,1,2,1,5),
    pop       = c(100,200,100,200,100,200,100,200)
  )
  # We want to aggregate both in space (macroarea) and time (month).
  # => For each month, sum the cases and sum the pop across all microareas
  out <- aggregate_cases(
    data = df,
    cases = "cases",
    pop   = "pop",
    time  = "date",
    area  = "microarea",     # your "finest" area
    aggregate_space = "macroarea",  # aggregated variable
    aggregate_time  = "month"       # temporal aggregation
  )
  
  #  February 2021:
  #   Cases => sum() = 19
  #   Pop   => sum for the two areas (100+200) and averaged across times (300)
  # inc => (cases / pop)
  #   Fev inc => 19/300 = 0.063
  
  expect_equal(nrow(out), 1)
  

  # Cases
  expect_equal(sort(out$cases), c(19))
  
  # Pop
  expect_equal(out$pop, c(300))
  
  # Inc
  # Check approximate decimal
  # (19/300)*100000 ~ 6333.333
  expect_equal(round(out$inc, 3), c(6333.333))
})
