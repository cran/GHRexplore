

# Test case: Missing 'data' argument
test_that("Error when 'data' is missing", {
  expect_error(plot_timeseries(time = "date"), "Error: Missing required argument 'data'")
})

# Test case: 'data' is not a data.frame
test_that("'data' should be a data.frame", {
  expect_error(plot_timeseries(data = list(a = 1), time = "date"),
               "'data' should be a data.frame")
})

# Test case: Missing 'time' argument
test_that("Error when 'time' is missing", {
  data <- data.frame(a = 1:5)
  expect_error(plot_timeseries(data = data, var="a"), "Error: Missing required argument 'time'")
})

# Test case: 'time' missing in input
test_that("'time' argument missing", {
  data <- data.frame(time = as.Date("2025-02-18"), a = 10)
  expect_error(plot_timeseries(data = data, var = "a"), "Error: Missing required argument 'time'")
})

# Test case: Invalid 'time' format
test_that("'Date' format is invalid", {
  data <- data.frame(time = c("2025-18-02", "2025-19-02"), a = 10)
  expect_error(plot_timeseries(data = data, var="a", time = "time"),
               "'Date' should be in 'yyyy-mm-dd' format")
})

# Test case: Missing 'var' argument
test_that("Error when 'var' is missing", {
  data <- data.frame(time = as.Date("2025-01-01"), a = 1)
  expect_error(plot_timeseries(data = data, time=time), "Error: Missing required argument 'var'")
})

# Test case: Check if 'area' exists in data
test_that("No column matches the 'area' argument", {
  data <- data.frame(time = as.Date("2025-01-01"), a = 1)
  expect_error(plot_timeseries(data = data, time = "time", area = "region",
                               var = "a"),
               "No column of the data matches the 'area' argument")
})

# Test case: 'aggregate_space' is invalid if specified
test_that("No column matches the 'aggregate_space' argument", {
  data <- data.frame(time = as.Date("2025-01-01"), a = 1)
  expect_error(plot_timeseries(data = data, time = "time", var = "a",
                               aggregate_space = "region"),
               "No column of the data match the 'aggregate_space' argument")
})

test_that("incorrect type", {
  data <- data.frame(time = as.Date("2025-01-01"), cases = 1)
  expect_error(plot_seasonality(data = data, var = "cases", 
                                time = "time", type = "invalid"), 
               "type must be either 'cov', 'counts' or 'inc'")
})

# Test case: 'aggregate_time' has invalid value
test_that("'aggregate_time' can be 'week','month',year'", {
  data <- data.frame(time = as.Date("2025-01-01"), a = 1)
  expect_error(plot_timeseries(data = data, time = "time", aggregate_time = "day",
                               var = "a"),
               "'aggregate_time' can be 'week','month',year'")
})

# Test case: 'aggregate_space_fun' has invalid function
test_that("aggregate_space_fun can be 'sum', 'mean', 'median'", {
  data <- data.frame(time = as.Date("2025-01-01"), cases = 10, region = "a")
  # Test that invalid aggregate_space_fun throws the expected error
  expect_error(
    plot_timeseries(data = data, var = "cases", time = "time", area = "region",
                    aggregate_space = "region", aggregate_space_fun = "max"),
    "aggregate_space_fun can be 'sum', 'mean' 'median'"
  )
})


# Test case: 'aggregate_time_fun' has invalid function
test_that("aggregate_time_fun can be 'sum', 'mean', 'median'", {
  data <- data.frame(date = as.Date("2025-01-01"), cases = 10)
  expect_error(plot_timeseries(data = data, var = "cases", time = "date",
                               aggregate_time = "month", aggregate_time_fun = "max"),
               "aggregate_time_fun can be 'sum', 'mean', 'median'")
})

# Test case: 'transform' has invalid function
test_that("valid transform", {
  data <- data.frame(date = as.Date("2025-01-01"), cases = 10)
  expect_error(plot_timeseries(data = data, var = "cases", time = "date",
                               aggregate_time = "month", transform = "wrong"),
               "Invalid transform.")
})

# Test case: 'facet' and 'highlight' cannot be specified together
test_that("'facet' and 'highlight' cannot be specified together", {
  data <- data.frame(time = as.Date("2025-01-01"), a = 1)
  expect_error(plot_timeseries(data = data, time = "time",var = "a",
                               facet = TRUE, highlight = "a"),
               "'facet' and 'highlight' cannot be specified together")
})

# Test case: Missing values in 'var'
test_that("Missing values in 'var'", {
  data <- data.frame(
    time = as.Date(c('2025-01-01', '2025-01-02', '2025-01-03')),
    a = c(1, NA, 3)
  )
  # Match only the warning related to missing values in 'cases', ignoring others.
  expect_warning(
    plot_timeseries(data = data, time = "time", var = "a"),
    "Missing values found in the ' a ' column",
    fixed = TRUE  # Use fixed = TRUE for exact matching
  )
})

test_that("Missing values in 'var'", {
  data <- data.frame(
    time = as.Date(c('2025-01-01', '2025-01-02', '2025-01-03')),
    cases = c(1, NA, 3)
  )
  # Match only the warning related to missing values in 'cases', ignoring others.
  expect_warning(
    plot_timeseries(data = data, var="cases", time = "time"),
    "Missing values found in the ' cases ' column",
    fixed = TRUE  # Use fixed = TRUE for exact matching
  )
})


# Option 1. If the variable to be plotted is a covariate ----

# Test case: 'var' is not a column in the data
test_that("Error when 'var' is not found in the data", {
  data <- data.frame(time = as.Date('2025-01-01'), value = 5)
  
  expect_error(plot_timeseries(data, var = "missing_var", time = "time"),
               "No column of the data matches the 'var' argument")
})

# Test case: 'var' is not numeric
test_that("'var' should be numeric", {
  data <- data.frame(time = as.Date('2025-01-01'), value = "non-numeric")
  
  expect_error(plot_timeseries(data, var = "value", time = "time"),
               "'var' should be numeric")
})

# Test case: Duplicated time units detected in a single time series
test_that("Duplicated time units in a single time series", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-01')), value = 5)
  
  expect_error(plot_timeseries(data, var = "value", time = "time"),
               "Duplicated time units detected. If the data contains")
})

# Test case: Duplicated time units within the same area
test_that("Duplicated time units detected within the same 'area'", {
  data <- data.frame(time = rep(as.Date('2025-01-01'), 2), 
                     value = 5, 
                     area = c('Area1', 'Area1'))
  
  expect_error(plot_timeseries(data, var = "value", time = "time", area = "area"),
               "Duplicated time units detected. Ensure ")
})

# Test case: Correct plot for a single time series (no area)
test_that("Correct plot for single time series", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02')), 
                     value = c(5, 6))
  
  plot_result <- plot_timeseries(data, var = "value", time = "time", facet = FALSE)
  
  expect_s3_class(plot_result, "gg")  # Ensure it's a ggplot object
})

# Test case: Warning when there are more than 15 areas and highlight is not specified
test_that("Warning when more than 15 areas without highlight", {
  data <- data.frame(time = rep(as.Date('2025-01-01'), 20), 
                     value = 5, 
                     area = rep(letters[1:20], each = 1))
  
  expect_warning(plot_timeseries(data, var = "value", time = "time", area = "area"),
                 "More than 15 time series detected. Try 'highlight' or 'aggregate_space'?")
})


# Test case: Correct handling of aggregation for multiple time series
test_that("Correct handling of aggregation", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-01-01', '2025-01-02')), 
                     value = c(1,2,2,1), 
                     area = rep(1:2, each = 2))
  
  plot_result <- plot_timeseries(data, var = "value", time = "time", area = "area", 
                                 aggregate_space ="area")
  
  expect_s3_class(plot_result, "gg")  # Ensure it's a ggplot object
})


# Test case: Test for free y-axis scaling if free_y_scale is TRUE
test_that("Free y-axis scaling when free_y_scale is TRUE", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-01-01', '2025-01-02')), 
                     value = c(1,2,3,1), 
                     area = rep(1:2, each = 2))
  
  plot_result <- plot_timeseries(data, var = "value", time = "time", area = "area", 
                                 facet = TRUE, free_y_scale = TRUE)
  
  expect_s3_class(plot_result, "gg")  # Ensure it's a ggplot object
  
  # Extract the plot's scales using ggplot_build
  plot_build <- ggplot2::ggplot_build(plot_result)
  
  # Check if the facet_wrap facet has 'scales = "free_y"'
  facet_info <- plot_build$layout$facet$params
  expect_true(facet_info$free$y == "TRUE")
})

# Test case: Check for plot title generation logic
test_that("Correct title generation", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-01-01', '2025-01-02')), 
                     value = c(1,2,3,1), 
                     area = rep(1:2, each = 2))
  
  plot_result <- plot_timeseries(data, var = "value", time = "time", area = "area",
                                 title = "Custom Title")
  expect_equal(plot_result$labels$title, "Custom Title")
  
  plot_result <- plot_timeseries(data, var = "value", time = "time", area = "area")
  expect_equal(plot_result$labels$title, NULL)
})

# Test case: Correct handling of different time aggregation - week
test_that("Correct time aggregation handling", {
  data<- data.frame(
    time = as.Date(c(
      "2021-01-05","2021-01-06","2021-01-07",
      "2021-01-08", "2021-01-09","2021-01-10","2021-01-11")),
    var = 1
  )
  plot_result_week<- plot_timeseries(data, var = "var", time = "time", aggregate_time = "week")
  expect_equal(as.Date(c("2021-01-04", "2021-01-11")), plot_result_week$data$time)
  
})

# Test case: Correct handling of different time aggregation - month
test_that("Correct time aggregation handling", {
  data<- data.frame(
    time = seq.Date(as.Date("2021-01-01"), as.Date("2021-02-28"), "week"),
    var = 1
  )
  plot_result_month<- plot_timeseries(data, var = "var", time = "time", aggregate_time = "month")
  expect_equal(as.Date(c("2021-01-01", "2021-02-01")), plot_result_month$data$time)
  
})

# Test case: Highlight functionality for areas
test_that("Highlight functionality for areas", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-01-01', '2025-01-02')), 
                     value = c(1,2,3,1), 
                     area = rep(letters[1:2], each = 2))
  
  
  plot_result <- plot_timeseries(data, var = "value", time = "time", area = "area", 
                                 highlight = "a")
  
  expect_s3_class(plot_result, "gg")  # Ensure it's a ggplot object
  
  # Extract the plot's scales using ggplot_build
  plot_build <- ggplot2::ggplot_build(plot_result)
  
  # Check if the color 'red' is found in the computed geometry of layer 2
  green_line_found <- "#168C81" %in% plot_build$plot$layers[[2]]$computed_geom_params$colour
  
  # Expect that the red line is found
  expect_true(green_line_found)
  
  # Warning if highlight and a color palette
  expect_message(plot_timeseries(data, var = "value", time = "time", area = "area", 
                                 highlight = "a", palette = "IDE1"),
                 "A color ramp was selected for highlight. ")
})


# Test case: facet = TRUE for multiple areas
test_that("Correct handling for facet = TRUE", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-01-01', '2025-01-02')), 
                     value = c(1,2,3,1), 
                     area = rep(letters[1:2], each = 2))
  
  plot_result <- plot_timeseries(data, var = "value", time = "time",
                                 area = "area", facet = TRUE)
  
  # Extract the plot's scales using ggplot_build
  plot_build <- ggplot2::ggplot_build(plot_result)
  
  facet_info <- plot_build$layout$facet$params$facets
  
  # Check if facets are present in the plot
  expect_true(!is.null(facet_info), "Facets should be present in the plot")
})


# Option 2. If the variable to be plotted is cases ----
test_that("Error when 'var' (cases) is missing or incorrect", {
  
  # Create a mock dataframe with no 'cases' column
  data <- data.frame(time = as.Date("2022-01-01"), pop = 1000)
  
  # Expect an error when 'cases' argument is missing
  expect_error(plot_timeseries(data = data, time = "time", type = "counts",
                               var = "cases"),
               "No column of the data matches the 'var' argument")
  
  # Create a mock dataframe with 'cases' as a non-numeric column
  data <- data.frame(time = as.Date("2022-01-01"), cases = "non_numeric", pop = 1000)
  
  # Expect an error when 'cases' is not numeric
  expect_error(plot_timeseries(data = data, time = "time", var = "cases",
                               pop = "pop", type = "counts"),
               "'var' should be numeric")
})


test_that("Error when 'pop' is missing for 'incidence'", {
  data <- data.frame(time = as.Date("2022-01-01"), cases = 100)
  
  # Expect error when 'pop' is missing for 'incidence' type
  expect_error(plot_timeseries(data = data, time = "time", var = "cases",
                               type = "inc"), 
               "pop' required if type = 'inc'")
})


test_that("Error when duplicated time units are present in single time series", {
  data <- data.frame(time = as.Date(c("2022-01-01", "2022-01-01")),
                     cases = c(100, 150), pop = c(1000, 1000))
  
  # Expect error when duplicated time units are detected
  expect_error(plot_timeseries(data = data, time = "time", var = "cases", 
                               type="counts", pop = "pop"),
               "Duplicated time units detected")
})


test_that("Error when 'area' column is missing", {
  data <- data.frame(time = as.Date("2022-01-01"), cases = 100, pop = 1000)
  
  # Expect error when area is specified but column doesn't exist
  expect_error(plot_timeseries(data = data, time = "time", var = "cases",
                               pop = "pop", area = "area", type = "counts"),
               "No column of the data matches the 'area' argument")
})


test_that("Correct time aggregation based on 'aggregate_time' - month", {
  data<- data.frame(
    time = seq.Date(as.Date("2021-01-01"), as.Date("2021-02-28"), "week"),
    cases = 1, pop = 100
  )
  
  # Test for monthly aggregation
  result <- plot_timeseries(data = data, time = "time", var = "cases",
                            pop = "pop", aggregate_time = "month", type = "counts")
  expect_equal(length(unique(result$data$time)), 2)  # Should have 2 unique months
})


test_that("Correct time aggregation based on 'aggregate_time' - week", {
  data<- data.frame(
    time = as.Date(c(
      "2021-01-04","2021-01-05","2021-01-06","2021-01-07",
      "2021-01-08", "2021-01-09","2021-01-10","2021-01-11")),
    cases = rep(1:2, each=2), pop = rep(1000, 8)
  )
  
  result <- plot_timeseries(data, var = "cases", type="counts",
                            pop = "pop", time = "time",
                            aggregate_time = "week")
  
  expect_equal(length(unique(result$data$time)), 2)  # Should have 2 unique months
})

test_that("facets adjust properly based on area", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-01-01', '2025-01-02')), 
                     cases= c(1,2,3,1), pop = c(1000, 1000, 1000, 1000),
                     area = rep(letters[1:2], each = 2))
  
  result <- plot_timeseries(data = data, time = "time", var = "cases",
                            area = "area", type = "counts", facet = TRUE)
  
  # Extract the plot's scales using ggplot_build
  plot_build <- ggplot2::ggplot_build(result)
  
  facet_info <- plot_build$layout$facet$params$facets
  
  # Check if facets are present in the plot
  expect_true(!is.null(facet_info), "Facets should be present in the plot")
  
  
})


test_that("Correct plot type is returned for 'counts' and 'incidence'", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-01-01', '2025-01-02')), 
                     cases= c(1,2,3,1), pop = c(1000, 1000, 1000, 1000),
                     area = rep(letters[1:2], each = 2))
  
  result <- plot_timeseries(data = data, time = "time", var = "cases",
                            area = "area",
                            pop = "pop", type = "counts")
  expect_true("gg" %in% class(result))  # Should return a ggplot object
  
  result <- plot_timeseries(data = data, time = "time", var = "cases",
                            pop = "pop", type = "inc", area = "area")
  expect_true("gg" %in% class(result))  # Should return a ggplot object
  
  result <- plot_timeseries(data = data, time = "time", var = "cases",
                            pop = "pop", type = "inc", area = "area",
                            transform = "sqrt")
  expect_true("gg" %in% class(result))  # Should return a ggplot object
  
  result <- plot_timeseries(data = data, time = "time", var = "cases",
                            pop = "pop", type = "inc", area = "area",
                            transform = "log10p1")
  expect_true("gg" %in% class(result))  # Should return a ggplot object
})

test_that("Palette message when there is no grouping", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-01-03', '2025-01-04')), 
                     cases= c(1,2,3,1), pop = c(1000, 1000, 1000, 1000),
                     area = rep(letters[1:2], each = 2))
  expect_message(plot_timeseries(data = data, time = "time", var = "cases",
                                 pop = "pop", type = "counts", 
                                 palette = "IDE1"),
                 paste0("A color ramp was selected for a single time series. ", 
                        "Please select a single color to have more control."))
  
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-01-01', '2025-01-02')), 
                     cases= c(1,2,3,1), pop = c(1000, 1000, 1000, 1000),
                     area = rep(letters[1:2], each = 2))
  expect_message(plot_timeseries(data = data, time = "time", var = "cases",
                                 pop = "pop", type = "counts", area = "area",
                                 facet = TRUE, palette = "IDE1"),
                 paste0("A color ramp was selected for a single time series. ", 
                        "Please select a single color to have more control."))})