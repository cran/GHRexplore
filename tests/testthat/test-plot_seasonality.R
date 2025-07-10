
test_that("plot_seasonality handles missing or invalid data", {
  data <- data.frame(time = as.Date('2025-01-01'),
                     cases = 10, pop = 1000, area = 'A')
  
  # Test missing 'data'
  expect_error(plot_seasonality(), "Error: Missing required argument 'data'")
  
  # Test invalid 'data' type (e.g., not a data.frame)
  expect_error(plot_seasonality(data = list(time = as.Date('2025-01-01'), cases = 10)),
               "'data' should be a data.frame")
  
  # Test missing 'time'
  expect_error(plot_seasonality(data = data), "Error: Missing required argument 'var'")
  
  # Test missing 'time'
  expect_error(plot_seasonality(data = data, var = "cases", 
                                time = "time", type = "invalid"), 
               "type must be either 'cov', 'counts' or 'inc'")
})


test_that("Apropriate facets for area", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-01-01', '2025-01-02')), 
                     var = c(1,2,3,1), 
                     area = rep(letters[1:2], each = 2))
  
  plot_result <- plot_seasonality(data = data,
                                  var = "var",
                                  time = "time",
                                  area = "area")
  
  # Extract the plot's build components using ggplot_build
  plot_build <- ggplot2::ggplot_build(plot_result)
  
  # Test if facets are used correctly for multiple areas
  facet_info <- plot_build$layout$facet$params$facets$area
  # 
  expect_true(!is.null(facet_info), "Area facets should be present in the plot")
  
})


test_that("plot_seasonality aggregates data correctly", {
  # Simulate monthly data
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-02-01',
                                      '2025-01-01', '2025-02-01')), 
                     var = c(1,2,3,1), 
                     area = rep(letters[1:2], each = 2))
  
  # Test aggregation by month
  plot_result <- plot_seasonality(data = data, var = "var", time = "time",
                                  area = "area", aggregate_time = "month")
  
  expect_equal(length(unique(plot_result$data$time)), 2)
  
})

test_that("plot_seasonality detects inappropriate fun for type!='cov'", {
  
  # Simulate monthly data
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-02-01',
                                      '2025-01-01', '2025-02-01')), 
                     var = c(1,2,3,1), 
                     area = rep(letters[1:2], each = 2))
  
  expect_warning(plot_seasonality(data = data, var = "var", time = "time",
                                  type = "counts",
                                  area = "area", aggregate_time = "month",
                                  aggregate_time_fun = "sum"),
                 "aggregate_time_fun' for case counts and incidence rates"
                 
  )
})

test_that("Error when population is missing for incidence", {
  data <- data.frame(time = as.Date('2025-01-01'),
                     cases = 10, pop = NA, area = 'A')
  
  # Test for missing population with incidence type
  expect_error(plot_seasonality(data = data, var = "cases", time = "time", type = "inc"),
               "'pop' required if type = 'inc'")
})

# Test case: 'transform' has invalid function
test_that("valid transform", {
  data <- data.frame(date = as.Date("2025-01-01"), cases = 10)
  expect_error(plot_seasonality(data = data, var = "cases", time = "date",
                                aggregate_time = "month", transform = "wrong"),
               "Invalid transform.")
})


test_that("Person-time subtitle is correctly displayed for incidence", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02', '2025-01-03',
                                      '2025-01-01', '2025-01-02', '2025-01-03')), 
                     cases = c(1,2,3,2,3,1), 
                     pop= rep(1000, 6),
                     area = rep(letters[1:2], each = 3))
  
  plot_result <- plot_seasonality(data = data, var = "cases", time = "time",
                                  area = "area", pop= "pop", type="inc",
                                  pt = 1000)
  plot_build <- ggplot2::ggplot_build(plot_result)
  
  # Check if subtitle contains person-time
  expect_true(plot_build$plot$labels$y == "Incidence (1,000 person-month)")
})

test_that("Valid transforms", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02', '2025-01-03',
                                      '2025-01-01', '2025-01-02', '2025-01-03')), 
                     cases = c(1,2,3,2,3,1), 
                     pop= rep(1000, 6),
                     area = rep(letters[1:2], each = 3))
  
  p <- plot_seasonality(data = data, var = "cases", time = "time",
                                  area = "area", pop= "pop", type="inc",
                                  pt = 1000)
  expect_true(ggplot2::is_ggplot(p))
  p <- plot_seasonality(data = data, var = "cases", time = "time",
                        area = "area", pop= "pop", type="inc",
                        pt = 1000, transform = "sqrt")
  expect_true(ggplot2::is_ggplot(p))
  p <- plot_seasonality(data = data, var = "cases", time = "time",
                        area = "area", pop= "pop", type="inc",
                        pt = 1000, transform = "log10p1")
  expect_true(ggplot2::is_ggplot(p))
})
