test_that("plot_compare returns a ggplot object for valid inputs", {
  data("dengue_MS")
  
  # Time series
  p1 <- plot_compare(
    plot_function = plot_timeseries,
    data = dengue_MS,
    var = c("pdsi", "tmax"),
    type = c("cov", "cov"),
    time = "date",
    area = "micro_code",
    var_label = c("PDSI", "Max Temp"),
    combine_legend = FALSE,
    ncol = 1)
  
  # Heatmap
  p2 <- plot_compare(
    plot_function = plot_heatmap,
    data = dengue_MS,
    var = c("pdsi", "tmax"),
    type = c("cov", "cov"),
    time = "date",
    area = "micro_code",
    var_label = c("PDSI", "Max Temp"),
    combine_legend = FALSE,
    ncol = 1)
  
  # Seasonality
  p3 <- plot_compare(
    plot_function = plot_seasonality,
    data = dengue_MS,
    var = c("pdsi", "tmax"),
    type = c("cov", "cov"),
    time = "date",
    area = "micro_code",
    var_label = c("PDSI", "Max Temp"),
    combine_legend = FALSE,
    ncol = 1)
  
  expect_s3_class(p1, "gg")
  expect_s3_class(p2, "gg")
  expect_s3_class(p3, "gg")
})

test_that("plot_compare throws error for unnamed arguments", {
  data("dengue_MS")
  
  expect_error(
    plot_compare(
      plot_function = plot_timeseries,
      data = dengue_MS,
      var = c("pdsi"),
      type = c("cov"),
      "time"),
    "All additional arguments must be named"
  )
})

test_that("plot_compare validates input plot_function", {
  data("dengue_MS")
  
  expect_error(
    plot_compare(
      plot_function = "bad_function",
      data = dengue_MS,
      var = c("pdsi"),
      type = c("cov"),
      time = "date",
      area = "micro_code"
    ),
    "Invalid plot function"
  )
})

test_that("plot_compare validates input data type", {
  expect_error(
    plot_compare(
      plot_function = plot_timeseries,
      data = "not_a_df",
      var = c("pdsi"),
      type = c("cov"),
      time = "date",
      area = "micro_code"
    ),
    "'data' must be a data.frame"
  )
})

test_that("plot_compare checks missing variables in data", {
  data("dengue_MS")
  
  expect_error(
    plot_compare(
      plot_function = plot_timeseries,
      data = dengue_MS,
      var = c("missing_var"),
      type = c("cov"),
      time = "date",
      area = "micro_code"
    ),
    "The following variables are missing"
  )
})

test_that("plot_compare errors when combine_legend is TRUE for heatmap or map", {
  data("dengue_MS")
  data("map_MS")
  
  expect_error(
    plot_compare(
      plot_function = plot_heatmap,
      data = dengue_MS,
      var = c("pdsi", "tmax"),
      type = c("cov", "cov"),
      time = "date",
      area = "micro_code",
      palette = c("Blues", "Reds"),
      combine_legend = TRUE
    ),
    "Legends of heatmap or map plots cannot be combined"
  )
})

test_that("plot_compare errors when palette is a vector and combine_legend = TRUE", {
  data("dengue_MS")
  
  expect_error(
    plot_compare(
      plot_function = plot_seasonality,
      data = dengue_MS,
      var = c("pdsi", "tmax"),
      type = c("cov", "cov"),
      time = "date",
      area = "micro_code",
      palette = c("Blues", "Reds"),
      combine_legend = TRUE
    ),
    "Legends of plots with different palettes cannot be combined"
  )
})
