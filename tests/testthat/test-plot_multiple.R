test_that("error if plot_function is not one of the allowed functions", {
  dummy_fun <- function() {}
  expect_error(plot_multiple("plot_bivariate"), "Invalid plot function")
  expect_error(plot_multiple(dummy_fun), "Unsupported plot function")
})

test_that("returns list of ggplots from plot_timeseries", {
  df <- data.frame(
    date = rep(seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 6), 2),
    region = c(rep("A", 6), rep("B", 6)),
    x = rnorm(12), y = rnorm(12), z = rnorm(12), pop = rnorm(12)/100
  )
  
  plots <- plot_multiple(
    plot_function = plot_timeseries,
    data = df,
    var = c("x", "y", "z"),
    type = c("cov", "counts", "inc"),
    pop = "pop",
    var_label = c("X var", "Y var", "Z var"),
    palette = c("Reds", "Blues", "Greens"),
    time = "date",
    area = "region"
  )
  
  expect_type(plots, "list")
  expect_length(plots, 3)
  expect_s3_class(plots[[1]], "ggplot")
  expect_s3_class(plots[[2]], "ggplot")
  expect_s3_class(plots[[3]], "ggplot")
})

test_that("returns list of ggplots from plot_heatmap", {
  df <- data.frame(
    date = rep(seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 6), 2),
    region = c(rep("A", 6), rep("B", 6)),
    x = rnorm(12), y = rnorm(12), z = rnorm(12), pop = rnorm(12)/100
  )
  
  plots <- plot_multiple(
    plot_function = plot_heatmap,
    data = df,
    var = c("x", "y", "z"),
    type = c("cov", "counts", "inc"),
    pop = "pop",
    var_label = c("X var", "Y var", "Z var"),
    palette = c("Reds", "Blues", "Greens"),
    time = "date",
    area = "region"
  )
  
  expect_type(plots, "list")
  expect_length(plots, 3)
  expect_s3_class(plots[[1]], "ggplot")
  expect_s3_class(plots[[2]], "ggplot")
  expect_s3_class(plots[[3]], "ggplot")
})


test_that("returns list of ggplots from plot_seasonality", {
  df <- data.frame(
    date = rep(seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 6), 2),
    region = c(rep("A", 6), rep("B", 6)),
    x = rnorm(12), y = rnorm(12), z = rnorm(12), pop = rnorm(12)/100
  )
  
  plots <- plot_multiple(
    plot_function = plot_seasonality,
    data = df,
    var = c("x", "y", "z"),
    type = c("cov", "counts", "inc"),
    pop = "pop",
    var_label = c("X var", "Y var", "Z var"),
    palette = c("red", "blue", "green"),
    time = "date",
    area = "region",
  )
  
  expect_type(plots, "list")
  expect_length(plots, 3)
  expect_s3_class(plots[[1]], "ggplot")
  expect_s3_class(plots[[2]], "ggplot")
  expect_s3_class(plots[[3]], "ggplot")
})


test_that("returns list of ggplots from plot_map", {
  data("map_MS")
  map_df <- map_MS[1:2,]
  map_df$code <- c("A", "B")
  df <- data.frame(
    date = rep(seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 6), 2),
    region = c(rep("A", 6), rep("B", 6)),
    x = rnorm(12), y = rnorm(12), z = rnorm(12), pop = rnorm(12)/100
  )
  
  plots <- plot_multiple(
    plot_function = plot_map,
    data = df,
    map = map_df,
    var = c("x", "y", "z"),
    type = c("cov", "counts", "inc"),
    pop = "pop",
    var_label = c("X var", "Y var", "Z var"),
    palette = c("Reds", "Blues", "Greens"),
    time = "date",
    area = "region",
    map_area = "code",
  )
  
  expect_type(plots, "list")
  expect_length(plots, 3)
  expect_s3_class(plots[[1]], "ggplot")
  expect_s3_class(plots[[2]], "ggplot")
  expect_s3_class(plots[[3]], "ggplot")
})
