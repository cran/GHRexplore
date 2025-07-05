test_that("error if var is not length 2 or doesn't exist in data", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
  expect_error(plot_bivariate(df, var = "a"), "'var' is required and must be a vector with exactly 2 elements")
  expect_error(plot_bivariate(df, var = c("a", "z")), "One or more elements in 'var' are not found in 'data'")
})

test_that("error if var_label is not of length 2", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(plot_bivariate(df, var = c("a", "b"), var_label = "label"),
               "'var_label' must be a vector with exactly 2 elements corresponding to the variables in the 'var' vector.")
})

test_that("error if data is not a data.frame", {
  expect_error(plot_bivariate(data = list(a = 1), var = c("a", "b")), "data.*data.frame")
})

test_that("error if both variables are categorical", {
  df <- data.frame(cat1 = as.factor(c("a", "b", "c")), cat2 = as.factor(c("x", "y", "z")))
  expect_error(plot_bivariate(df, var = c("cat1", "cat2")), 
               "Only one categorical variable is allowed")
})

test_that("error if area column does not exist", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(plot_bivariate(df, var = c("a", "b"), area = "fake_area"), 
               "No column of the data matches the 'area' argument")
})

test_that("returns ggplot object for numeric variables", {
  df <- data.frame(a = rnorm(100), b = runif(100))
  result <- plot_bivariate(df, var = c("a", "b"))
  expect_s3_class(result, "ggplot")
})

test_that("returns ggplot object with factor + numeric (boxplot)", {
  df <- data.frame(group = factor(rep(c("A", "B"), 5)), value = rnorm(10))
  result <- plot_bivariate(df, var = c("group", "value"))
  expect_s3_class(result, "ggplot")
})

test_that("works with area grouping", {
  df <- data.frame(a = rnorm(20), b = rnorm(20), region = rep(c("X", "Y"), 10))
  p <- plot_bivariate(df, var = c("a", "b"), area = "region")
  expect_s3_class(p, "ggplot")
})

test_that("works with facets", {
  df <- data.frame(a = rnorm(20), b = rnorm(20), region = rep(c("X", "Y"), 10))
  p <- plot_bivariate(df, var = c("a", "b"), area = "region", facet = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("free_x_scale and free_y_scale work", {
  df <- data.frame(a = rnorm(20), b = rnorm(20), region = rep(c("X", "Y"), 10))
  expect_s3_class(plot_bivariate(df, var = c("a", "b"), area = "region", facet = TRUE, free_x_scale = TRUE), "ggplot")
  expect_s3_class(plot_bivariate(df, var = c("a", "b"), area = "region", facet = TRUE, free_y_scale = TRUE), "ggplot")
  expect_s3_class(plot_bivariate(df, var = c("a", "b"), area = "region", facet = TRUE, free_x_scale = TRUE, free_y_scale = TRUE), "ggplot")
})

test_that("custom title, labels, and legend are handled", {
  df <- data.frame(a = 1:10, b = 11:20, region = rep(c("A", "B"), 5))
  p <- plot_bivariate(df,
                      var = c("a", "b"),
                      area = "region",
                      title = "Custom Title",
                      var_label = c("Var A", "Var B"),
                      legend = "Region")
  expect_s3_class(p, "ggplot")
})

test_that("Color palette message", {
  df <- data.frame(a = rnorm(20), b = rnorm(20), region = rep(c("X", "Y"), 10))
  expect_message(plot_bivariate(data = df, var = c("a", "b"),
                                palette = "IDE1"),
                 paste0("A color ramp was selected for a single grouping. ", 
                        "Please select a single color to have more control."))
  expect_message(plot_bivariate(data = df, var = c("a", "b"), area = "region",
                                facet = TRUE, palette = "IDE1"),
                 paste0("A color ramp was selected for a single grouping. ", 
                        "Please select a single color to have more control."))
})
