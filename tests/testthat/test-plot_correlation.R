test_that("error if data is not a data.frame", {
  expect_error(plot_correlation(data = list(a = 1), var = "a"), 
               "'data' should be a 'data.frame'")
})

test_that("error if variables are not numeric", {
  df <- data.frame(num1 = rnorm(10), cat1 = letters[1:10])
  expect_error(plot_correlation(df, var = c("num1", "cat1")), "variables should be numeric")
})

test_that("returns a ggplot object for valid inputs", {
  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  result <- plot_correlation(df, var = c("a", "b", "c"))
  expect_s3_class(result, "ggplot")
})

test_that("uses var_label when provided", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  expect_no_error(plot_correlation(df, var = c("a", "b"), var_label = c("Var A", "Var B")))
})

test_that("handles custom title correctly", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  plot <- plot_correlation(df, var = c("a", "b"), title = "Test Title")
  expect_s3_class(plot, "ggplot")
})

test_that("handles palette argument", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  expect_no_error(plot_correlation(df, var = c("a", "b"), palette = "Rocket"))
})

test_that("works with different correlation methods", {
  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  expect_no_error(plot_correlation(df, var = c("a", "b", "c"), method = "pearson"))
  expect_no_error(plot_correlation(df, var = c("a", "b", "c"), method = "spearman"))
  expect_no_error(plot_correlation(df, var = c("a", "b", "c"), method = "kendall"))
})

test_that("prints correlation matrix if print = TRUE", {
  df <- data.frame(a = 1:10, b = 10:1)
  expect_output(
    plot_correlation(df, var = c("a", "b"), print = TRUE)
  )
})

test_that("handles NA values in data", {
  df <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA))
  expect_no_error(plot_correlation(df, var = c("a", "b")))
})

test_that("error for invalid plot_type", {
  df <- data.frame(num1 = rnorm(10), num2 = rnorm(10))
  expect_error(plot_correlation(df, var = c("num1", "num2"), plot_type = 1),
               "plot_type must be a character vector of length 2")
  expect_error(plot_correlation(df, var = c("num1", "num2"), plot_type = c(1,"circle")),
               "Accepted plot_type include: 'raster', 'number', 'circle'")
})


test_that("error for invalid plot_type", {
  df <- data.frame(num1 = rnorm(10), num2 = rnorm(10))
  result <- plot_correlation(df, var = c("num1", "num2"), 
                             plot_type = c("raster", "circle"))
  expect_s3_class(result, "ggplot")
  result <- plot_correlation(df, var = c("num1", "num2"), 
                             plot_type = c("raster", "number"))
  expect_s3_class(result, "ggplot")
  result <- plot_correlation(df, var = c("num1", "num2"), 
                             plot_type = c("number", "circle"))
  expect_s3_class(result, "ggplot")
})

