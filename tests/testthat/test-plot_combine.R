# sample plots for testing
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

test_that("plot_list must be a list of ggplot objects", {
  expect_error(plot_combine("not_a_list"), "plot_list must be a list")
  expect_error(plot_combine(list(1, "text")), "All elements in plot_list must be ggplot objects")
})

test_that("logical parameters must be logical", {
  expect_error(plot_combine(plots, combine_legend = "yes"), 
               "combine_legend must be a logical value")
  expect_error(plot_combine(plots, combine_xaxis = "no"),
               "combine_xaxis must be a logical value")
})

test_that("ncol and align validation works", {
  expect_error(plot_combine(plots, ncol = 0), "ncol must be a positive integer")
  expect_error(plot_combine(plots, align = "z"), "align must be one of")
})

test_that("legend layout params are validated", {
  expect_error(plot_combine(plots, combine_legend = TRUE, ncol_l = 0), 
               "ncol_l must be a positive integer")
  expect_error(plot_combine(plots, combine_legend = TRUE, nrow_l = -1),
               "nrow_l must be a positive integer or NULL")
  expect_error(plot_combine(plots, combine_legend = TRUE, rel_widths_l = c(-1, 1)), 
               "rel_widths_l must be a numeric vector")
  expect_error(plot_combine(plots, combine_legend = TRUE, rel_heights_l = c(1)),
               "rel_heights_l must be a numeric vector of length 2")
})

test_that("combine_xaxis = TRUE only allowed with ncol = 1", {
  expect_error(plot_combine(plots, combine_xaxis = TRUE, ncol = 2),
               "only use combine_xaxis = TRUE when plots aligned in 1 column")
})

test_that("plot_combine returns a ggplot grob", {
  result <- plot_combine(plots)
  expect_s3_class(result, "gg")
})

test_that("plot_combine works with combine_xaxis = TRUE", {
  result <- plot_combine(plots, combine_xaxis = TRUE)
  expect_s3_class(result, "gg")
})

test_that("plot_combine works with combine_legend = TRUE", {
  result <- plot_combine(plots, combine_legend = TRUE)
  expect_s3_class(result, "gg")
})

test_that("plot_combine with both combine_legend and combine_xaxis = TRUE", {
  result <- plot_combine(plots,
                         combine_legend = TRUE,
                         combine_xaxis = TRUE,
                         rel_widths_l = c(2, 1),
                         rel_heights_l = c(1, 1))
  expect_s3_class(result, "gg")
})
