library(testthat)
library(ggplot2)

df <- data.frame(
  date = as.Date("2020-01-01") + 0:9,
  x = rnorm(10),
  y = rnorm(10),
  cases = 1:10,
  pop = rep(1000, 10),
  area = rep(c("A","B"), each = 5),
  group = rep(c("G1","G2"), each = 5)
)

test_that("basic input validation works", {
  expect_error(plot_timeseries2(), "Missing required argument 'data'")
  expect_error(plot_timeseries2(data = df), "Missing required argument 'var'")
  expect_error(plot_timeseries2(data = df, var=c("x","y")), "Missing required argument 'time'")
  expect_error(plot_timeseries2(data = "notdf", var=c("x","y"), time="date"), "should be a data.frame")
  expect_error(plot_timeseries2(df, var="x", time="date"), "'var' must have length 2")
  expect_error(plot_timeseries2(df, var=c("x","notacol"), time="date"), "do not include the 'var'")
  expect_error(plot_timeseries2(df, var=c("x","y"), time="date", type=c("bad","cov")), "type must be")
  expect_error(plot_timeseries2(df, var=c("x","y"), time="date", align="bad"), "'align' must")
  expect_error(plot_timeseries2(df, var=c("cases","x"), time="date", type=c("inc","cov")), "'pop' required")
})

test_that("plot is returned for valid input types", {
  p <- plot_timeseries2(df, var=c("x","y"), time="date")
  expect_s3_class(p, "ggplot")
})

test_that("incidence requires population + pt", {
  p1 <- plot_timeseries2(df, var=c("cases","y"), type=c("inc","cov"), 
                        pop="pop", time="date")
  expect_s3_class(p1, "ggplot")
  p2 <- plot_timeseries2(df, var=c("cases","y"), type=c("inc","cov"),
                        pop="pop", time="date", pt = 1e+05*2)
  expect_s3_class(p2, "ggplot")  
  expect_equal(p1$data$inc1 / p2$data$inc1,  rep(0.5, 10))
})

test_that("temporal aggregation works", {
  p <- plot_timeseries2(df, var=c("cases","y"), type=c("counts", "cov"), 
                        time="date", aggregate_time="month")
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$plot_var1, sum(1:10))
  expect_equal(p$data$plot_var2, mean(df$y))
})

test_that("spatial aggregation works and facets", {
  p <- plot_timeseries2(df, var=c("cases","y"), type=c("counts","cov"), 
                        time="date", area="area", aggregate_space="group")
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
  expect_false(p$facet$params$free$y) 
  p <- plot_timeseries2(df, var=c("cases","y"), type=c("counts","cov"), 
                        time="date", area="area", aggregate_space="group",
                        free_y_scale = TRUE)
  expect_true(p$facet$params$free$y) 
})

test_that("spatiotemporal aggregation works", {
  p <- plot_timeseries2(df, var=c("cases","y"), type=c("counts","cov"), 
                        time="date", area="area", aggregate_space="group",
                        aggregate_time="month")
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$plot_var1, c(sum(1:5), sum(6:10)))
})
  
test_that("custom labels and legend work", {
  p <- plot_timeseries2(df, var=c("cases","y"), type=c("inc","cov"), 
                        time="date", pop="pop", 
                        var_label=c("Incid","Covariate"), 
                        legend="CustomLegend", 
                        ylab=c("Left","Right"), xlab="Time axis", 
                        title="My Title")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "My Title")
  expect_equal(p$labels$x, "Time axis")
  expect_equal(p$labels$colour, "CustomLegend")
})

test_that("the different alignment possibilities work", {
  p <- plot_timeseries2(df, var=c("x","y"), time="date", align = "min")
  expect_s3_class(p, "ggplot")
  p <- plot_timeseries2(df, var=c("x","y"), time="date", align = "mean")
  expect_s3_class(p, "ggplot")
  p <- plot_timeseries2(df, var=c("x","y"), time="date", align = "median")
  expect_s3_class(p, "ggplot")
})

test_that("palette and alpha work", {
  p <- plot_timeseries2(df, var=c("x","y"), time="date",
                        palette = c("red", "blue"), alpha = 0.1)

  expect_equal(p$layers[[1]]$aes_params$alpha, 0.1)
  expect_equal(p$scales$scales[[2]]$palette(1), c("red", "blue"), ignore_attr = TRUE)
})