
# Sample data for testing
testdata <- data.frame(
  time = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01", 
                   "2024-01-02", "2024-01-02", "2024-01-02")),
  area = c("50001", "50002", "50003",
           "50001", "50002", "50003"),
  cat_char = LETTERS[1:6],
  cat_factor = as.factor(LETTERS[1:6]),
  cases = c(10, 20, 30,
            10, 20, 30),
  pop = c(1000, 2000, 3000,1000, 2000, 3000)
)


# import a simple sf object for testing
requireNamespace("sf", quietly = TRUE)
data("map_MS")
map <- map_MS |> 
  dplyr::filter(code %in% c("50001", "50002", "50003"))

# Test cases
test_that("plot_map handles missing data argument", {
  expect_error(plot_map(), "Missing required argument 'data'")
})

test_that("plot_map requires a data.frame", {
  expect_error(plot_map(data = list()), "'data' should be a data.frame")
})

test_that("plot_map requires a valid var column", {
  expect_error(plot_map(testdata, var = "invalid"),
               "No column of the data matches the 'var' argument")
})

test_that("incorrect type", {
  expect_error(plot_seasonality(data = testdata, var = "cases", 
                                time = "time", type = "invalid"), 
               "type must be either 'cov', 'counts' or 'inc'")
})

test_that("plot_map supports faceting by year", {
  expect_silent(plot_map(testdata,
                         var = "cases",
                         time = "time",
                         area = "area",
                         map = map,         # the sf object for the map 
                         map_area = "code",    # Variable defining the area in the sf object
                         palette ="Reds", 
                         by_year = TRUE))
})

test_that("plot_map requires time column to be in date format when by_year= TRUE", {
  testdata_wrongtime <- testdata
  # Convert to incorrect format
  testdata_wrongtime$time <- as.character(format.Date(testdata$time, "%d-%m-%Y"))  
  expect_error(plot_map(testdata_wrongtime,
                        var = "cases",
                        time = "time",
                        area = "area",
                        map = map,
                        map_area= "code", 
                        by_year = TRUE),
               "'Date' should be in 'yyyy-mm-dd' format", fixed = TRUE)
})

test_that("plot_map requires a valid area column", {
  expect_error(plot_map(testdata, var = "cases", time = "time", area = "invalid"),
               "No column of the data matches the 'area' argument", fixed = TRUE)
})

test_that("plot_map requires a valid map argument", {
  expect_error(plot_map(testdata, var = "cases", time = "time", area = "area"), 
               "Missing required argument 'map'")
})

test_that("wrong type", {
  expect_error(plot_map(testdata, var = "cases", type="wrong", time = "time",
                        area = "area", map = map),
               "type must be either 'cov', 'counts' or 'inc'")
})

test_that("valid transform", {
  expect_error(plot_map(testdata, var = "cases", time = "time",
                        area = "area", map = map, transform = "wrong"),
               "Invalid transform.")
})

test_that("plot_map returns a ggplot object for counts", {
  
  p <- plot_map(data = testdata, 
                var = "cases",         # Variable to be plotted 
                type = "counts",
                time = "time",        # Variable defining the date "dd-mm-yyyy"
                area = "area",  # Variable defining area in the dataframe
                map = map,         # the sf object for the map 
                map_area = "code",    # Variable defining the area in the sf object
                palette ="Viridis")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot_map returns a ggplot object for inc", {
  
  p <- plot_map(data = testdata, 
                var = "cases",         # Variable to be plotted 
                type = "inc",
                time = "time",        # Variable defining the date "dd-mm-yyyy"
                area = "area",  # Variable defining area in the dataframe
                pop = "pop",
                map = map,         # the sf object for the map 
                map_area = "code",    # Variable defining the area in the sf object
                palette ="Sunset")
  
  expect_s3_class(p, "ggplot")
})

test_that("plot_map returns a ggplot object when var is provided", {
  testdata$var <- c(1.5, 2.5, 3.5, 5, 2, 3)  # Add a numeric variable
  p <- plot_map(testdata,
                time = "time",
                area = "area",
                var = "var",
                map = map,         
                map_area = "code",   
                palette ="Purp")
  expect_s3_class(p, "ggplot")
})


test_that("plot_map handles missing area matches gracefully", {
  testdata_invalid <- testdata
  testdata_invalid[c(1,4),"area"] <- 9999
  expect_warning(plot_map(testdata_invalid,
                          var = "cases",
                          time = "time",
                          area = "area",
                          map = map,       
                          map_area = "code"),
                 "Some IDs do not match between map and data")
})

test_that("plot_map stops when var column is missing", {
  testdata_missing_var <- testdata |> select(-cases)
  expect_error(plot_map(testdata_missing_var,
                        var = "cases",
                        time = "time",
                        area = "area",
                        map = map,         # the sf object for the map 
                        map_area = "code"), 
               "No column of the data matches the 'var' argument")
})

test_that("plot_map handles NA values in variable column", {
  testdata_na <- testdata
  testdata_na$cases[1] <- NA
  expect_warning(plot_map(testdata_na,
                          var = "cases",
                          time = "time",
                          area = "area",
                          map = map,         # the sf object for the map 
                          map_area = "code"),
                 "Missing values found in the ' cases ' column")
})

test_that("plot_map works when all values in var column are zero", {
  testdata_zeros <- testdata |> mutate(cases = 0)
  expect_silent(plot_map(testdata_zeros,
                         var = "cases",
                         time = "time",
                         area = "area",
                         map = map,         # the sf object for the map 
                         map_area = "code",    # Variable defining the area in the sf object
                         palette ="Greys"))
})


test_that("bins", {
  expect_error(plot_map(testdata,
                         var = "cases",
                         time = "time",
                         area = "area",
                         map = map,         
                         map_area = "code",    
                         bins = 0.5),
               "bins must be an integer.")
  expect_error(plot_map(testdata,
                        var = "cases",
                        time = "time",
                        area = "area",
                        map = map,         
                        map_area = "code",    
                        bins = 3,
                        bins_method = "wrong"),
               "bins_method must be 'quantile' or 'equal'.")
  expect_error(plot_map(testdata,
                        var = "cases",
                        time = "time",
                        area = "area",
                        map = map,         
                        map_area = "code",    
                        bins = 3,
                        bins_method = "quantile",
                        bins_label = c("A", "B")),
               "The length of bins_label must be equal to the number of bins.")
  expect_silent(plot_map(testdata,
                        var = "cases",
                        time = "time",
                        area = "area",
                        map = map,         
                        map_area = "code",    
                        bins = 3,
                        bins_method = "equal",
                        bins_label = c("A", "B", "C")))
})


test_that("Categorical data", {
  p1 <- plot_map(testdata,
                 var = "cat_char",
                 time = "time",
                 area = "area",
                 map = map,        
                 map_area = "code",   
                 palette ="Greys")
  p2 <- plot_map(testdata,
                 var = "cat_factor",
                 time = "time",
                 area = "area",
                 map = map,        
                 map_area = "code",   
                 palette ="Greys")  
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  
})

test_that("Transforms", {
  expect_error(plot_map(testdata,
                        var = "cases",
                        time = "time",
                        area = "area",
                        map = map,        
                        map_area = "code",   
                        palette ="Greys",
                        transform = "log10p1"),
               "Transform log10p1 is only available")
  p1 <- plot_map(testdata,
                 var = "cases",
                 type = "counts",
                 pop = "pop",
                 time = "time",
                 area = "area",
                 map = map,        
                 map_area = "code",   
                 palette ="Greys",
                 transform = "log10p1")
  p2 <- plot_map(testdata,
                 var = "cases",
                 type = "inc",
                 pop = "pop",
                 time = "time",
                 area = "area",
                 map = map,        
                 map_area = "code",   
                 palette ="Greys",
                 transform = "log1p")  
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  
})

test_that("Centering", {
  p1 <- plot_map(testdata, var = "cases",
                 time = "time", area = "area", map = map,  map_area = "code",
                 centering = 25)
  p2 <- plot_map(testdata, var = "cases",
                 time = "time", area = "area", map = map,  map_area = "code",
                 centering = "median")
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_error(plot_map(testdata, var = "cases",
                        time = "time", area = "area", map = map,  map_area = "code",
                        centering = 0),
               "The centering value must be")
  expect_error(plot_map(testdata, var = "cases",
                        time = "time", area = "area", map = map,  map_area = "code",
                        centering = 100),
               "The centering value must be")
  
})


