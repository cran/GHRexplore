
test_that("GHR_palette returns a function", {
  expect_true(is.function(GHR_palette("IDE1")))
})

test_that("GHR_palette rejects invalid palettes", {
  expect_error(GHR_palette("InvalidPalette"),
               "The selected palette is invalid.")
})

test_that("GHR_palette generates correct number of colors", {
  colors <- GHR_palette(palette = "Purp", 3)(3)
  expect_equal(length(colors), 3)
})

test_that("GHR_palette handles GHR palettes", {
  expect_silent(GHR_palette(palette = "Colorblind", 5))
  expect_silent(GHR_palette(palette = "-Colorblind"))
  expect_silent(GHR_palette(palette = "IDE1"))
  expect_silent(GHR_palette(palette = "Purp"))
  expect_silent(GHR_palette(palette = "BlYlRd"))
  expect_silent(GHR_palette(palette = "Green"))
  expect_silent(GHR_palette(palette = "IDE2"))
  expect_silent(GHR_palette(palette = "Colorblind"))
})

test_that("GHR_palette with RColorBrewer", {
  expect_silent(GHR_palette(palette = "Reds", 5))
  expect_silent(GHR_palette(palette = "-Reds"))
})

test_that("GHR_palette with colorspace", {
  expect_silent(GHR_palette(palette = "Rocket", 5))
  expect_silent(GHR_palette(palette = "-Rocket"))
})

test_that("GHR_palette with single R color", {
  expect_silent(GHR_palette(palette = "skyblue", 5))
  expect_silent(GHR_palette(palette = "-skyblue"))
})

test_that("GHR_palette with single hex color", {
  expect_silent(GHR_palette(palette = "#03fcca", 5))
  expect_silent(GHR_palette(palette = "-#03fcca"))
  
})

test_that("GHR_palette with vector of colors", {
  expect_silent(GHR_palette(palette = c("#03fcca", "red"), 5))
})
