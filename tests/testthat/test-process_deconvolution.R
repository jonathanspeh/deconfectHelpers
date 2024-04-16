test_that("process_cellanneal turns cellanneal output in long format", {
  test_data <- readRDS("testdata/cellanneal_deconvolution.Rdata")
  expected <- readRDS("testdata/cellanneal_deconvolution_long.Rdata")
  expect_equal(process_cellanneal(test_data), expected)
  expect_error(process_cellanneal(mtcars), "provide valid cellanneal result")
  })
