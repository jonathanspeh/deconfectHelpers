test_that("run cellanneal works", {
  expected <- readRDS("testdata/cellanneal_result.Rdata")
  set.seed(42)
  actual <- run_cellanneal_r("testdata/signature.csv",
                             "testdata/mixture.csv")
  expect_equal(dim(actual), dim(expected))
  expect_equal(colnames(actual), colnames(expected))
  expect_equal(actual$sample, expected$sample)
})
