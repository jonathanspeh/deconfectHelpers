testdata <- readRDS("testdata/cellanneal_deconvolution_long.Rdata")


test_that("plots without error", {
  expect_no_error(plot_deconvolution_violin(testdata, testdata))
  expect_no_error(plot_deconvolution_celltype_corrs(testdata, testdata))
})
