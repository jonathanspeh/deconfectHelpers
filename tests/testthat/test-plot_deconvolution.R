testdata <- readRDS("testdata/cellanneal_deconvolution_long.Rdata")


test_that("plots without error", {
  expect_no_error(plot_deconvolution_violin(testdata, testdata))
  expect_no_error(plot_deconvolution_celltype_corrs(testdata, testdata))
  expect_no_error(plot_deconvolution_celltype_corrs(testdata, testdata, add_x_y_line = TRUE))
  expect_no_error(plot_deconvolution_celltype_corrs(testdata, testdata, add_x_y_line = FALSE))
  expect_no_error(plot_deconvolution_corrs(testdata, testdata, add_x_y_line = TRUE))
  expect_no_error(plot_deconvolution_corrs(testdata, testdata, add_x_y_line = FALSE))
  expect_no_error(plot_deconvolution_corrs(testdata, testdata, add_x_y_line = TRUE, add_metrics = TRUE))
  expect_equal(length(plot_deconvolution_celltype_corrs(testdata, testdata)), 8)
})
