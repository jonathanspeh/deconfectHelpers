test_that("pivot_longer_deconvolution turns deconvolution output in long format", {
  test_data <- readRDS("testdata/cellanneal_deconvolution.Rdata")
  expected <- readRDS("testdata/cellanneal_deconvolution_long.Rdata")
  expect_equal(pivot_longer_deconvolution(test_data, fix_cell_names = FALSE), expected)
  expect_error(pivot_longer_deconvolution(mtcars), "provide valid deconvolution dataframe")
  })


test_that("can standardise celltypes", {
          cells <- c("b cells", "bcell", "b_cell", "Naive B Cell",
                     "cd4", "T cellcD4", "cd8", "cytotoxic T lymphocyte",
                     "neutro", "mono", "cdc")

          standardised <- c("B_cells","B_cells","B_cells", "B_cells",
                            "T_cells_CD4", "T_cells_CD4", "T_cells_CD8", "T_cells_CD8",
                            "Neutrophils", "Monocytes", "conventional_DCs")
          expect_equal(fix_cell_names(cells), standardised) })


test_that("can process wide deconvolution results", {
  testdata <- readRDS("testdata/deconvolution_immunedeconv.Rdata")
  expected <- readRDS("testdata/deconvolution_immunedeconv_long.Rdata")
  expect_equal(process_immunedeconv(testdata), expected)
})

