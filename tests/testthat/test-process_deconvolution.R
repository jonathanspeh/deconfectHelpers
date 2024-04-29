test_that("process_cellanneal turns cellanneal output in long format", {
  test_data <- readRDS("testdata/cellanneal_deconvolution.Rdata")
  expected <- readRDS("testdata/cellanneal_deconvolution_long.Rdata")
  expect_equal(process_cellanneal(test_data), expected)
  expect_error(process_cellanneal(mtcars), "provide valid cellanneal result")
  })


test_that("can standardise celltypes", {
          cells <- c("b cells", "bcell", "b_cell", "Naive B Cell",
                     "cd4", "T cellcD4", "cd8", "cytotoxic T lymphocyte",
                     "neutro", "mono", "cdc")

          standardised <- c("B_cells","B_cells","B_cells", "B_cells",
                            "T_cells_CD4", "T_cells_CD4", "T_cells_CD8", "T_cells_CD8",
                            "Neutrophils", "Monocytes", "conventional_DCs")
          expect_equal(fix_cell_names(cells), standardised) })


