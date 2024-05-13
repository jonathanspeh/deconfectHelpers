test_that("pivot_longer_deconvolution turns deconvolution output in long format", {
  test_data <- readRDS("testdata/cellanneal_deconvolution.Rdata")
  expected <- readRDS("testdata/cellanneal_deconvolution_long.Rdata")
  expect_equal(pivot_longer_deconvolution(test_data), expected)
  expect_error(pivot_longer_deconvolution(mtcars), "provide valid deconvolution dataframe")
  })


test_that("can standardise celltypes with legacy function", {
          cells <- c("b cells", "bcell", "b_cell", "Naive B Cell",
                     "cd4", "T cellcD4", "cd8", "cytotoxic T lymphocyte",
                     "neutro", "mono", "cdc")

          standardised <- c("B_cells","B_cells","B_cells", "B_cells",
                            "T_cells_CD4", "T_cells_CD4", "T_cells_CD8", "T_cells_CD8",
                            "neutrophils", "monocytes", "conventional_dendritic_cells")
          expect_equal(fix_cell_names_legacy(cells), standardised) })


test_that("can process wide deconvolution results", {
  testdata <- readRDS("testdata/deconvolution_immunedeconv.Rdata")
  expected <- readRDS("testdata/deconvolution_immunedeconv_long.Rdata")
  expect_equal(process_immunedeconv(testdata), expected)
})



test_that("New celltype renaming works", {
  cells_quantiseq <- c( "T.CD8.Memory", "Neutrophils.LD", "B.cells.memory", "MAIT" )
  actual_quantiseq <- c("T_cells_CD8_memory", "neutrophils", "B_cells_memory", "MAIT" )
  cells_abis <- c("T.gd.Vd2", "Neutrophils.LD", "T.gd.non.Vd2", "Basophils.LD", "Monocytes.NC.I")
  actual_abis <- c("T_cells_gd", "neutrophils", "T_cells_gd", "basophils", "monocytes_non_conventional")
  expect_equal(fix_cell_names(cells_quantiseq, "quantiseq"), actual_quantiseq)
  expect_equal(fix_cell_names(cells_abis, "abis"), actual_abis)
  expect_error(fix_cell_names(cell_quantiseq, "wrong method"), "Method must be")
  })





