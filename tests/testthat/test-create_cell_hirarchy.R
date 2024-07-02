# probably not needed while tree is still growing
# test_that("Creation of data.tree works", {
#   test_mixture <- readRDS("testdata/deconvolution_immunedeconv_long.Rdata")
#   test_mixture$cell_type <- fix_cell_names(test_mixture$cell_type, "abis")
#   reference <- readRDS("testdata/deconvolution_tree.Rdata")
#
#   actual <- make_cell_tree(test_mixture)
#   reference_traverse <- data.tree::Traverse(reference, "post-order")
#   actual_traverse <- data.tree::Traverse(actual, "post-order")
#
#   expect_equal(data.tree::Get(actual_traverse, "level"), data.tree::Get(reference_traverse, "level"))
#   #expect_equal(data.tree::Get(actual_traverse, "attributes"), data.tree::Get(reference_traverse, "attributes"))
# })



test_that("cumulative sums work", {
  test_tree <- readRDS("testdata/deconvolution_tree.Rdata")
  expected_proportion_full <- readRDS("testdata/deconvolution_cumulative_proportions_full.Rdata")
  actual_proportion_full <- get_cumulative_proportions(test_tree, TRUE)
  expect_equal(actual_proportion_full, expected_proportion_full)
})
