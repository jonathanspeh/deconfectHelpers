test_that("abis deconvolution works", {
  mixture <- readRDS("testdata/mixture_quantiseq.Rdata")
  expected <- readRDS("testdata/abis_deconv.Rdata")
  actual <- run_abis(mixture = mixture)
  expect_equal(actual, expected)
})



# mixture <- readRDS("tests/testthat/testdata/mixture_quantiseq.Rdata")
# signature <- readRDS()

#run_abis(mixture)

# mixture_quantiseq <- data.table::fread("~/Desktop/doktorarbeit/simulate_mixtures/data/raw/quantiseq_reference/GSE107572_tpm.txt.gz")
# genes <- mixture_quantiseq$GENE
# mixture_quantiseq <- as.matrix(mixture_quantiseq[,-1])
# rownames(mixture_quantiseq) <- genes
# genes <- intersect(rownames(mixture_quantiseq), rownames(abis_signature))
#
#
#
#
#
# abis_deconvolution <- (apply(mixture_quantiseq[genes, , drop=F], 2,
#                              function(x) coef(MASS::rlm(as.matrix(abis_signature[genes,]), x, maxit =100 ))))
#
# saveRDS(abis_deconvolution, "tests/testthat/testdata/abis_deconv.Rdata")
# saveRDS(mixture_quantiseq, "tests/testthat/testdata/mixture_quantiseq.Rdata")
