test_that("can simulated sc-set", {
  n_genes <- 1000
  n_cells_est <- 300
  simulation <- simulate_sc(n_genes = n_genes, n_cells_est = n_cells_est, make_signature = FALSE)
  expect_equal(length(simulation), 3)
  expect_equal(dim(simulation$counts), c(n_genes, n_cells_est))
  expect_equal(dim(simulation$cpms), c(n_genes, n_cells_est))
  expect_equal(dim(simulation$annotation), c(n_cells_est, 2))
  n_genes <- 200
  n_cells_est <- 600
  simulation <- simulate_sc(n_genes = n_genes, n_cells_est = n_cells_est, make_signature = FALSE)
  expect_equal(length(simulation), 3)
  expect_equal(dim(simulation$counts), c(n_genes, n_cells_est))
  expect_equal(dim(simulation$annotation), c(n_cells_est, 2))
  expect_error(simulate_sc(n_genes = n_genes, n_cells_est = n_cells_est, make_signature = FALSE, lambdas = c(1,2,3)))
  n_genes <- 100
  n_cells_est <- 60
  simulation <- simulate_sc(n_genes = n_genes, n_cells_est = n_cells_est, make_signature = FALSE, lambdas = c(1,2,3,4,5,6))
  expect_equal(length(simulation), 3)
  expect_equal(dim(simulation$counts), c(n_genes, n_cells_est))
  expect_equal(dim(simulation$annotation), c(n_cells_est, 2))

  })
